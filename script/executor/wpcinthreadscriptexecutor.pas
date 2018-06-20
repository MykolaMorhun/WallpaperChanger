unit WpcInThreadScriptExecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FpTimer,
  GStack,
  WpcScriptExecutor,
  WpcScriptCommons,
  WpcBaseStatement,
  WpcBranchStatement,
  WpcBranchActionsStatements,
  WpcWallpaperStatement,
  WpcDirectoryStatement,
  WpcChooserStatements,
  WpcWaitStatement,
  WpcStopStatement,
  WpcScript,
  WpcWallpaperSetter,
  WpcExceptions;

type

  { TWpcInThreadScriptExecutor }

  {
    Eexecutes Wallpaper Changer Script.
    This class is designed to be invoked from main thread, so
    it applies needed action and release control fast.
  }
  TWpcInThreadScriptExecutor = class(IWpcScriptExecutor)
  private type
    TWpcInThreadScriptExecutorStackEntryInfo = record
      // Is used for iterations within statement
      Counter : Integer;
    end;

    TWpcInThreadScriptExecutorStackEntry = record
      // Name of branch in which execution pointer is set
      Branch : String;
      // Number of statement on which execution pointer is set. Starts form zero.
      Statement : Integer;
      // Additional info about current statement
      FrameInfo : TWpcInThreadScriptExecutorStackEntryInfo;
    end;

    TWpcInThreadScriptExecutorStack = specialize TStack<TWpcInThreadScriptExecutorStackEntry>;

  private
    FWallpaperSetter : IWallpaperSetter;
    // Stores current script
    FScript : TWpcScript;
    // Script stack
    FStack : TWpcInThreadScriptExecutorStack;
    FIsRunning : Boolean;
    // Used to invoke callbacks after waits
    FTimer : TFPTimer;

    FOnStopCallback : TWpcScriptExecutorStopCallback;
  public
    constructor Create(WallpaperSetter : IWallpaperSetter);
    destructor Destroy(); override;
  public
    procedure RunScript(Script : TWpcScript); override;
    procedure Terminate(); override;
    procedure SkipCurrentDelay(); override;
    function IsRunning() : Boolean; override;
    procedure SetOnStopCallback(Callback : TWpcScriptExecutorStopCallback); override;
  private
    procedure ExecuteBranch(BranchName : String);
    procedure ExecuteStatement(Statement : IWpcBaseScriptStatement); //inline;

    procedure ExecuteWaitStatement(Statement : TWpcWaitStatement); //inline;
    procedure ExecuteWallpaperStatement(Statement : TWpcWallpaperStatement); //inline;
    procedure ExecuteDirectoryStatement(Statement : TWpcDirectoryStatement); //inline;
    procedure ExecuteStopStatement(Statement : TWpcStopStatement); //inline;
    procedure ExecuteSwitchBranchStatement(Statement : TWpcSwitchBranchStatement); //inline;
    procedure ExecuteUseBtranchStatement(Statement : TWpcUseBranchStatement); //inline;
    procedure ExecuteWallpaperChooserStatement(Statement : TWpcWallpaperChooserStatement); //inline;
    procedure ExecuteBranchToUseChooserStatement(Statement : TWpcUseBranchChooserStatement); //inline;
    procedure ExecuteBranchToSwitchChooserStatement(Statement : TWpcSwitchBranchChooserStatement); //inline;

    function IsTriggered(Probability : Byte) : Boolean;

    procedure ContunueExecution();
    function Next() : Boolean;
    procedure IncStatementPointer(); //inline;
    procedure DecStatementTimesCounter(); //inline;
    procedure SetStatementTimesCounter(Value : Integer); //inline;
    function GetCurrentStatement() : IWpcBaseScriptStatement; //inline;
    function GetTimes(Statement : IWpcBaseScriptStatement) : LongWord;
  private
    procedure TimerSleep(Milliseconds : LongWord);
    procedure TimerCallback(Sender: TObject);
  end;


implementation

{ TWpcInThreadScriptExecutor }

constructor TWpcInThreadScriptExecutor.Create(WallpaperSetter : IWallpaperSetter);
begin
  FWallpaperSetter := WallpaperSetter;

  FIsRunning := False;
  FTimer := TFPTimer.Create(nil);
  FTimer.OnTimer := @TimerCallback;
  FOnStopCallback := nil;
end;

destructor TWpcInThreadScriptExecutor.Destroy();
begin
  FTimer.Enabled := False;
  FTimer.Free();

  if (FStack <> nil) then FStack.Free();
end;

{
  Executes given script. This method is not responsible for the script lifecycle.
}
procedure TWpcInThreadScriptExecutor.RunScript(Script : TWpcScript);
begin
  if (FIsRunning) then
    raise TWpcUseErrorException.Create('Another script is already running');

  FIsRunning := True;
  FScript := Script;

  if (FStack <> nil) then FStack.Free();
  FStack := TWpcInThreadScriptExecutorStack.Create();

  // Execute asynchronously main branch
  ExecuteBranch(MAIN_BARNCH);
end;

{
  Terminates current script execution.
}
procedure TWpcInThreadScriptExecutor.Terminate();
begin
  if (not FIsRunning) then
    raise TWpcUseErrorException.Create('Script is not running');

  FIsRunning := False;
  FTimer.Enabled := False;

  if (FOnStopCallback <> nil) then
    FOnStopCallback();
end;

{
  Interrupts current delay and executes next statement.
}
procedure TWpcInThreadScriptExecutor.SkipCurrentDelay();
begin
  FTimer.Enabled := False;
  ContunueExecution();
end;

function TWpcInThreadScriptExecutor.IsRunning() : Boolean;
begin
  Result := FIsRunning;
end;

procedure TWpcInThreadScriptExecutor.SetOnStopCallback(Callback : TWpcScriptExecutorStopCallback);
begin
  FOnStopCallback := Callback;
end;

procedure TWpcInThreadScriptExecutor.ExecuteBranch(BranchName : String);
var
  StackFrame : TWpcInThreadScriptExecutorStackEntry;
begin
  StackFrame.Branch := BranchName;
  StackFrame.Statement := -1; // point before first statement
  StackFrame.FrameInfo.Counter := 0;

  FStack.Push(StackFrame);

  ContunueExecution();
end;

{
  Applies given script statement.
  This method doesn't count any repeated actions inside given statement, i.e. times property is ignored.
}
procedure TWpcInThreadScriptExecutor.ExecuteStatement(Statement : IWpcBaseScriptStatement);
begin
  // Type casting is safe here because of IDs.
  case (Statement.GetId()) of
    WPC_WAIT_STATEMENT_ID:
      ExecuteWaitStatement(TWpcWaitStatement(Statement));
    WPC_WALLPAPER_STATEMENT_ID:
      ExecuteWallpaperStatement(TWpcWallpaperStatement(Statement));
    WPC_DIRECTORY_STATEMENT_ID:
      ExecuteDirectoryStatement(TWpcDirectoryStatement(Statement));
    WPC_STOP_STATEMENT_ID:
      ExecuteStopStatement(TWpcStopStatement(Statement));
    WPC_SWITCH_BRANCH_STATEMENT_ID:
      ExecuteSwitchBranchStatement(TWpcSwitchBranchStatement(Statement));
    WPC_USE_BRANCH_STATEMENT_ID:
      ExecuteUseBtranchStatement(TWpcUseBranchStatement(Statement));
    WPC_WALLPAPER_CHOOSER_STATEMENT_ID:
      ExecuteWallpaperChooserStatement(TWpcWallpaperChooserStatement(Statement));
    WPC_BRANCH_TO_USE_CHOOSER_STATEMENT_ID:
      ExecuteBranchToUseChooserStatement(TWpcUseBranchChooserStatement(Statement));
    WPC_BRANCH_TO_SWITCH_CHOOSER_STATEMENT_ID:
      ExecutebranchToSwitchChooserStatement(TWpcSwitchBranchChooserStatement(Statement));
  else
    raise TWpcUseErrorException.Create('Script Executor: unknown statement: ' + StatementIdToStr(Statement.GetId()));
  end;
end;

procedure TWpcInThreadScriptExecutor.ExecuteWaitStatement(Statement : TWpcWaitStatement);
begin
  if (IsTriggered(Statement.GetProbability())) then
     TimerSleep(Statement.GetDelay())
  else
    // This delay should be skipped
    ContunueExecution();
end;

procedure TWpcInThreadScriptExecutor.ExecuteWallpaperStatement(Statement : TWpcWallpaperStatement);
begin
  if (IsTriggered(Statement.GetProbability())) then begin
    FWallpaperSetter.SetDesktopWallpaper(Statement.GetImage().GetPath(), Statement.GetStyle());
    if (Statement.GetDelay() <> 0) then
      TimerSleep(Statement.GetDelay())
    else
      ContunueExecution();
  end
  else
    ContunueExecution();
end;

procedure TWpcInThreadScriptExecutor.ExecuteDirectoryStatement(Statement : TWpcDirectoryStatement);
begin
  if (IsTriggered(Statement.GetProbability())) then begin
    FWallpaperSetter.SetDesktopWallpaper(Statement.GetNextImage().GetPath(), Statement.GetStyle());
    if (Statement.GetDelay() <> 0) then
      TimerSleep(Statement.GetDelay())
    else
      // continue execution but release executor stack
      TimerSleep(1);
  end
  else
    ContunueExecution();
end;

procedure TWpcInThreadScriptExecutor.ExecuteStopStatement(Statement : TWpcStopStatement);
begin
  if (IsTriggered(Statement.GetProbability())) then
    Terminate()
  else
    ContunueExecution();
end;

procedure TWpcInThreadScriptExecutor.ExecuteSwitchBranchStatement(Statement : TWpcSwitchBranchStatement);
begin
   if (IsTriggered(Statement.GetProbability())) then begin
     // Replace current branch in stack with given one.
     FStack.Pop();
     ExecuteBranch(Statement.GetBranchName());
   end
   else
     ContunueExecution();
end;

procedure TWpcInThreadScriptExecutor.ExecuteUseBtranchStatement(Statement : TWpcUseBranchStatement);
begin
  if (IsTriggered(Statement.GetProbability())) then begin
     ExecuteBranch(Statement.GetBranchName());
   end
   else
     ContunueExecution();
end;

procedure TWpcInThreadScriptExecutor.ExecuteWallpaperChooserStatement(Statement : TWpcWallpaperChooserStatement);
begin
  ExecuteWallpaperStatement(Statement.ChooseItem());
end;

procedure TWpcInThreadScriptExecutor.ExecuteBranchToUseChooserStatement(Statement : TWpcUseBranchChooserStatement);
begin
  ExecuteUseBtranchStatement(Statement.ChooseItem());
end;

procedure TWpcInThreadScriptExecutor.ExecuteBranchToSwitchChooserStatement(Statement : TWpcSwitchBranchChooserStatement);
begin
  ExecuteSwitchBranchStatement(Statement.ChooseItem());
end;

function TWpcInThreadScriptExecutor.IsTriggered(Probability : Byte) : Boolean;
begin
  if (Probability = 100) then Result := true
  else if (Probability = 0) then Result := false
  else Result := (Random(101) < Probability);
end;

{
  Continues execution of current script.
  Depending on the current state it could continue execution of current statement,
  start execution of the next one or manipulate with stack entities (add/remove) and
  proceed with a statement of a previous/next branch.

  End of this method shouldn't be reached without reinvocation sheduled.
}
procedure TWpcInThreadScriptExecutor.ContunueExecution();
begin
  while (FStack.Top().FrameInfo.Counter = 0) do begin
    // Move to the next statement
    if (not Next()) then begin
      FStack.Pop(); // return from this branch
      if (FStack.IsEmpty()) then begin
        // We just returned from the top (main) branch. End of execution is reached.
        Terminate();
        exit;
      end;
      // Here this method should be invoked recursively (with brach on top of the stack we just returned to).
      // But instead of recursion while loop if used.
    end;
  end;

  DecStatementTimesCounter();
  ExecuteStatement(GetCurrentStatement());
end;

{
  Shifts stack pointer one position further.
  Returns true if shift was successful or false if end of the branch is reached.
}
function TWpcInThreadScriptExecutor.Next() : Boolean;
begin
  IncStatementPointer();
  if (FStack.Top().Statement = FScript.GetBranch(FStack.Top().Branch).CountStatements()) then begin
    // End of current branch is reached
    Result := False;
  end
  else begin
    SetStatementTimesCounter(GetTimes(GetCurrentStatement()));
    Result := True;
  end;
end;

{
  Equivalent of Inc(FStack.Top().Statement)
}
procedure TWpcInThreadScriptExecutor.IncStatementPointer();
var
  StackFrame : TWpcInThreadScriptExecutorStackEntry;
begin
  StackFrame := FStack.Top();
  Inc(StackFrame.Statement);
  FStack.Pop();
  FStack.Push(StackFrame);
end;

{
  Equivalent of Dec(FStack.Top().FrameInfo.Counter)
}
procedure TWpcInThreadScriptExecutor.DecStatementTimesCounter();
var
  StackFrame : TWpcInThreadScriptExecutorStackEntry;
begin
  StackFrame := FStack.Top();
  Dec(StackFrame.FrameInfo.Counter);
  FStack.Pop();
  FStack.Push(StackFrame);
end;

{
  Equivalent of FStack.Top().FrameInfo.Counter :=
}
procedure TWpcInThreadScriptExecutor.SetStatementTimesCounter(Value : Integer);
var
  StackFrame : TWpcInThreadScriptExecutorStackEntry;
begin
  StackFrame := FStack.Top();
  StackFrame.FrameInfo.Counter := Value;
  FStack.Pop();
  FStack.Push(StackFrame);
end;

function TWpcInThreadScriptExecutor.GetCurrentStatement() : IWpcBaseScriptStatement;
begin
  Result := FScript.GetBranch(FStack.Top().Branch)
                   .GetStatement(FStack.Top().Statement);
end;

{
  Returns how many times should be repeated given statement.
}
function TWpcInThreadScriptExecutor.GetTimes(Statement : IWpcBaseScriptStatement) : LongWord;
begin
  case (Statement.GetId()) of
    WPC_WAIT_STATEMENT_ID:
      Result := TWpcWaitStatement(Statement).GetTimes();
    WPC_USE_BRANCH_STATEMENT_ID:
      Result := TWpcUseBranchStatement(Statement).GetTimes();
    WPC_DIRECTORY_STATEMENT_ID:
      if (TWpcDirectoryStatement(Statement).CountImages() = 0) then
        // Skip this statement because specified directory doesn't contain any image
        Result := 0
      else
        Result := TWpcDirectoryStatement(Statement).GetTimes();
  else
    Result := 1;
  end;
end;

{
  Schedule timer to invoke continuation of the script execution and returns control.
}
procedure TWpcInThreadScriptExecutor.TimerSleep(Milliseconds : LongWord);
begin
  FTimer.Interval := Milliseconds;
  FTimer.Enabled := True;
end;

procedure TWpcInThreadScriptExecutor.TimerCallback(Sender: TObject);
begin
  FTimer.Enabled := False;
  ContunueExecution();
end;


end.
