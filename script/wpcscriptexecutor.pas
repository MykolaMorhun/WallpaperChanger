unit WpcScriptExecutor;

{$mode objfpc}{$H+}
{$GOTO ON}

interface

uses
  Classes, SysUtils,
  WpcScriptCommons,
  WpcStatements,
  WpcScript,
  WpcExceptions,
  WallpaperSetter;

type

  { TWpcScriptExecutor }

  TWpcScriptExecutor = class(TObject)
  private
    FScript                  : TWpcScript;
    FWallpaperSetter         : IWallpaperSetter;
    FIsStopStatementReached  : Boolean;

    FIsSwitchBranchTriggered : Boolean;
    FSwithToBranchName       : String;
  public
    constructor Create(Script : TWpcScript; WpSetter : IWallpaperSetter);
    destructor Destroy(); override;
  public
    procedure ExecuteScript();

  private
    procedure ExecuteBranch(BranchName : String);
    procedure ExecuteStatement(Statement : IWpcBaseScriptStatement);

    procedure ExecuteWaitStatement(Statement : TWpcWaitStatement);
    procedure ExecuteWallpaperStatement(Statement : TWpcWallpaperStatement);
    procedure ExecuteStopStatement(Statement : TWpcStopStatement);
    procedure ExecuteSwitchBranchStatement(Statement : TWpcSwitchBranchStatement);
    procedure ExecuteUseBtranchStatement(Statement : TWpcUseBranchStatement);
    procedure ExecuteWallpaperChooserStatement(Statement : TWpcWallpaperChooserStatement);
    procedure ExecuteBranchToUseChooserStatement(Statement : TWpcUseBranchChooserStatement);
    procedure ExecuteBranchToSwitchChooserStatement(Statement : TWpcSwitchBranchChooserStatement);

    function IsTriggered(Probability : Byte) : Boolean;
  end;

implementation

{ TWpcScriptExecutor }

constructor TWpcScriptExecutor.Create(Script: TWpcScript; WpSetter : IWallpaperSetter);
begin
  FScript := Script;
  FWallpaperSetter := WpSetter;
  FIsStopStatementReached := False;
end;

destructor TWpcScriptExecutor.Destroy;
begin
  FScript.Free();
  inherited Destroy();
end;

{
  Executes Wallpaper Changer script.
  Entry point is branch with 'Main' name.
  When executor reach end of branch then Main branch will be executed again.
  Execution will be stopped only if it reach Stop statement.
}
procedure TWpcScriptExecutor.ExecuteScript();
begin
  repeat
    ExecuteBranch(MAIN_BARNCH);
  until (FIsStopStatementReached);
end;

{ Helper methods }

procedure TWpcScriptExecutor.ExecuteBranch(BranchName: String);
label
  StartBranchLabel;
var
  CurrentBranch          : TWpcBranchStatement;
  CurrentStatementNumber : Integer;
begin
  CurrentBranch := FScript.GetBranch(BranchName);
  StartBranchLabel:
  for CurrentStatementNumber:=0 to (CurrentBranch.CountStatements() - 1) do begin
    if (FIsStopStatementReached) then
      break;
    if (FIsSwitchBranchTriggered) then begin
      FIsSwitchBranchTriggered := False;
      CurrentBranch := FScript.GetBranch(FSwithToBranchName);
      goto StartBranchLabel;
    end;
    ExecuteStatement(CurrentBranch.GetStatement(CurrentStatementNumber));
  end;
end;

{
  Applies given script statement.
}
procedure TWpcScriptExecutor.ExecuteStatement(Statement: IWpcBaseScriptStatement);
begin
  // Type casting is safe here because of IDs.
  case (Statement.GetId()) of
    WPC_WAIT_STATEMENT_ID:
      ExecuteWaitStatement(TWpcWaitStatement(Statement));
    WPC_WALLPAPER_STATEMENT_ID:
      ExecuteWallpaperStatement(TWpcWallpaperStatement(Statement));
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

procedure TWpcScriptExecutor.ExecuteWaitStatement(Statement: TWpcWaitStatement);
var
  i : Integer;
begin
  for i:=1 to Statement.GetTimes() do
    if (IsTriggered(Statement.GetProbability())) then
      Sleep(Statement.GetDelay());
end;

procedure TWpcScriptExecutor.ExecuteWallpaperStatement(Statement: TWpcWallpaperStatement);
begin
  if (IsTriggered(Statement.GetProbability())) then begin
    FWallpaperSetter.SetDesktopWallpaper(Statement.GetImage().GetPath(), Statement.GetStyle());
    if (Statement.GetDelay() <> 0) then
      Sleep(Statement.GetDelay());
  end;
end;

procedure TWpcScriptExecutor.ExecuteStopStatement(Statement: TWpcStopStatement);
begin
  if (IsTriggered(Statement.GetProbability())) then
    FIsStopStatementReached := True;
end;

{
  Replaces current branch execution with new branch.
}
procedure TWpcScriptExecutor.ExecuteSwitchBranchStatement(Statement: TWpcSwitchBranchStatement);
begin
  if (IsTriggered(Statement.GetProbability())) then begin
    FIsSwitchBranchTriggered := True;
    FSwithToBranchName := Statement.GetBranchName();
  end;
end;

{
  Executes subbranch and returns to the next statement of current branch.
}
procedure TWpcScriptExecutor.ExecuteUseBtranchStatement(Statement: TWpcUseBranchStatement);
var
  i : Integer;
begin
  for i:=1 to Statement.GetTimes() do
    if (IsTriggered(Statement.GetProbability())) then
      ExecuteBranch(Statement.GetBranchName());
end;

procedure TWpcScriptExecutor.ExecuteWallpaperChooserStatement(Statement: TWpcWallpaperChooserStatement);
begin
  ExecuteWallpaperStatement(Statement.ChooseItem());
end;

procedure TWpcScriptExecutor.ExecuteBranchToUseChooserStatement(Statement: TWpcUseBranchChooserStatement);
begin
  ExecuteUseBtranchStatement(Statement.ChooseItem());
end;

procedure TWpcScriptExecutor.ExecuteBranchToSwitchChooserStatement(Statement: TWpcSwitchBranchChooserStatement);
begin
  ExecuteSwitchBranchStatement(Statement.ChooseItem());
end;

function TWpcScriptExecutor.IsTriggered(Probability: Byte): Boolean;
begin
  if (Probability = 100) then Result := true
  else if (Probability = 0) then Result := false
  else Result := (Random(101) < Probability);
end;


end.

