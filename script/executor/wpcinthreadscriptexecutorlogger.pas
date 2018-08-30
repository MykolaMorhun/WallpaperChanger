unit WpcInThreadScriptExecutorLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcScriptExecutor,
  WpcInThreadScriptTracer,
  WpcWallpaperSetter,
  WpcWallpaperStyles,
  WpcScriptCommons,
  WpcBaseStatement,
  WpcStatementProperties,
  WpcTimeMeasurementUnits,
  WpcScript,
  WpcImage,
  WpcLogger,
  WpcExceptions;

type

  {
    Verbose level of tracer.
    Each next level includes all previous.
  }
  TWpcScriptTraceLevel = (
    STL_NONE,          // log nothing
    STL_START_STOP,    // log start and stop
    STL_SET,           // log set wallpaper only
    STL_SET_AND_DELAY, // log set wallpaper and delays
    STL_BRANCH,        // log step in/out branch plus set wallapaer and delays
    STL_STATEMENT,     // log each statement id
    STL_ALL            // log each statement details // TODO implement
  );

  { TWpcInThreadScriptExecutorLogger }

  // TODO do not extend TWpcInThreadScriptTracer.
  // Implement IWpcScriptExecutor instead and use TWpcInThreadScriptTracer for events notification.
  TWpcInThreadScriptExecutorLogger = class(TWpcInThreadScriptTracer)
  protected
    FTraceLevel : TWpcScriptTraceLevel;
    FLogger : IWpcLogger;

    // If true date and time will be added to each log message
    FLogDateTime : Boolean;
    // If true prints logs with indent according to branch level,
    // e.g. First call of Main - no indent, branch called from Main - one block indent and so on.
    FIndent : Boolean;
    // Number of spaces in each indent block.
    FIndentSize : Integer;
    // Character with which indent is filled.
    FIndentSymbol : Char;

    // AddIndent function static local variable.
    SIndentString : String;
  public
    // Logger should be released separately from this instance.
    constructor Create(WallpaperSetter : IWallpaperSetter; Logger : IWpcLogger);
    destructor Destroy(); override;
  private
    procedure SetIndentSize(NewIndentSize : Integer);
    procedure SetIndentSymbol(NewIndentSymbol : Char);
  public
    property TraceLevel: TWpcScriptTraceLevel read FTraceLevel write FTraceLevel;
    property LogDateTime : Boolean read FLogDateTime write FLogDateTime;
    property Indent : Boolean read FIndent write FIndent;
    property IndentSize : Integer read FIndentSize write SetIndentSize;
    property IndentSymbol : Char read FIndentSymbol write SetIndentSymbol;
  protected
    procedure OnScriptStartCallback(var Script : TWpcScript);
    procedure OnScriptStopCallback(ExitStatus : TWpcScriptExecutionExitStatus);
    procedure OnBranchEnterCallback(var BranchName : String);
    procedure OnBranchExitCallback(var BranchName : String);
    procedure OnStatementExecutionCallback(var Statement : IWpcBaseScriptStatement);
    procedure OnWaitCallback(var Milliseconds : LongWord);
    procedure OnSetWallpaperCallback(var Image : TWpcImage; var Style : TWpcWallpaperStyle);
  protected

  protected
    procedure LogMessage(Message : String; NoIndent : Boolean = false);
    function AddIndent(Message : String) : String; inline;

    function ShouldPrint(MinTraceLevel : TWpcScriptTraceLevel) : Boolean; inline;
  end;


implementation

{ TWpcScriptTraceLogger }

constructor TWpcInThreadScriptExecutorLogger.Create(WallpaperSetter : IWallpaperSetter; Logger: IWpcLogger);
begin
  if (Logger = nil) then
    raise TWpcUseErrorException.Create('Logger should be specified.');

  FLogger := Logger;
  FTraceLevel:= STL_BRANCH;
  FLogDateTime := True;
  FIndent := True;
  FIndentSymbol := ' ';
  FIndentSize := 2;

  OnScriptStart := @OnScriptStartCallback;
  OnScriptStop := @OnScriptStopCallback;
  OnBranchEnter := @OnBranchEnterCallback;
  OnBranchExit := @OnBranchExitCallback;
  OnStatementExecution := @OnStatementExecutionCallback;
  OnWait := @OnWaitCallback;
  OnSetWallpaper := @OnSetWallpaperCallback;

  inherited Create(WallpaperSetter);
end;

destructor TWpcInThreadScriptExecutorLogger.Destroy();
begin
  inherited Destroy();
end;

procedure TWpcInThreadScriptExecutorLogger.SetIndentSize(NewIndentSize : Integer);
begin
  if (NewIndentSize < 0) then
    raise TWpcUseErrorException.Create('Indent size should not be negative.');

  FIndentSize := NewIndentSize;
end;

procedure TWpcInThreadScriptExecutorLogger.SetIndentSymbol(NewIndentSymbol : Char);
var
  i : Integer;
begin
  if (FIndentSymbol = NewIndentSymbol) then
    exit;

  FIndentSymbol := NewIndentSymbol;
  for i:=1 to Length(SIndentString) do
    SIndentString[i] := FIndentSymbol;
end;

procedure TWpcInThreadScriptExecutorLogger.OnScriptStartCallback(var Script : TWpcScript);
begin
  if (ShouldPrint(STL_START_STOP)) then
    LogMessage('Starting script execution', true);
end;

procedure TWpcInThreadScriptExecutorLogger.OnScriptStopCallback(ExitStatus : TWpcScriptExecutionExitStatus);
begin
  if (ShouldPrint(STL_START_STOP)) then begin
    LogMessage('Script execution finished', true);
    case (ExitStatus) of
      SES_FINISHED: LogMessage('End of script execution reached.', true);
      SES_TERMINATED: LogMessage('Execution terminated.', true);
      SES_ERROR_STACK_OVERFLOW: LogMessage('Error: Stack overflow', true);
    end;
  end;
end;

procedure TWpcInThreadScriptExecutorLogger.OnBranchEnterCallback(var BranchName : String);
begin
  if (ShouldPrint(STL_BRANCH)) then
    LogMessage(BranchName + ' - entering branch');
end;

procedure TWpcInThreadScriptExecutorLogger.OnBranchExitCallback(var BranchName: String);
begin
  if (ShouldPrint(STL_BRANCH)) then
    LogMessage(BranchName  + ' - exiting branch');
end;

procedure TWpcInThreadScriptExecutorLogger.OnStatementExecutionCallback(var Statement : IWpcBaseScriptStatement);
begin
  if (ShouldPrint(STL_STATEMENT)) then
    if (FTraceLevel = STL_ALL) then
      LogMessage(StatementIdToStr(Statement.GetId())) // TODO print all fields
    else
      LogMessage(StatementIdToStr(Statement.GetId()));
end;

procedure TWpcInThreadScriptExecutorLogger.OnWaitCallback(var Milliseconds : LongWord);
var
  Delay           : LongWord;
  MeasurementUnit : TWpcTimeMeasurementUnits;
begin
  if (ShouldPrint(STL_SET_AND_DELAY)) then begin
    TWpcDelayStatementProperty.ConvertToReadableUnits(Milliseconds, Delay, MeasurementUnit);
    LogMessage('Idling: ' + IntToStr(Delay) + ' ' + TimeMeasurementUnitToStr(MeasurementUnit));
  end;
end;

procedure TWpcInThreadScriptExecutorLogger.OnSetWallpaperCallback(var Image : TWpcImage; var Style : TWpcWallpaperStyle);
begin
  if (ShouldPrint(STL_SET)) then
    LogMessage('Setting wallpaper: ' + Image.GetPath() + ' ' + WallpaperStyleToStr(Style));
end;

procedure TWpcInThreadScriptExecutorLogger.LogMessage(Message : String; NoIndent : Boolean);
begin
  if (FIndent and not NoIndent) then
    Message := AddIndent(Message);

  if (FLogDateTime) then
    Message := FormatDateTime('DD-MM-YYYY hh:mm:ss', Now()) + ' |' + Message;

  FLogger.LogMessage(Message);
end;

function TWpcInThreadScriptExecutorLogger.AddIndent(Message : String) : String;
var
  IndentLen : Integer;
  i         : Integer;
  LenDiff   : Integer;
begin
  IndentLen := FStack.Size() * FIndentSize;
  if (Length(SIndentString) <> IndentLen) then begin
    LenDiff := IndentLen - Length(SIndentString);
    SetLength(SIndentString, IndentLen);
    if (LenDiff > 0) then
      for i:=(IndentLen - LenDiff + 1) to IndentLen do
        SIndentString[i] := FIndentSymbol;
  end;
  Result := SIndentString + Message;
end;

function TWpcInThreadScriptExecutorLogger.ShouldPrint(MinTraceLevel : TWpcScriptTraceLevel) : Boolean;
begin
  Result := Ord(MinTraceLevel) <= Ord(FTraceLevel);
end;


end.

