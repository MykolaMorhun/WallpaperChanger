unit WpcInThreadScriptTracer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcScriptExecutor,
  WpcInThreadScriptExecutor,
  WpcScript,
  WpcBaseStatement,
  WpcBranchActionsStatements,
  WpcImage,
  WpcWallpaperStyles;

type

  TWpcScriptRunTraceEvent = procedure(var Script : TWpcScript) of Object;
  TWpcScriptTerminateTraceEvent = procedure(ExitStatus : TWpcScriptExecutionExitStatus) of Object;
  TWpcScriptBranchTraceEvent = procedure(var BranchName : String) of Object;
  TWpcScriptStatementTraceEvent = procedure(var Statement : IWpcBaseScriptStatement) of Object;
  TWpcScriptWaitTraceEvent = procedure(var Milliseconds : LongWord) of Object;
  TWpcScriptSetWallpaperTraceEvent = procedure(var Image : TWpcImage; var Style : TWallpaperStyle) of Object;

  { TWpcInThreadScriptTracer }

  TWpcInThreadScriptTracer = class(TWpcInThreadScriptExecutor)
  protected
    FOnScriptStart : TWpcScriptRunTraceEvent;
    FOnScriptStop  : TWpcScriptTerminateTraceEvent;
    FOnBranchEnter : TWpcScriptBranchTraceEvent;
    FOnBranchExit  : TWpcScriptBranchTraceEvent;
    FOnStatementExecution : TWpcScriptStatementTraceEvent;
    FOnWait : TWpcScriptWaitTraceEvent;
    FOnSetWallpaper : TWpcScriptSetWallpaperTraceEvent;
  public
    property OnScriptStart : TWpcScriptRunTraceEvent read FOnScriptStart write FOnScriptStart;
    property OnScriptStop : TWpcScriptTerminateTraceEvent read FOnScriptStop write FOnScriptStop;
    property OnBranchEnter : TWpcScriptBranchTraceEvent read FOnBranchEnter write FOnBranchEnter;
    property OnBranchExit : TWpcScriptBranchTraceEvent read FOnBranchExit write FOnBranchExit;
    property OnStatementExecution : TWpcScriptStatementTraceEvent read FOnStatementExecution write FOnStatementExecution;
    property OnWait : TWpcScriptWaitTraceEvent read FOnWait write FOnWait;
    property OnSetWallpaper : TWpcScriptSetWallpaperTraceEvent read FOnSetWallpaper write FOnSetWallpaper;
  public
    procedure RunScript(Script : TWpcScript); override;
    procedure Terminate(ExitStatus : TWpcScriptExecutionExitStatus); override;
  protected
    procedure ExecuteBranch(BranchName : String); override;
    procedure ExecuteSwitchBranchStatement(Statement : TWpcSwitchBranchStatement); override;
    procedure ExitCurrentBranch(); override;
    procedure ExecuteStatement(Statement : IWpcBaseScriptStatement); override;
    procedure SetWallpaper(Image : TWpcImage; Style : TWallpaperStyle); override;
    procedure TimerSleep(Milliseconds : LongWord); override;
  end;


implementation


procedure TWpcInThreadScriptTracer.RunScript(Script : TWpcScript);
begin
  if (Assigned(FOnScriptStart)) then
    FOnScriptStart(Script);

  inherited RunScript(Script);
end;

procedure TWpcInThreadScriptTracer.Terminate(ExitStatus : TWpcScriptExecutionExitStatus);
begin
  if (Assigned(FOnScriptStop)) then
    FOnScriptStop(ExitStatus);

  inherited Terminate(ExitStatus);
end;

procedure TWpcInThreadScriptTracer.ExecuteBranch(BranchName : String);
begin
   if (Assigned(FOnBranchEnter)) then
     FOnBranchEnter(BranchName);

  inherited ExecuteBranch(BranchName);
end;

procedure TWpcInThreadScriptTracer.ExecuteSwitchBranchStatement(Statement : TWpcSwitchBranchStatement);
var
  BranchName : String;
begin
  if (Assigned(FOnBranchExit)) then begin
     BranchName := Statement.GetBranchName();
     FOnBranchExit(BranchName);
  end;

  inherited ExecuteSwitchBranchStatement(Statement);
end;

procedure TWpcInThreadScriptTracer.ExitCurrentBranch();
begin
  if (Assigned(FOnBranchExit)) then
     FOnBranchExit(FStack.Top().Branch);

  inherited ExitCurrentBranch();
end;

procedure TWpcInThreadScriptTracer.ExecuteStatement(Statement : IWpcBaseScriptStatement);
begin
  if (Assigned(FOnStatementExecution)) then
    FOnStatementExecution(Statement);

  inherited ExecuteStatement(Statement);
end;

procedure TWpcInThreadScriptTracer.SetWallpaper(Image : TWpcImage; Style : TWallpaperStyle);
begin
  if (Assigned(FOnSetWallpaper)) then
    FOnSetWallpaper(Image, Style);

  inherited SetWallpaper(Image, Style);
end;

procedure TWpcInThreadScriptTracer.TimerSleep(Milliseconds : LongWord);
begin
  if (Assigned(FOnWait)) then
    FOnWait(Milliseconds);

  inherited TimerSleep(Milliseconds);
end;


end.

