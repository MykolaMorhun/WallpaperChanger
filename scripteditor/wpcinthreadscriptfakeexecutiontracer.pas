unit WpcInThreadScriptFakeExecutionTracer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcInThreadScriptExecutorLogger,
  WpcWallpaperSetter,
  WpcNullWallpaperSetter,
  WpcLogger;

type

  { TWpcInThreadScriptFakeExecutionTracer }

  TWpcInThreadScriptFakeExecutionTracer = class(TWpcInThreadScriptExecutorLogger)
  public
    constructor Create(Logger : IWpcLogger);
    destructor Destroy(); override;
  protected
    procedure TimerSleep(Milliseconds : LongWord); override;
  end;

implementation

{ TWpcInThreadScriptFakeExecutionTracer }

constructor TWpcInThreadScriptFakeExecutionTracer.Create(Logger : IWpcLogger);
var
  NullWallpaperSetter : IWallpaperSetter;
begin
  FLogDateTime := False;
  FTraceLevel := TWpcScriptTraceLevel.STL_STATEMENT;

  // Just trace script and do not change actual wallpaper.
  NullWallpaperSetter := TWpcNullWallpaperSetter.Create();

  inherited Create(NullWallpaperSetter, Logger);
end;

destructor TWpcInThreadScriptFakeExecutionTracer.Destroy();
begin
  inherited Destroy();

  FWallpaperSetter.Free();
end;

procedure TWpcInThreadScriptFakeExecutionTracer.TimerSleep(Milliseconds : LongWord);
begin
  if (Assigned(FOnWait)) then
    FOnWait(Milliseconds);

  FTimer.Interval := 1; // Do not wait, just trace script and log actions.
  FTimer.Enabled := True;
end;


end.

