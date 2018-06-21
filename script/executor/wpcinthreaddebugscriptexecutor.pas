unit WpcInThreadDebugScriptExecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcInThreadScriptExecutor,
  WpcWallpaperSetter;

type
  TWpcInThreadDebugScriptExecutorLogger = procedure(Line: String) of Object;

  TWpcInThreadDebugScriptExecutor = class(TWpcInThreadScriptExecutor)
  public
    procedure SetLoggerCallback(Callback : TWpcInThreadDebugScriptExecutorLogger);
  protected
    procedure TimerSleep(Milliseconds : LongWord); override;
  end;

implementation

end.

