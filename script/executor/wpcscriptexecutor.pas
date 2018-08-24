unit WpcScriptExecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcScript;

type

  TWpcScriptExecutionExitStatus = (
    SES_FINISHED,
    SES_TERMINATED,
    SES_ERROR_STACK_OVERFLOW
  );

  TWpcScriptExecutorStopCallback = procedure(ExitStatus : TWpcScriptExecutionExitStatus) of Object;

  {
    Defines script executor capabilities.
  }
  IWpcScriptExecutor = class abstract(TObject)
    // Runs given script.
    procedure RunScript(Script : TWpcScript); virtual; abstract;

    // Stops execution of current script.
    procedure Terminate(); virtual; abstract;

    // Interrupts current delay and executes next statement immediately.
    procedure SkipCurrentDelay(); virtual; abstract;

    // Returns true if executor runs a script.
    // Script is supposed running if executor waits on a delay.
    function IsRunning() : Boolean; virtual; abstract;

    // Callback to invoke after script execution finished or terminated.
    procedure SetOnStopCallback(Callback : TWpcScriptExecutorStopCallback); virtual; abstract;
  end;


implementation


end.

