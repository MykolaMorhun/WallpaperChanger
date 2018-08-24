unit WpcWaitStatement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcBaseStatement,
  WpcStatementProperties,
  WpcScriptCommons;

type

  { TWpcWaitStatement }

  TWpcWaitStatement = class(IWpcBaseScriptStatement)
  private
    FDelay       : TWpcDelayStatementProperty;
    FProbability : TWpcProbabilityStatementProperty;
    FTimes       : TWpcTimesStatementProperty;
  public
    constructor Create(Delay : LongWord);
    destructor Destroy(); override;
  public
    procedure SetDelay(Delay : LongWord);
    function GetDelay() : LongWord;
    procedure SetProbability(Probability : Byte);
    function GetProbability() : Byte;
    procedure SetTimes(Times : LongWord);
    function GetTimes() : LongWord;

    function GetId() : TWpcStatemetId; override;
  end;


implementation

{ TWpcWaitStatement }

function TWpcWaitStatement.GetId() : TWpcStatemetId;
begin
  Result := WPC_WAIT_STATEMENT_ID;
end;

constructor TWpcWaitStatement.Create(Delay : LongWord);
begin
  FDelay := TWpcDelayStatementProperty.Create();
  FProbability := TWpcProbabilityStatementProperty.Create();
  FTimes := TWpcTimesStatementProperty.Create();

  FDelay.Delay := Delay;
end;

destructor TWpcWaitStatement.Destroy();
begin
  FDelay.Free();
  FProbability.Free();
  FTimes.Free();
  inherited Destroy();
end;

procedure TWpcWaitStatement.SetDelay(Delay : LongWord);
begin
  FDelay.Delay := Delay;
end;

function TWpcWaitStatement.GetDelay() : LongWord;
begin
   Result := FDelay.Delay;
end;

procedure TWpcWaitStatement.SetProbability(Probability : Byte);
begin
  FProbability.Probability := Probability;
end;

function TWpcWaitStatement.GetProbability() : Byte;
begin
  Result := FProbability.Probability;
end;

procedure TWpcWaitStatement.SetTimes(Times : LongWord);
begin
  FTimes.Times := Times;
end;

function TWpcWaitStatement.GetTimes() : LongWord;
begin
  Result := FTimes.Times;
end;


end.

