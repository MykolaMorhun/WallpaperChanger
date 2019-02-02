unit WpcWaitStatement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DateUtils,
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
    constructor Create(Delay : LongWord; IsStatic : Boolean = True);
    destructor Destroy(); override;
  public
    procedure SetDelay(Delay : LongWord; IsStatic : Boolean = True);
    function GetDelay() : LongWord;
    function GetOriginalDelayValue() : LongWord;
    function IsDelayStatic() : Boolean;
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

constructor TWpcWaitStatement.Create(Delay : LongWord; IsStatic : Boolean);
begin
  FDelay := TWpcDelayStatementProperty.Create();
  FProbability := TWpcProbabilityStatementProperty.Create();
  FTimes := TWpcTimesStatementProperty.Create();

  FDelay.Delay := Delay;
  FDelay.IsStatic := IsStatic;
end;

destructor TWpcWaitStatement.Destroy();
begin
  FDelay.Free();
  FProbability.Free();
  FTimes.Free();
  inherited Destroy();
end;

procedure TWpcWaitStatement.SetDelay(Delay: LongWord; IsStatic : Boolean = True);
begin
  FDelay.Delay := Delay;
  FDelay.IsStatic := IsStatic;
end;

function TWpcWaitStatement.GetDelay() : LongWord;
begin
  Result := FDelay.Delay;
end;

function TWpcWaitStatement.GetOriginalDelayValue() : LongWord;
begin
  Result := FDelay.HoldingValue;
end;

function TWpcWaitStatement.IsDelayStatic() : Boolean;
begin
  Result := FDelay.IsStatic;
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

