unit WpcStopStatement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcBaseStatement,
  WpcStatementProperties,
  WpcScriptCommons;

type

  { TWpcStopStatement }

  TWpcStopStatement = class(IWpcBaseScriptStatement)
  private
    FProbability : TWpcProbabilityStatementProperty;
  public
    constructor Create();
    destructor Destroy(); override;
  public
    procedure SetProbability(Probability : Byte);
    function GetProbability() : Byte;

    function GetId() : TWpcStatemetId; override;
  end;


implementation

{ TWpcStopStatement }

function TWpcStopStatement.GetId() : TWpcStatemetId;
begin
  Result := WPC_STOP_STATEMENT_ID;
end;

constructor TWpcStopStatement.Create();
begin
  FProbability := TWpcProbabilityStatementProperty.Create();
end;

destructor TWpcStopStatement.Destroy();
begin
  FProbability.Free();
  inherited Destroy();
end;

procedure TWpcStopStatement.SetProbability(Probability : Byte);
begin
  FProbability.Probability := Probability;
end;

function TWpcStopStatement.GetProbability() : Byte;
begin
  Result := FProbability.Probability;
end;


end.

