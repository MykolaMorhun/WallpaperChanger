unit WpcBranchStatement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Fgl,
  WpcBaseStatement,
  WpcScriptCommons;

type

  TListOfBranchStatements = specialize TFPGList<IWpcBaseScriptStatement>;

  { TWpcBranchStatement }

  TWpcBranchStatement = class(IWpcBaseScriptStatement)
  private
    FStatements : TListOfBranchStatements;
    FBranchName : String;
  public
    constructor Create(BranchName : String);
    destructor Destroy(); override;
  public
    procedure AddStatement(Statement : IWpcBaseScriptStatement);
    function GetStatement(StatementNumber : Integer) : IWpcBaseScriptStatement;
    function GetBranchStatements() : TListOfBranchStatements;
    function CountStatements() : Integer;
    function GetName() : String;

    function GetId() : TWpcStatemetId; override;
  end;


implementation

{ TWpcBranchStatement }

function TWpcBranchStatement.GetId() : TWpcStatemetId;
begin
  Result := WPC_BRANCH_STATEMENT_ID;
end;

constructor TWpcBranchStatement.Create(BranchName : String);
begin
  FBranchName := BranchName;
  FStatements := TListOfBranchStatements.Create();
end;

destructor TWpcBranchStatement.Destroy();
var
  Statement : IWpcBaseScriptStatement;
begin
  for Statement in FStatements do
    Statement.Free();
  FStatements.Free();
  inherited Destroy();
end;

procedure TWpcBranchStatement.AddStatement(Statement : IWpcBaseScriptStatement);
begin
  FStatements.Add(Statement);
end;

function TWpcBranchStatement.GetStatement(StatementNumber : Integer) : IWpcBaseScriptStatement;
begin
  Result := FStatements[StatementNumber];
end;

function TWpcBranchStatement.GetBranchStatements() : TListOfBranchStatements;
begin
  Result := FStatements;
end;

function TWpcBranchStatement.CountStatements() : Integer;
begin
  Result := FStatements.Count;
end;

function TWpcBranchStatement.GetName() : String;
begin
  Result := FBranchName;
end;


end.

