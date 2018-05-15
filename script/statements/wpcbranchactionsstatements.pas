unit WpcBranchActionsStatements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcBaseStatement,
  WpcStatementProperties,
  WpcScriptCommons;

type

  { IWpcBranchReferrer }

  IWpcBranchReferrer = class abstract(IWpcBaseScriptStatement)
  protected
    FBarnchName : String;
  public
    procedure SetBarnchName(BranchName : String);
    function GetBranchName() : String;
  end;

  { TWpcSwitchBranch }

  TWpcSwitchBranchStatement = class(IWpcBranchReferrer)
  private
    FProbability : TWpcProbabilityStatementProperty;
  public
    constructor Create(BranchName : String);
    destructor Destroy(); override;
  public
    procedure SetProbability(Probability : Byte);
    function GetProbability() : Byte;

    function GetId() : TWpcStatemetId; override;
  end;

  { TWpcUseBranchStatement }

  TWpcUseBranchStatement = class(TWpcSwitchBranchStatement)
  private
    FTimes : TWpcTimesStatementProperty;
  public
    constructor Create(BranchName : String);
    destructor Destroy(); override;
  public
    procedure SetTimes(Times : LongWord);
    function GetTimes() : LongWord;

    function GetId() : TWpcStatemetId; override;
  end;


implementation

{ IWpcBranchReferrer }

procedure IWpcBranchReferrer.SetBarnchName(BranchName : String);
begin
  FBarnchName := BranchName;
end;

function IWpcBranchReferrer.GetBranchName() : String;
begin
  Result := FBarnchName;
end;

{ TWpcSwitchBranchStatement }

function TWpcSwitchBranchStatement.GetId() : TWpcStatemetId;
begin
  Result := WPC_SWITCH_BRANCH_STATEMENT_ID;
end;

constructor TWpcSwitchBranchStatement.Create(BranchName : String);
begin
  FBarnchName := BranchName;
  FProbability := TWpcProbabilityStatementProperty.Create();
end;

destructor TWpcSwitchBranchStatement.Destroy();
begin
  FProbability.Free();
  inherited Destroy();
end;

procedure TWpcSwitchBranchStatement.SetProbability(Probability : Byte);
begin
  FProbability.Probability := Probability;
end;

function TWpcSwitchBranchStatement.GetProbability() : Byte;
begin
  Result := FProbability.Probability;
end;

{ TWpcUseBranchStatement }

function TWpcUseBranchStatement.GetId() : TWpcStatemetId;
begin
  Result := WPC_USE_BRANCH_STATEMENT_ID;
end;

constructor TWpcUseBranchStatement.Create(BranchName : String);
begin
  FTimes := TWpcTimesStatementProperty.Create();
  inherited Create(BranchName);
end;

destructor TWpcUseBranchStatement.Destroy();
begin
  FTimes.Free();
  inherited Destroy();
end;

procedure TWpcUseBranchStatement.SetTimes(Times : LongWord);
begin
  FTimes.Times := Times;
end;

function TWpcUseBranchStatement.GetTimes() : LongWord;
begin
  Result := FTimes.Times;
end;


end.

