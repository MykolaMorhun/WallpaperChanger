unit WpcScript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Fgl,
  WpcStatements,
  WpcExceptions;

const
  MAIN_BARNCH = 'Main';

type

  { TWpcScript }

  TWpcScript = class(TObject)
  private type
    TWpcScriptBranches = specialize TFPGMap<String, TWpcBranchStatement>;
  private
    FBranches : TWpcScriptBranches;
  public
    constructor Create();
    destructor Destroy(); override;
  public
    procedure AddBranch(Name : String; Branch : TWpcBranchStatement);
    function GetBranch(BranchName : String) : TWpcBranchStatement;
    function GetBranchNames() : TStringList;
  end;

implementation

{ TWpcScript }

constructor TWpcScript.Create();
begin
  FBranches := TWpcScriptBranches.Create();
end;

destructor TWpcScript.Destroy();
var
  i : Integer;
begin
  for i:=0 to (FBranches.Count - 1) do
    FBranches.Data[i].Free();
  FBranches.Free();
  inherited Destroy;
end;

procedure TWpcScript.AddBranch(Name : String; Branch: TWpcBranchStatement);
begin
  if (FBranches.IndexOf(Name) <> -1) then
    raise TWpcUseErrorException.Create('Attempt to add already existing branch "' + Name + '".');

  FBranches.Add(Name, Branch);
end;

{
  Returns Branch statement or nil if branch with given name doesn't exist.
}
function TWpcScript.GetBranch(BranchName: String): TWpcBranchStatement;
var
  Index : Integer;
begin
  Index := FBranches.IndexOf(BranchName);
  if (Index <> -1) then
    Result := FBranches.Data[Index]
  else
    Result := nil;
end;

{
  Returns list of names of all branches.
  Note, that returning list should be freed by invoker.
}
function TWpcScript.GetBranchNames() : TStringList;
var
  BranchesNames : TStringList;
  i            : Integer;
begin
  BranchesNames := TStringList.Create();

  for i:=0 to (FBranches.Count - 1) do
    BranchesNames.Add(FBranches.Keys[i]);

  Result := BranchesNames;
end;

end.

