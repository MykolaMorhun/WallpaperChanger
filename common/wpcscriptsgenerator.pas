unit WpcScriptsGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcOptions,
  WpcDirectory,
  WpcScript,
  WpcBranchStatement,
  WpcDirectoryStatement,
  WpcBranchActionsStatements,
  WpcStatementProperties,
  WpcExceptions;

type

  { TWpcScriptsGenerator }

  TWpcScriptsGenerator = class(TObject)
    function GenerateDirectoryStatementScript(Dirctory : TWpcDirectory; Settings : TWpcPersistentSettings) : TWpcScript;
  end;

implementation

{ TWpcScriptsGenerator }

{
  Generates script for changing wallpapers within spesified directory.
  Throws Runtime exception if specified directory doesn't contain images.

  Settings is read only.
}
function TWpcScriptsGenerator.GenerateDirectoryStatementScript(Dirctory : TWpcDirectory; Settings : TWpcPersistentSettings) : TWpcScript;
var
  Script : TWpcScript;
  Branch : TWpcBranchStatement;
  DirectoryStatement    : TWpcDirectoryStatement;
  SwitchBranchStatement : TWpcSwitchBranchStatement;
begin
  DirectoryStatement := TWpcDirectoryStatement.Create(Dirctory, Settings.KeepOrder, Settings.SearchInSubdirectories);
  if (DirectoryStatement.CountImages() = 0) then begin
    DirectoryStatement.Free();
    raise TWpcRuntimeException.Create('Specified directory doesn''t contain images');
  end;

  DirectoryStatement.SetStyle(Settings.WallpaperStyle);
  if (Settings.UseConstantDelay) then begin
    if (Settings.ConstantDelay = 0) then
      Settings.ConstantDelay := 1;
    DirectoryStatement.SetDelay(Settings.ConstantDelay);
    DirectoryStatement.SetRamdomDelay(0);
  end
  else begin
    if (Settings.MinimalDelay = 0) then
      Settings.MinimalDelay := 1;
    DirectoryStatement.SetDelay(Settings.MinimalDelay);
    DirectoryStatement.SetRamdomDelay(Settings.MaximalDelay - Settings.MinimalDelay);
  end;
  // TODO add FOREVER flag and remove switch branch statement
  DirectoryStatement.SetTimes(TWpcTimesStatementProperty.MAX_TIMES);
  DirectoryStatement.SetProbability(100);

  SwitchBranchStatement := TWpcSwitchBranchStatement.Create(MAIN_BARNCH);
  SwitchBranchStatement.SetProbability(100);

  Branch := TWpcBranchStatement.Create(MAIN_BARNCH);
  Branch.AddStatement(DirectoryStatement);
  Branch.AddStatement(SwitchBranchStatement);

  Script := TWpcScript.Create();
  Script.AddBranch(MAIN_BARNCH, Branch);

  Result := Script;
end;


end.

