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
  WpcStatementProperties;

type

  { TWpcScriptsGenerator }

  TWpcScriptsGenerator = class(TObject)
    function GenerateDirectoryStatementScript(Dirctory : TWpcDirectory; Settings : TWpcPersistentSettings) : TWpcScript;
  end;

implementation

{ TWpcScriptsGenerator }

{
  Generates script for changing wallpapers within spesified directory.

  Settings is read only.
}
function TWpcScriptsGenerator.GenerateDirectoryStatementScript(Dirctory : TWpcDirectory; Settings : TWpcPersistentSettings) : TWpcScript;
var
  Script : TWpcScript;
  Branch : TWpcBranchStatement;
  DirectoryStatement : TWpcDirectoryStatement;
begin
  DirectoryStatement := TWpcDirectoryStatement.Create(Dirctory, Settings.KeepOrder, true);
  DirectoryStatement.SetStyle(Settings.WallpaperStyle);
  DirectoryStatement.SetDelay(Settings.ConstantDelay); // TODO add support for vary delay
  DirectoryStatement.SetTimes(TWpcTimesStatementProperty.MAX_TIMES); // TODO add FOREVER flag
  DirectoryStatement.SetProbability(100);

  Branch := TWpcBranchStatement.Create(MAIN_BARNCH);
  Branch.AddStatement(DirectoryStatement);

  Script := TWpcScript.Create();
  Script.AddBranch(MAIN_BARNCH, Branch);

  Result := Script;
end;


end.

