unit WpcDirectory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWpcDirectory = class(TObject)
  public
    constructor Create(Path : String);
  private
    Path  : String;
  public
    function GetPath() : String;
    function IsExists() : Boolean;
  end;

implementation

constructor TWpcDirectory.Create(Path : String);
begin
  Self.Path := Path;
end;

function TWpcDirectory.GetPath() : String;
begin
  Result := Self.Path;
end;

function TWpcDirectory.IsExists() : Boolean;
begin
  Result := DirectoryExists(Self.Path);
end;

end.

