unit WpcCommonTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWpcTimeMeasurementUnits = (MILLISECONDS, SECONDS, MINUTES, HOURS, DAYS);

  { TWPCImage }

  TWpcImage = class(TObject)
  public
    constructor Create(Path : String);
  private
    Path  : String;
  public
    function GetPath() : String;
    function IsExists() : Boolean;
  end;

implementation

{ TWPCImage }

constructor TWpcImage.Create(Path: String);
begin
  Self.Path := Path;
end;

function TWpcImage.GetPath() : String;
begin
  Result := Self.Path;
end;

function TWpcImage.IsExists() : Boolean;
begin
  Result := FileExists(Self.Path);
end;

end.

