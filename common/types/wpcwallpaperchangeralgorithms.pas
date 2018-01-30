unit WpcWallpaperChangerAlgorithms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  WPCA_SCRIPT_ID = 'SCRIPT';
  WPCA_DIRECTORY_ID = 'DIRECTORY';
  WPCA_IMAGE_ID = 'IMAGE';
  WPCA_UNKNOWN_ID = 'UNKNOWN';

type

  TWpcWallpaperChangerAlgorithm = (WPCA_SCRIPT, WPCA_DIRECTORY, WPCA_IMAGE, WPCA_UNKNOWN);


function WallpaperChangerAlgorithmToStr(WallpaperChangerAlgorithm : TWpcWallpaperChangerAlgorithm) : String;
function StrToWallpaperChangerAlgorithm(WallpaperChangerAlgorithm : String) : TWpcWallpaperChangerAlgorithm;

implementation

function WallpaperChangerAlgorithmToStr(WallpaperChangerAlgorithm : TWpcWallpaperChangerAlgorithm) : String;
begin
  case (WallpaperChangerAlgorithm) of
    WPCA_SCRIPT:    Result := WPCA_SCRIPT_ID;
    WPCA_DIRECTORY: Result := WPCA_DIRECTORY_ID;
    WPCA_IMAGE:     Result := WPCA_IMAGE_ID;
    WPCA_UNKNOWN:   Result := WPCA_UNKNOWN_ID;
  end;
end;

function StrToWallpaperChangerAlgorithm(WallpaperChangerAlgorithm : String) : TWpcWallpaperChangerAlgorithm;
begin
  case (UpperCase(WallpaperChangerAlgorithm)) of
    WPCA_SCRIPT_ID:    Result := WPCA_SCRIPT;
    WPCA_DIRECTORY_ID: Result := WPCA_DIRECTORY;
    WPCA_IMAGE_ID:     Result := WPCA_IMAGE;
    else
      Result := WPCA_UNKNOWN;
  end;
end;


end.

