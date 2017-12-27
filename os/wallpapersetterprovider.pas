unit WallpaperSetterProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcExceptions,
  WallpaperSetter,
  OSUtils;

type

  { TWpcWallpaperSetterProvider }

  TWpcWallpaperSetterProvider = class(TObject)
  public
    function GetWallpaperSetter(SetterType : TWpcWallpaperSetterType) : IWallpaperSetter;
  private
    function DetectCurrentEnvironmentWallaperSetter() : TWpcWallpaperSetterType;
  end;

implementation

{ TWpcWallpaperSetterProvider }

function TWpcWallpaperSetterProvider.GetWallpaperSetter(SetterType: TWpcWallpaperSetterType): IWallpaperSetter;
begin
  case SetterType of
    WPST_AUTODETECT:
      Result := Self.GetWallpaperSetter(DetectCurrentEnvironmentWallaperSetter());
    WPST_DEBUG:
      Result := TWpcDebugWallpaperSetter.Create('Debug.txt')
  else
    raise TWpcUseErrorException.Create('Unsupported wallaper setter.');
  end;
end;

function TWpcWallpaperSetterProvider.DetectCurrentEnvironmentWallaperSetter() : TWpcWallpaperSetterType;
begin
  // TODO
  Result := WPST_DEBUG;
end;

end.

