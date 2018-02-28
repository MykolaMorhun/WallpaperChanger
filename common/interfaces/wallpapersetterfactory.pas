unit WallpaperSetterFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WallpaperSetter,
  WpcDesktopEnvironments;

type

  IWpcWallpaperSetterFactory = class abstract(TObject)
  public
    function GetWallpaperSetter(DesktopEnvironment: TDesktopEnvironment) : IWallpaperSetter; virtual; abstract;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; virtual; abstract;
  end;

implementation


end.

