unit WpcWallpaperSetterFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperSetter,
  WpcDesktopEnvironments;

type

  IWpcWallpaperSetterFactory = class abstract(TObject)
  public
    function GetWallpaperSetter(DesktopEnvironment: TDesktopEnvironment) : IWpcWallpaperSetter; virtual; abstract;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; virtual; abstract;
  end;

implementation


end.

