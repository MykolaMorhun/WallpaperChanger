unit WpcUnityWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcGnome3WallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments;

type

  { TWpcUnityWallpaperSetter }

  TWpcUnityWallpaperSetter = class(TWpcGnome3WallpaperSetter)
  public
    constructor Create();
  end;

implementation

{ TWpcUnityWallpaperSetter }

constructor TWpcUnityWallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_UNITY;
  SupportedStyles := [ TILED, ZOOMED, CENTERED, SCALED, STRETCHED, SPANNED ];
  DefaultWallpaperStyle := ZOOMED;
end;


end.

