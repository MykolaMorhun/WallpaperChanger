unit WpcGnomeClassicWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcGnome3WallpaperSetter,
  WpcDesktopEnvironments;

type

  { TWpcGnomeClassicWallpaperSetter }

  TWpcGnomeClassicWallpaperSetter = class(TWpcGnome3WallpaperSetter)
  public
    constructor Create();
  end;


implementation

{ TWpcGnomeClassicWallpaperSetter }

constructor TWpcGnomeClassicWallpaperSetter.Create();
begin
  inherited Create();
  TargetDesktopEnvironment := DE_GNOME_CLASSIC;
end;


end.

