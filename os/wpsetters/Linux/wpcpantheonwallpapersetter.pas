unit WpcPantheonWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcGnome3WallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments;

type

  { TWpcPantheonWallpaperSetter }

  TWpcPantheonWallpaperSetter = class(TWpcGnome3WallpaperSetter)
  public
    constructor Create();
  end;

implementation

{ TWpcPantheonWallpaperSetter }

constructor TWpcPantheonWallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_PANTHEON;
  SupportedStyles := [ TILED, CENTERED, SCALED, STRETCHED, ZOOMED, SPANNED ];
  DefaultWallpaperStyle := ZOOMED;
end;


end.

