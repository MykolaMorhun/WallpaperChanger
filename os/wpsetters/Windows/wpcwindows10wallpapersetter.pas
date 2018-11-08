unit WpcWindows10WallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWindowsAbstractWallpaperSetter,
  WpcDesktopEnvironments,
  WpcWallpaperStyles;

type

  { TWpcWindows10WallpaperSetter }

  TWpcWindows10WallpaperSetter = class(TWpcWindowsAbstractWallpaperSetter)
  public
    constructor Create();
  end;


implementation

{ TWpcWindows10WallpaperSetter }

constructor TWpcWindows10WallpaperSetter.Create();
begin
  inherited Create();

  TargetDesktopEnvironment := DE_WINDOWS_10;
  SupportedStyles := [ ZOOMED, SCALED, STRETCHED, TILED, CENTERED, SPANNED ];
  DefaultWallpaperStyle := ZOOMED;
end;


end.

