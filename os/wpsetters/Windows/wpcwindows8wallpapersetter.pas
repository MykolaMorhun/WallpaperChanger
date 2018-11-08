unit WpcWindows8WallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWindowsAbstractWallpaperSetter,
  WpcDesktopEnvironments,
  WpcWallpaperStyles;

type

  { TWpcWindows8WallpaperSetter }

  TWpcWindows8WallpaperSetter = class(TWpcWindowsAbstractWallpaperSetter)
  public
    constructor Create();
  end;

  { TWpcWindows8_1WallpaperSetter }

  TWpcWindows8_1WallpaperSetter = class(TWpcWindowsAbstractWallpaperSetter)
  public
    constructor Create();
  end;


implementation

{ TWpcWindows8WallpaperSetter }

constructor TWpcWindows8WallpaperSetter.Create();
begin
  inherited Create();

  TargetDesktopEnvironment := DE_WINDOWS_8;
  SupportedStyles := [ ZOOMED, SCALED, STRETCHED, TILED, CENTERED, SPANNED ];
  DefaultWallpaperStyle := ZOOMED;
end;

{ TWpcWindows8_1WallpaperSetter }

constructor TWpcWindows8_1WallpaperSetter.Create();
begin
  inherited Create();

  TargetDesktopEnvironment := DE_WINDOWS_8_1;
  SupportedStyles := [ ZOOMED, SCALED, STRETCHED, TILED, CENTERED, SPANNED ];
  DefaultWallpaperStyle := ZOOMED;
end;


end.

