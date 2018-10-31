unit WpcWindows7WallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWindowsAbstractWallpaperSetter,
  WpcDesktopEnvironments,
  WpcWallpaperStyles;

type

  { TWpcWindows7WallpaperSetter }

  TWpcWindows7WallpaperSetter = class(TWpcWindowsAbstractWallpaperSetter)
  public
    constructor Create();
  end;


implementation

{ TWpcWindows7WallpaperSetter }

constructor TWpcWindows7WallpaperSetter.Create();
begin
  inherited Create();

  TargetDesktopEnvironment := DE_WINDOWS_7;
  SupportedStyles := [ ZOOMED, SCALED, STRETCHED, TILED, CENTERED ];
  DefaultWallpaperStyle := ZOOMED;
end;


end.

