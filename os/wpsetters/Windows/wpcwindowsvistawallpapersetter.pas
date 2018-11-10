unit WpcWindowsVistaWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWindowsAbstractWallpaperSetter,
  WpcDesktopEnvironments,
  WpcWallpaperStyles;

type

  { TWpcWindowsVistaWallpaperSetter }

  TWpcWindowsVistaWallpaperSetter = class(TWpcWindowsAbstractWallpaperSetter)
  public
    constructor Create();
  end;


implementation

{ TWpcWindowsVistaWallpaperSetter }

constructor TWpcWindowsVistaWallpaperSetter.Create();
begin
  inherited Create();

  TargetDesktopEnvironment := DE_WINDOWS_VISTA;
  SupportedStyles := [ STRETCHED, TILED, CENTERED ];
  DefaultWallpaperStyle := ZOOMED;
end;


end.

