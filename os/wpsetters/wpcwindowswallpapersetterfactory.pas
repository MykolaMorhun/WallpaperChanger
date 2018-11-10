unit WpcWindowsWallpaperSetterFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperSetter,
  WpcWindows10WallpaperSetter,
  WpcWindows8WallpaperSetter,
  WpcWindows7WallpaperSetter,
  WpcWindowsVistaWallpaperSetter,
  WpcWindowsXPWallpaperSetter,
  WpcWindows2000WallpaperSetter,
  WpcReactOSWallpaperSetter,
  WpcWallpaperSetterFactory,
  WpcDesktopEnvironments;

type

  { TWpcWindowsWallpaperSetterFactory }

  TWpcWindowsWallpaperSetterFactory = class(IWpcWallpaperSetterFactory)
  private const
    SUPPORTED_ENVIRONMENTS = [
      DE_WINDOWS_10,
      DE_WINDOWS_8_1,
      DE_WINDOWS_8,
      DE_WINDOWS_7,
      DE_WINDOWS_VISTA,
      DE_WINDOWS_XP,
      DE_WINDOWS_2000,
      DE_REACTOS
    ];
  public
    function GetWallpaperSetter(DesktopEnvironment : TDesktopEnvironment) : IWpcWallpaperSetter; override;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; override;
  end;


implementation

{ TWpcWindowsWallpaperSetterFactory }

function TWpcWindowsWallpaperSetterFactory.GetWallpaperSetter(DesktopEnvironment : TDesktopEnvironment) : IWpcWallpaperSetter;
begin
  case (DesktopEnvironment) of
    DE_WINDOWS_10:    Result := TWpcWindows10WallpaperSetter.Create();
    DE_WINDOWS_8_1:   Result := TWpcWindows8_1WallpaperSetter.Create();
    DE_WINDOWS_8:     Result := TWpcWindows8WallpaperSetter.Create();
    DE_WINDOWS_7:     Result := TWpcWindows7WallpaperSetter.Create();
    DE_WINDOWS_VISTA: Result := TWpcWindowsVistaWallpaperSetter.Create();
    DE_WINDOWS_XP:    Result := TWpcWindowsXPWallpaperSetter.Create();
    DE_WINDOWS_2000:  Result := TWpcWindows2000WallpaperSetter.Create();

    DE_REACTOS:       Result := TWpcReactOSWallpaperSetter.Create();
  end;
end;

function TWpcWindowsWallpaperSetterFactory.GetSupportedEnvironments() : TDesktopEnvironmentsSet;
begin
  Result := SUPPORTED_ENVIRONMENTS;
end;


end.

