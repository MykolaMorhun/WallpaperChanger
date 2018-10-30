unit WpcWindows7WallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments,
  Windows,
  Registry;

type

  { TWpcWindows7WallpaperSetter }

  {
    This implementation sets wallpaper style via Windows registry and then sets
    wallpaper image using Windows SPI, which takes into account previously set wallpaper style.

    Changing of wallpaper style is done via editing of two registry parameters:
      HKEY_CURRENT_USER -> Control Panel -> Desktop -> WallpaperStyle
      HKEY_CURRENT_USER -> Control Panel -> Desktop -> TileWallpaper
    For more information about their values see
     https://docs.microsoft.com/en-us/windows/desktop/Controls/themesfileformat-overview#control-paneldesktop-section
  }
  TWpcWindows7WallpaperSetter = class(TWpcAbstractWallpaperSetter)
  const
    DESKTOP_REGISTRY_FOLDER_REG_KEY = '\Control Panel\Desktop\';
    DESKTOP_WALLPAPER_REG_KEY = 'Wallpaper';
    DESKTOP_WALLPAPER_STYLE_REG_KEY = 'WallpaperStyle';
    DESKTOP_WALLPAPER_TILE_REG_KEY = 'TileWallpaper';
  private
    DesktopRegistry : TRegIniFile;
  public
    constructor Create();
    destructor Destroy(); override;
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  private
    function WpcWallpaperStyleToWindows7WallpaperStyle(Style : TWpcWallpaperStyle) : String;
  end;


implementation

{ TWpcWindows7WallpaperSetter }

constructor TWpcWindows7WallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_WINDOWS_7;
  SupportedStyles := [ ZOOMED, SCALED, STRETCHED, TILED, CENTERED ];
  DefaultWallpaperStyle := ZOOMED;

  DesktopRegistry := TRegIniFile.Create(DESKTOP_REGISTRY_FOLDER_REG_KEY);
end;

destructor TWpcWindows7WallpaperSetter.Destroy();
begin
  DesktopRegistry.Free();
  inherited Destroy();
end;

procedure TWpcWindows7WallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
var
  StyleIndex : String;
  Registry   : TRegistry;
begin
  Validate(Path, Style);

  Registry := TRegistry.Create();
  Registry.RootKey := HKEY_CURRENT_USER;
  try
    if (Registry.OpenKey(DESKTOP_REGISTRY_FOLDER_REG_KEY, False)) then begin
      if (Style = TILED) then
        Registry.WriteString(DESKTOP_WALLPAPER_TILE_REG_KEY, '1')
      else
        Registry.WriteString(DESKTOP_WALLPAPER_TILE_REG_KEY, '0');

      StyleIndex := WpcWallpaperStyleToWindows7WallpaperStyle(Style);
      Registry.WriteString(DESKTOP_WALLPAPER_STYLE_REG_KEY, StyleIndex);

      Registry.CloseKey();
    end
  finally
    Registry.Free();
  end;

  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(Path), SPIF_UPDATEINIFILE or SPIF_SENDWININICHANGE);
end;

{
  Converts Wallpaper Style to Windows specific index of the style (see WallpaperStyle registry key).
  For TILED and CENTERED it should be set to 0 and the collision is resolved by
  setting TileWallpaper parameter into 0 (CENTERED) or 1 (TILED).
}
function TWpcWindows7WallpaperSetter.WpcWallpaperStyleToWindows7WallpaperStyle(Style : TWpcWallpaperStyle) : String;
begin
  case (Style) of
     ZOOMED:    Result := '10';
     SCALED:    Result := '6';
     STRETCHED: Result := '2';
     TILED:     Result := '0';
     CENTERED:  Result := '0';
  end;
end;


end.

