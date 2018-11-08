unit WpcWindowsAbstractWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractWallpaperSetter,
  WpcWallpaperStyles,
  Windows,
  Registry;

type

  { TWpcWindowsAbstractWallpaperSetter }

  {
    This implementation sets wallpaper style via Windows registry and then sets
    wallpaper image using Windows SPI, which takes into account previously set wallpaper style.

    Changing of wallpaper style is done via editing of two registry parameters:
      HKEY_CURRENT_USER -> Control Panel -> Desktop -> WallpaperStyle
      HKEY_CURRENT_USER -> Control Panel -> Desktop -> TileWallpaper
    For more information about their values see
     https://docs.microsoft.com/en-us/windows/desktop/Controls/themesfileformat-overview#control-paneldesktop-section
  }
  TWpcWindowsAbstractWallpaperSetter = class(TWpcAbstractWallpaperSetter)
  const
    DESKTOP_REGISTRY_FOLDER_REG_KEY = '\Control Panel\Desktop\';
    DESKTOP_WALLPAPER_STYLE_REG_KEY = 'WallpaperStyle';
    DESKTOP_WALLPAPER_TILE_REG_KEY = 'TileWallpaper';
  protected
    CurrentUserRegistry : TRegistry;
  public
    constructor Create();
    destructor Destroy(); override;
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  protected
    function WpcWallpaperStyleToWindowsWallpaperStyle(Style : TWpcWallpaperStyle) : String;
  end;


implementation

{ TWpcWindowsAbstractWallpaperSetter }

constructor TWpcWindowsAbstractWallpaperSetter.Create();
begin
  CurrentUserRegistry := TRegistry.Create();
  CurrentUserRegistry.RootKey := HKEY_CURRENT_USER;
end;

destructor TWpcWindowsAbstractWallpaperSetter.Destroy();
begin
  CurrentUserRegistry.Free();
  inherited Destroy();
end;

procedure TWpcWindowsAbstractWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
var
  StyleIndex : String;
begin
  Validate(Path, Style);

  if (CurrentUserRegistry.OpenKey(DESKTOP_REGISTRY_FOLDER_REG_KEY, False)) then begin
    if (Style = TILED) then
      CurrentUserRegistry.WriteString(DESKTOP_WALLPAPER_TILE_REG_KEY, '1')
    else
      CurrentUserRegistry.WriteString(DESKTOP_WALLPAPER_TILE_REG_KEY, '0');

    StyleIndex := WpcWallpaperStyleToWindowsWallpaperStyle(Style);
    CurrentUserRegistry.WriteString(DESKTOP_WALLPAPER_STYLE_REG_KEY, StyleIndex);

    CurrentUserRegistry.CloseKey();
  end;

  SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(Path), SPIF_UPDATEINIFILE or SPIF_SENDWININICHANGE);
end;

{
  Converts Wallpaper Style to Windows specific index of the style (see WallpaperStyle registry key).
  For TILED and CENTERED it should be set to 0 and the collision is resolved by
  setting TileWallpaper parameter into 0 (CENTERED) or 1 (TILED).
}
function TWpcWindowsAbstractWallpaperSetter.WpcWallpaperStyleToWindowsWallpaperStyle(Style : TWpcWallpaperStyle) : String;
begin
  case (Style) of
     SPANNED:   Result := '22';
     ZOOMED:    Result := '10';
     SCALED:    Result := '6';
     STRETCHED: Result := '2';
     TILED:     Result := '0';
     CENTERED:  Result := '0';
  end;
end;


end.

