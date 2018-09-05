unit WpcXfceWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractLinuxWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments,
  Unix;

type

  { TWpcXfceWallpaperSetter }

  {
    To get XFCE version:
      xfce4-about --version

    To set wallpaper in XFCE 4.12 and higer one can use commands:
      xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitor0/workspace0/last-image --set /path/to/image.jpg
      xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitor0/workspace0/image-style --set 1

    This setter doesn't handle multiple monitors or workspaces.
    // TODO add support for several monitors.
  }
  TWpcXfceWallpaperSetter = class(TWpcAbstractLinuxWallpaperSetter)
  private const
    DESKTOP_QUERY = 'xfconf-query --channel xfce4-desktop';
    PROPRTY_TEMPLATE = '--property /backdrop/screen%d/monitor%d/workspace%d/';
    IMAGE_PATH_KEY = 'last-image';
    IMAGE_STYLE_KEY = 'image-style';
    SET_KEY = '--set';
  private
    FSetWallpaperImageQuery : String;
    FSetWallpaperStyleQuery : String;
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  private
    procedure CreateQueriesTemplates(); inline;

    function WpcWallpaperStyleToXfceWallpaperStyle(Style : TWpcWallpaperStyle) : String;
  end;


implementation

{ TWpcXfceWallpaperSetter }

constructor TWpcXfceWallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_XFCE;
  SupportedStyles := [ CENTERED, TILED, STRETCHED, SCALED, ZOOMED ];
  DefaultWallpaperStyle := ZOOMED;

  CreateQueriesTemplates();
end;

procedure TWpcXfceWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
begin
  Validate(Path, Style);

  fpSystem(FSetWallpaperImageQuery + '"' + Path + '" && ' +
           FSetWallpaperStyleQuery + WpcWallpaperStyleToXfceWallpaperStyle(Style));
end;

procedure TWpcXfceWallpaperSetter.CreateQueriesTemplates();
var
  CommonPrefix : String;
begin
  // TODO handle many monitors and workspaces
  CommonPrefix := DESKTOP_QUERY + ' ' + Format(PROPRTY_TEMPLATE, [0, 0, 0]);
  FSetWallpaperImageQuery := CommonPrefix + IMAGE_PATH_KEY + ' ' + SET_KEY + ' ';
  FSetWallpaperStyleQuery := CommonPrefix + IMAGE_STYLE_KEY + ' ' + SET_KEY + ' ';
end;

function TWpcXfceWallpaperSetter.WpcWallpaperStyleToXfceWallpaperStyle(Style : TWpcWallpaperStyle) : String;
begin
  case (Style) of
    CENTERED:  Result := '1';
    TILED:     Result := '2';
    STRETCHED: Result := '3';
    SCALED:    Result := '4';
    ZOOMED:    Result := '5';
    // NONE:      Result := '0';
  end;
end;


end.

