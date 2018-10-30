unit WpcGnome3WallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments,
  Unix;

type

  { TWpcGnome3WallpaperSetter }

  TWpcGnome3WallpaperSetter = class(TWpcAbstractWallpaperSetter)
  private const
    SET_WALLPAPER_COMMAND_TEMPLATE = 'gsettings set org.gnome.desktop.background picture-uri ';
    SET_WALLPAPER_STYLE_COMMAND_TEMPLATE = 'gsettings set org.gnome.desktop.background picture-options ';
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  private
    function WpcWallpaperStyleToGnome3WallpaperStyle(Style : TWpcWallpaperStyle) : String;
  end;


implementation

{ TWpcGnome3WallpaperSetter }

constructor TWpcGnome3WallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_GNOME;
  SupportedStyles := [ CENTERED, STRETCHED, SCALED, SPANNED, ZOOMED, TILED ];
  DefaultWallpaperStyle := ZOOMED;
  // SupportedImageTypes.AddStrings([
  //  'jpg', 'jpeg', 'png', 'gif', 'bmp'
  // ]);
end;

procedure TWpcGnome3WallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
begin
  Validate(Path, Style);

  fpSystem(SET_WALLPAPER_COMMAND_TEMPLATE + '"file://' + Path + '" && ' +
           SET_WALLPAPER_STYLE_COMMAND_TEMPLATE + WpcWallpaperStyleToGnome3WallpaperStyle(Style));
end;

function TWpcGnome3WallpaperSetter.WpcWallpaperStyleToGnome3WallpaperStyle(Style : TWpcWallpaperStyle) : String;
begin
  case (Style) of
    SCALED:    Result := 'scaled';
    SPANNED:   Result := 'spanned';
    ZOOMED:    Result := 'zoom';
    TILED:     Result := 'wallpaper';
    CENTERED:  Result := 'centered';
    STRETCHED: Result := 'stretched';
  end;
end;


end.

