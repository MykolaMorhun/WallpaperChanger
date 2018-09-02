unit WpcCinnamonWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractLinuxWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments,
  Unix;

type

  { TWpcCinnamonWallpaperSetter }

  TWpcCinnamonWallpaperSetter = class(TWpcAbstractLinuxWallpaperSetter)
  private const
    SET_WALLPAPER_COMMAND_TEMPLATE = 'gsettings set org.cinnamon.desktop.background picture-uri ';
    SET_WALLPAPER_STYLE_COMMAND_TEMPLATE = 'gsettings set org.cinnamon.desktop.background picture-options ';
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  private
    function WpcWallpaperStyleToCinnamonWallpaperStyle(Style : TWpcWallpaperStyle) : String;
  end;


implementation

{ TWpcCinnamonWallpaperSetter }

constructor TWpcCinnamonWallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_CINNAMON;
  SupportedStyles := [ CENTERED, SCALED, STRETCHED, SPANNED, ZOOMED, TILED ];
  DefaultWallpaperStyle := ZOOMED;
end;

procedure TWpcCinnamonWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
begin
  Validate(Path, Style);

  fpSystem(SET_WALLPAPER_COMMAND_TEMPLATE + '"file://' + Path + '" && ' +
           SET_WALLPAPER_STYLE_COMMAND_TEMPLATE + WpcWallpaperStyleToCinnamonWallpaperStyle(Style));
end;

function TWpcCinnamonWallpaperSetter.WpcWallpaperStyleToCinnamonWallpaperStyle(Style : TWpcWallpaperStyle): String;
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

