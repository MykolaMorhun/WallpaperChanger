unit WpcLxdeWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractLinuxWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments,
  Unix;

type

  { TWpcLxdeWallpaperSetter }

  TWpcLxdeWallpaperSetter = class(TWpcAbstractLinuxWallpaperSetter)
  private const
    COMMAND = 'pcmanfm';
    SET_WALLPAPER_KEY = '--set-wallpaper';
    SET_WALLPAPER_STYLE_KEY = '--wallpaper-mode';
  private
    FSetWallpaperCommandTemplate : String;
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  private
    procedure CreateCommandTemplete(); inline;

    function WpcWallpaperStyleToLxdeWallpaperStyle(Style : TWpcWallpaperStyle) : String;
  end;


implementation

{ TWpcLxdeWallpaperSetter }

constructor TWpcLxdeWallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_LXDE;
  SupportedStyles := [ STRETCHED, SCALED, CENTERED, TILED, ZOOMED, SPANNED ];
  DefaultWallpaperStyle := ZOOMED;

  CreateCommandTemplete();
end;

procedure TWpcLxdeWallpaperSetter.CreateCommandTemplete();
begin
  FSetWallpaperCommandTemplate :=
    COMMAND + ' ' + SET_WALLPAPER_KEY + '=''%s'' ' + SET_WALLPAPER_STYLE_KEY + '=%s';
end;

procedure TWpcLxdeWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
begin
  Validate(Path, Style);

  fpSystem(Format(FSetWallpaperCommandTemplate, [Path, WpcWallpaperStyleToLxdeWallpaperStyle(Style)]));
end;

function TWpcLxdeWallpaperSetter.WpcWallpaperStyleToLxdeWallpaperStyle(Style : TWpcWallpaperStyle) : String;
begin
  case (Style) of
    ZOOMED:    Result := 'crop';
    TILED:     Result := 'tile';
    CENTERED:  Result := 'center';
    STRETCHED: Result := 'stretch';
    SCALED:    Result := 'fit';
    SPANNED:   Result := 'screen';
  end;
end;


end.

