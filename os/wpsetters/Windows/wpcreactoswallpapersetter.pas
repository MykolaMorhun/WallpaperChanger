unit WpcReactOSWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWindowsAbstractWallpaperSetter,
  WpcDesktopEnvironments,
  WpcWallpaperStyles,
  WpcImagesUtils;

type

  { TWpcReactOSWallpaperSetter }

  TWpcReactOSWallpaperSetter = class(TWpcWindowsAbstractWallpaperSetter)
  private
    BmpWallpaperPath : String;
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  end;


implementation

{ TWpcReactOSWallpaperSetter }

constructor TWpcReactOSWallpaperSetter.Create();
begin
  inherited Create();

  TargetDesktopEnvironment := DE_REACTOS;
  SupportedStyles := [ CENTERED, STRETCHED, TILED, SCALED, ZOOMED ];
  DefaultWallpaperStyle := ZOOMED;

  BmpWallpaperPath := GetTempDir(False) + 'wpc_wallpaper.bmp';
end;

procedure TWpcReactOSWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
begin
  Validate(Path, Style);

  if (LowerCase(ExtractFileExt(Path)) <> 'bmp') then
    ConvertImageToBmp(Path, BmpWallpaperPath);

  inherited SetDesktopWallpaper(BmpWallpaperPath, Style);
end;


end.

