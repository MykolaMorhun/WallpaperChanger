unit WpcWindows2000WallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWindowsAbstractWallpaperSetter,
  WpcDesktopEnvironments,
  WpcWallpaperStyles,
  WpcImagesUtils;

type

  { TWpcWindows2000WallpaperSetter }

  TWpcWindows2000WallpaperSetter = class(TWpcWindowsAbstractWallpaperSetter)
  private
    BmpWallpaperPath : String;
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  end;


implementation

{ TWpcWindows2000WallpaperSetter }

constructor TWpcWindows2000WallpaperSetter.Create();
begin
  inherited Create();

  TargetDesktopEnvironment := DE_WINDOWS_2000;
  SupportedStyles := [ CENTERED, TILED, STRETCHED ];
  DefaultWallpaperStyle := CENTERED;

  BmpWallpaperPath := GetTempDir(False) + 'wpc_wallpaper.bmp';
end;

procedure TWpcWindows2000WallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
begin
  Validate(Path, Style);

  if (LowerCase(ExtractFileExt(Path)) <> 'bmp') then
    ConvertImageToBmp(Path, BmpWallpaperPath);

  inherited SetDesktopWallpaper(BmpWallpaperPath, Style);
end;


end.

