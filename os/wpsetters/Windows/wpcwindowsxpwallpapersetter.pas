unit WpcWindowsXPWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWindowsAbstractWallpaperSetter,
  WpcDesktopEnvironments,
  WpcWallpaperStyles,
  WpcImagesUtils;

type

  { TWpcWindowsXPWallpaperSetter }

  TWpcWindowsXPWallpaperSetter = class(TWpcWindowsAbstractWallpaperSetter)
  private
    BmpWallpaperPath : String;
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  end;


implementation

{ TWpcWindowsXPWallpaperSetter }

constructor TWpcWindowsXPWallpaperSetter.Create();
begin
  inherited Create();

  TargetDesktopEnvironment := DE_WINDOWS_XP;
  SupportedStyles := [ CENTERED, TILED, STRETCHED ];
  DefaultWallpaperStyle := CENTERED;

  BmpWallpaperPath := GetTempDir(False) + 'wallpaper.bmp';
end;

procedure TWpcWindowsXPWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
begin
  Validate(Path, Style);

  if (LowerCase(ExtractFileExt(Path)) <> 'bmp') then
    ConvertImageToBmp(Path, BmpWallpaperPath);

  inherited SetDesktopWallpaper(BmpWallpaperPath, Style);
end;


end.

