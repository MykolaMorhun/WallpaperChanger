unit WpcWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperStyles;

type

  IWallpaperSetter = class abstract(TObject)
    // Sets given image as desktop wallpaper with given style.
    procedure SetDesktopWallpaper(Path : String; Style : TWallpaperStyle); virtual; abstract;
    // Checks whether given style (e.g. TILE) is supported by this wallpaper setter.
    function IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean; virtual; abstract;
    // Checks whether given image type (e.g. png) is supported by this wallpaper setter.
    function IsWallpaperTypeSupported(Image : String) : Boolean; virtual; abstract;
  end;

implementation


end.

