unit WpcWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperStyles,
  WpcDesktopEnvironments;

type

  IWallpaperSetter = class abstract(TObject)
    // Sets given image as desktop wallpaper with given style.
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); virtual; abstract;
    // Returns set of all supported in this setter wallpepar styles.
    function GetWallpaperStylesSupported() : TWpcSetOfWallpaperStyles; virtual; abstract;
    // Checks whether given style (e.g. TILE) is supported by this wallpaper setter.
    function IsWallpaperStyleSupported(Style : TWpcWallpaperStyle) : Boolean; virtual; abstract;
    // Checks whether given image type (e.g. png) is supported by this wallpaper setter.
    function IsWallpaperTypeSupported(Image : String) : Boolean; virtual; abstract;
    // Returns desktop environmer of this wallpaper setter.
    function GetEnvironmet() : TDesktopEnvironment; virtual; abstract;
  end;

implementation


end.

