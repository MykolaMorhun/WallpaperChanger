unit WpcWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperStyles;

type

  IWallpaperSetter = class abstract(TObject)
    procedure SetDesktopWallpaper(Path : String; Style : TWallpaperStyle); virtual; abstract;
    function IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean; virtual; abstract;
  end;

implementation


end.

