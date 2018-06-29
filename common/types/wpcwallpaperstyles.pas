unit WpcWallpaperStyles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  WPST_CENTER = 'CENTER';
  WPST_CENTRE = WPST_CENTER;   // ?
  WPST_TILE = 'TILE';
  WPST_STRETCH = 'STRETCH';
  // TODO add other types

type
   // TODO alises for strings, FILL == ZOOM, FIT == SCALED, STRETCH == STRETCHED, ... but Enum should be unique.
   TWallpaperStyle = (CENTER, TILE, STRETCH, UNKNOWN);
  // ROS: STRETCH, TILE, CENTER  (alias CENTRE ?)
  // win7: FILL(ZOOM), FIT(scaled),  STRETCH, TILE, CENTER
  // xfce: STRETCHed, TILEd, CENTERed , SCALED (fit), Zoomed
  // NONE ?

  TWpcSetOfWallpaperStyles = Set of TWallpaperStyle;

function WallpaperStyleToStr(Style : TWallpaperStyle) : String;
function StrToWallpaperStyle(Style : String) : TWallpaperStyle;

implementation

function WallpaperStyleToStr(Style: TWallpaperStyle) : String;
begin
  case (Style) of
    CENTER: Result := WPST_CENTER;
    TILE: Result := WPST_TILE;
    STRETCH : Result := WPST_STRETCH;
  end;
end;

function StrToWallpaperStyle(Style: String): TWallpaperStyle;
begin
  case (Style) of
    WPST_CENTER:
      Result := CENTER;
    WPST_TILE:
      Result := TILE;
    WPST_STRETCH:
      Result := STRETCH;
    else
      Result := UNKNOWN;
  end;
end;

end.

