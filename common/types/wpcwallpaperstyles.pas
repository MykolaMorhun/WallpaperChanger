unit WpcWallpaperStyles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  WPST_CENTERED = 'CENTERED';
  WPST_TILED = 'TILED';
  WPST_ZOOMED = 'ZOOMED';
  WPST_SCALED = 'SCALED';
  WPST_STRETCHED = 'STRETCHED';
  WPST_SPANNED = 'SPANNED';

type

  {
    Contains all available desktop wallpaper styles.
    Note, the same style could be named differently on different platforms/environments,
    so it is possible that a few names reference to the same actual style. Usually they are
    similar, like Zoomed and Zoom. Also set of supported styles may differ between desktops.

    Styles (unique, ids):
      Tiled:
        Image is duplicated with its original size in both directions until whole
        screen will be filled.
      Centered:
        Puts the center of the image in the center of desktop keeping original image size.
      Zoomed:
        Like Centered but if image size is not enought to fill the screen, scales image
        keeping ratio until it fills whole screen (crops image if it has different aspect
        ratio than screen).
      Scaled:
        Resizes image keeping ratio until it can be placed in screen (leave spaces if the
        image has different aspect ratio than screen)
      Stretched:
        Resizes image to fit destop size. Does not keep proportions.
      Spanned:
        Sets image through all monitors.
      Center-Tiled:
        Like Center but if image size is less than screen size it will tile the screen
        starting from centered one.

    List of Desktop Environments in format: Original-name (unique id, see above):
    Linux desktops:
      MATE: Center, Tile, Scale, Stretch, Span, Zoom.
      XFCE: Centered, Tiled, Stretched, Scaled, Zoomed.
      GNOME: Centered, Wallpaper (Center-Tiled), Stretched, Scaled, Spanned, Zoom.
      KDE (Plasma): Scaled-and-Cropped (Zoomed), Scaled (Stretched), Scaled-keep-proportions (Scaled), Centered, Tiled.
      CINNAMON: Mosaic (Tiled), Centered, Scaled, Stretched, Zoom, Spanned.
      LXDE (ids only): Stretched, Scaled, Centered, Tiled, Zoomed, Spanned.

    Windows family:
      Windows 10: Fill (Zoomed), Fit (Scaled), Stretch, Tile, Center, Span.
      Windows 8: Fill (Zoomed), Fit (Scaled), Stretch, Tile, Center, Span.
      Windows 7: Fill (Zoomed), Fit (Scaled), Stretch, Tile, Center.
      Windows XP: Center, Tile, Stretch.
      React OS: Center, Stretch, Tile, Fit (Scaled), Fill (Zoomed).
  }

  TWpcWallpaperStyle = (
    CENTERED,
    TILED,
    ZOOMED,
    SCALED,
    STRETCHED,
    SPANNED,
    UNKNOWN // Unknown should be the last
  );

  TWpcSetOfWallpaperStyles = Set of TWpcWallpaperStyle;

function WallpaperStyleToStr(Style : TWpcWallpaperStyle) : String;
function StrToWallpaperStyle(Style : String) : TWpcWallpaperStyle;

implementation

function WallpaperStyleToStr(Style : TWpcWallpaperStyle) : String;
begin
  case (Style) of
    CENTERED:  Result := WPST_CENTERED;
    TILED:     Result := WPST_TILED;
    ZOOMED:    Result := WPST_ZOOMED;
    SCALED:    Result := WPST_SCALED;
    STRETCHED: Result := WPST_STRETCHED;
    SPANNED:   Result := WPST_SPANNED;
  end;
end;

function StrToWallpaperStyle(Style : String): TWpcWallpaperStyle;
begin
  Style := UpperCase(Style);
  case (Style) of
    WPST_CENTERED:  Result := CENTERED;
    WPST_TILED:     Result := TILED;
    WPST_ZOOMED:    Result := ZOOMED;
    WPST_SCALED:    Result := SCALED;
    WPST_STRETCHED: Result := STRETCHED;
    WPST_SPANNED:   Result := SPANNED;
    else
      Result := UNKNOWN;
  end;
end;


end.

