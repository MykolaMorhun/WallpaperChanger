unit WpcNullWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Process,
  WpcWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments;

type

  { TWpcNullWallpaperSetter }

  TWpcNullWallpaperSetter = class(IWallpaperSetter)
  private
    SupportedStyles : TWpcSetOfWallpaperStyles;
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWallpaperStyle); override;
    function GetWallpaperStylesSupported() : TWpcSetOfWallpaperStyles; override;
    function IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean; override;
    function IsWallpaperTypeSupported(Image : String) : Boolean; override;
    function GetEnvironmet() : TDesktopEnvironment; override;
  end;

implementation

{ TWpcNullWallpaperSetter }

constructor TWpcNullWallpaperSetter.Create();
var
  WallpaperStyle : TWallpaperStyle;
begin
  for WallpaperStyle in TWallpaperStyle do
    Include(SupportedStyles, WallpaperStyle);
end;

procedure TWpcNullWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWallpaperStyle);
begin
  // Do nothing.
end;

function TWpcNullWallpaperSetter.GetWallpaperStylesSupported() : TWpcSetOfWallpaperStyles;
begin
  Result := SupportedStyles;
end;

function TWpcNullWallpaperSetter.IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean;
begin
  Result := true;
end;

function TWpcNullWallpaperSetter.IsWallpaperTypeSupported(Image : String) : Boolean;
begin
  Result := true;
end;

function TWpcNullWallpaperSetter.GetEnvironmet() : TDesktopEnvironment;
begin
  Result := DE_CUSTOM;
end;



end.

