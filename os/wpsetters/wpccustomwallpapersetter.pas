unit WpcCustomWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Process,
  WpcWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments;

type

  { TWpcCustomWallpaperSetter }

  TWpcCustomWallpaperSetter = class(IWallpaperSetter)
  private
    SupportedStyles : TWpcSetOfWallpaperStyles;
  private
    PathToExecutable: String;
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
    function GetWallpaperStylesSupported() : TWpcSetOfWallpaperStyles; override;
    function IsWallpaperStyleSupported(Style : TWpcWallpaperStyle) : Boolean; override;
    function IsWallpaperTypeSupported(Image : String) : Boolean; override;
    function GetEnvironmet() : TDesktopEnvironment; override;
  end;

implementation

uses
  WpcApplication;

{ TWpcCustomWallpaperSetter }

constructor TWpcCustomWallpaperSetter.Create();
var
  WallpaperStyle : TWpcWallpaperStyle;
begin
  PathToExecutable := ApplicationManager.CurrentSettings.CustomSetter;

  for WallpaperStyle in TWpcWallpaperStyle do
    Include(SupportedStyles, WallpaperStyle);
  Exclude(SupportedStyles, TWpcWallpaperStyle.UNKNOWN);
end;

procedure TWpcCustomWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
var
  output : String;
begin
  RunCommand(PathToExecutable, [ Path, WallpaperStyleToStr(Style) ],
             output,
             [ poWaitOnExit ]);
end;

function TWpcCustomWallpaperSetter.GetWallpaperStylesSupported() : TWpcSetOfWallpaperStyles;
begin
  Result := SupportedStyles;
end;

function TWpcCustomWallpaperSetter.IsWallpaperStyleSupported(Style : TWpcWallpaperStyle) : Boolean;
begin
  Result := true;
end;

function TWpcCustomWallpaperSetter.IsWallpaperTypeSupported(Image : String) : Boolean;
begin
  Result := true;
end;

function TWpcCustomWallpaperSetter.GetEnvironmet() : TDesktopEnvironment;
begin
  Result := DE_CUSTOM;
end;


end.

