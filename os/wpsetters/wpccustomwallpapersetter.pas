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
    procedure SetDesktopWallpaper(Path : String; Style : TWallpaperStyle); override;
    function GetWallpaperStylesSupported() : TWpcSetOfWallpaperStyles; override;
    function IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean; override;
    function IsWallpaperTypeSupported(Image : String) : Boolean; override;
    function GetEnvironmet() : TDesktopEnvironment; override;
  end;

implementation

uses
  WpcApplication;

{ TWpcLinuxCustomWallpaperSetter }

constructor TWpcCustomWallpaperSetter.Create();
var
  WallpaperStyle : TWallpaperStyle;
begin
  PathToExecutable := ApplicationManager.CurrentSettings.CustomSetter;

  for WallpaperStyle in TWallpaperStyle do
    Include(SupportedStyles, WallpaperStyle);
end;

procedure TWpcCustomWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWallpaperStyle);
var
  output : String;
begin
  RunCommand(PathToExecutable, [ Path, WallpaperStyleToStr(Style) ],
             output,
             [ poWaitOnExit ]);
end;

function TWpcCustomWallpaperSetter.GetWallpaperStylesSupported(): TWpcSetOfWallpaperStyles;
begin
  Result := SupportedStyles;
end;

function TWpcCustomWallpaperSetter.IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean;
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

