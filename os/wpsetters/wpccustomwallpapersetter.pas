unit WpcCustomWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Process,
  WpcWallpaperSetter,
  WpcWallpaperStyles;

type

  { TWpcCustomWallpaperSetter }

  TWpcCustomWallpaperSetter = class(IWallpaperSetter)
  private
    PathToExecutable: String;
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWallpaperStyle); override;
    function IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean; override;
    function IsWallpaperTypeSupported(Image : String) : Boolean; override;
  end;

implementation

uses
  WpcApplication;

{ TWpcLinuxCustomWallpaperSetter }

constructor TWpcCustomWallpaperSetter.Create();
begin
  PathToExecutable := ApplicationManager.CurrentSettings.CustomSetter;
end;

procedure TWpcCustomWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWallpaperStyle);
var
  output : String;
begin
  RunCommand(PathToExecutable, [ Path, WallpaperStyleToStr(Style) ],
             output,
             [ poWaitOnExit ]);
end;

function TWpcCustomWallpaperSetter.IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean;
begin
  Result := true;
end;

function TWpcCustomWallpaperSetter.IsWallpaperTypeSupported(Image : String) : Boolean;
begin
  Result := true;
end;


end.

