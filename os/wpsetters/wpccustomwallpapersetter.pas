unit WpcCustomWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
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
  end;

implementation

{ TWpcLinuxCustomWallpaperSetter }

constructor TWpcCustomWallpaperSetter.Create();
begin
  // TODO read PathToExecutable from settings
end;

procedure TWpcCustomWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWallpaperStyle);
begin
  try
    SysUtils.ExecuteProcess(PathToExecutable, [Path, WallpaperStyleToStr(Style)], []);
  except
    on E : EOSError do begin
      // Custom setter failed, skip it.
    end;
  end;
end;

function TWpcCustomWallpaperSetter.IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean;
begin
  Result := true;
end;


end.

