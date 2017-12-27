unit WallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcCommonTypes,
  WpcWallpaperStyles,
  WpcExceptions;

type

  // TODO add all types, like WINDOWS_7, XFCE, ...
  TWpcWallpaperSetterType = (
    WPST_AUTODETECT,
    WPST_DEBUG,
    WPST_CUSTOM,
    WPST_
  );

  { IWallpaperSetter }

  IWallpaperSetter = class(TObject)
    procedure SetDesktopWallpaper(Path : String; Style : TWallpaperStyle); virtual; abstract;
  end;

  { TWpcDebugWallpaperSetter }

  TWpcDebugWallpaperSetter = class(IWallpaperSetter)
  private
    FLogFile : TextFile;
  public
    constructor Create(LogFile : String);
    destructor Destroy; override;

    procedure SetDesktopWallpaper(Path : String; Style : TWallpaperStyle); override;
  end;

implementation

{ TWpcDebugWallpaperSetter }

constructor TWpcDebugWallpaperSetter.Create(LogFile: String);
begin
  if (LogFile = '') then
    raise TWpcIllegalArgumentException.Create('Log file should be specified.');
  AssignFile(FLogFile, LogFile);
  Rewrite(FLogFile);
end;

destructor TWpcDebugWallpaperSetter.Destroy();
begin
  CloseFile(FLogFile);
  inherited Destroy();
end;

procedure TWpcDebugWallpaperSetter.SetDesktopWallpaper(Path: String; Style: TWallpaperStyle);
begin
  Writeln(FLogFile, Path, ' : ', Style, ' at ', DateTimeToStr(Now()));
end;

end.

