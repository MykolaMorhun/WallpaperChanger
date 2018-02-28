unit WpcDebugWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WallpaperSetter,
  WpcWallpaperStyles,
  WpcExceptions;

type

  { TWpcDebugWallpaperSetter }

   TWpcDebugWallpaperSetter = class(IWallpaperSetter)
   private
     FLogFile : TextFile;
   public
     constructor Create(LogFile : String);
     destructor Destroy; override;

     procedure SetDesktopWallpaper(Path : String; Style : TWallpaperStyle); override;
     function IsWallpaperStyleSupported(Style : TWallpaperStyle) : Boolean; override;
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

function TWpcDebugWallpaperSetter.IsWallpaperStyleSupported(Style: TWallpaperStyle): Boolean;
begin
 Result := True;
end;


end.

