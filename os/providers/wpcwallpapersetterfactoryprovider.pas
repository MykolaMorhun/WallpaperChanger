unit WpcWallpaperSetterFactoryProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperSetterFactory,
  {$IFDEF LINUX} WpcLinuxWallpaperSetterFactory {$ENDIF}
  {$IFDEF WINDOWS} WpcWindowsWallpaperSetterFactory {$ENDIF}
  ;

function GetWallpaperSetterFactory() : IWpcWallpaperSetterFactory;


implementation

function GetWallpaperSetterFactory() : IWpcWallpaperSetterFactory;
begin
  {$IFDEF LINUX}
  Result := TWpcLinuxWallpaperSetterFactory.Create();
  {$ENDIF}

  {$IFDEF WINDOWS}
  Result := TWpcWindowsWallpaperSetterFactory.Create();
  {$ENDIF}
end;


end.
