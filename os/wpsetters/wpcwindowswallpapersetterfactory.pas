unit WpcWindowsWallpaperSetterFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperSetter,
  WpcWallpaperSetterFactory,
  WpcDesktopEnvironments;

type

  { TWpcWindowsWallpaperSetterFactory }

  TWpcWindowsWallpaperSetterFactory = class(IWpcWallpaperSetterFactory)
  private const
    SUPPORTED_ENVIRONMENTS = [
      DE_WINDOWS_10,
      DE_WINDOWS_8,
      DE_WINDOWS_7,
      // DE_WINDOWS_VISTA,
      DE_WINDOWS_XP,
      DE_REACTOS
    ];
  public
    function GetWallpaperSetter(DesktopEnvironment : TDesktopEnvironment) : IWpcWallpaperSetter; override;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; override;
  end;


implementation

{ TWpcWindowsWallpaperSetterFactory }

function TWpcWindowsWallpaperSetterFactory.GetWallpaperSetter(DesktopEnvironment : TDesktopEnvironment) : IWpcWallpaperSetter;
begin
  case (DesktopEnvironment) of
    DE_WINDOWS_10:    Result := nil;
    DE_WINDOWS_8_1:   Result := nil;
    DE_WINDOWS_8:     Result := nil;
    DE_WINDOWS_7:     Result := nil;
    DE_WINDOWS_VISTA: Result := nil;
    DE_WINDOWS_XP:    Result := nil;
    DE_WINDOWS_2000:  Result := nil;

    DE_REACTOS:       Result := nil;
  end;
end;

function TWpcWindowsWallpaperSetterFactory.GetSupportedEnvironments() : TDesktopEnvironmentsSet;
begin
  Result := SUPPORTED_ENVIRONMENTS;
end;


end.

