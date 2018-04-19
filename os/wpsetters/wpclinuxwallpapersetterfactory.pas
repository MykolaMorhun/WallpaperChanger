unit WpcLinuxWallpaperSetterFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperSetter,
  WpcWallpaperSetterFactory,
  WpcDesktopEnvironments,
  WpcCustomWallpaperSetter,
  WpcExceptions;

type

  { TWpcLinuxWallpaperSetterFactory }

  TWpcLinuxWallpaperSetterFactory = class(IWpcWallpaperSetterFactory)
  private
    const SupportedEnvironments = [
      DE_GNOME,
      DE_MATE,
      DE_CINNAMON,
      DE_KDE,
      DE_XFCE,
      DE_LXDE,
      DE_LXQT,
      DE_UNITY,
      DE_PANTHEON,
      DE_ENLIGHTENMENT,
      DE_BUDGIE,
      DE_DEEPIN
    ];
  public
    function GetWallpaperSetter(DesktopEnvironment: TDesktopEnvironment) : IWallpaperSetter; override;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; override;
  end;


implementation

{ TWpcLinuxWallpaperSetterFactory }

function TWpcLinuxWallpaperSetterFactory.GetWallpaperSetter(DesktopEnvironment : TDesktopEnvironment): IWallpaperSetter;
begin
  case (DesktopEnvironment) of
    DE_GNOME:         Result := nil;
    DE_MATE:          Result := nil;
    DE_CINNAMON:      Result := nil;
    DE_KDE:           Result := nil;
    DE_XFCE:          Result := nil;
    DE_LXDE:          Result := nil;
    DE_LXQT:          Result := nil;
    DE_UNITY:         Result := nil;
    DE_PANTHEON:      Result := nil;
    DE_ENLIGHTENMENT: Result := nil;
    DE_BUDGIE:        Result := nil;
    DE_DEEPIN:        Result := nil;

    DE_CUSTOM:        Result := TWpcCustomWallpaperSetter.Create();
  else
    raise TWpcIllegalArgumentException('Unsupported Desktop Environmet: ' + DesktopEnvironmentToStr(DesktopEnvironment));
  end;
end;

function TWpcLinuxWallpaperSetterFactory.GetSupportedEnvironments() : TDesktopEnvironmentsSet;
begin
  Result := SupportedEnvironments;
end;


end.

