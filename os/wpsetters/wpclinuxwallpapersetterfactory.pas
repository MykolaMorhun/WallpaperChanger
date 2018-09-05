unit WpcLinuxWallpaperSetterFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperSetter,
  WpcWallpaperSetterFactory,
  WpcGnome3WallpaperSetter,
  WpcMateWallpaperSetter,
  WpcCinnamonWallpaperSetter,
  WpcXfceWallpaperSetter,
  WpcDesktopEnvironments,
  WpcCustomWallpaperSetter,
  WpcExceptions;

type

  { TWpcLinuxWallpaperSetterFactory }

  TWpcLinuxWallpaperSetterFactory = class(IWpcWallpaperSetterFactory)
  private const
    SUPPORTED_ENVIRONMENTS = [
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
    function GetWallpaperSetter(DesktopEnvironment: TDesktopEnvironment) : IWpcWallpaperSetter; override;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; override;
  end;


implementation

{ TWpcLinuxWallpaperSetterFactory }

function TWpcLinuxWallpaperSetterFactory.GetWallpaperSetter(DesktopEnvironment : TDesktopEnvironment): IWpcWallpaperSetter;
begin
  case (DesktopEnvironment) of
    DE_GNOME:         Result := TWpcGnome3WallpaperSetter.Create();
    DE_MATE:          Result := TWpcMateWallpaperSetter.Create();
    DE_CINNAMON:      Result := TWpcCinnamonWallpaperSetter.Create();
    DE_KDE:           Result := nil;
    DE_XFCE:          Result := TWpcXfceWallpaperSetter.Create();
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
  Result := SUPPORTED_ENVIRONMENTS;
end;


end.

