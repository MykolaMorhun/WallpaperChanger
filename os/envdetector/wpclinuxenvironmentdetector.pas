unit WpcLinuxEnvironmentDetector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcDesktopEnvironments,
  WpcEnvironmentDetector;

type

  { TWpcLinuxEnvironmentDetector }

  TWpcLinuxEnvironmentDetector = class(IWpcEnvironmentDetector)
  private const
    SUPPORTED_ENVIRONMETS = [
      DE_GNOME,
      DE_GNOME_CLASSIC,
      DE_MATE,
      DE_CINNAMON,
      DE_KDE,
      DE_XFCE,
      DE_LXDE
      // DE_LXQT,
      // DE_UNITY,
      // DE_PANTHEON,
      // DE_ENLIGHTENMENT,
      // DE_BUDGIE,
      // DE_DEEPIN
    ];
  private const
    XDG_CURRENT_DESKTOP_ENV_VARIABLE_KEY = 'XDG_CURRENT_DESKTOP';

    XDG_DESKTOP_GNOME = 'GNOME';
    XDG_DESKTOP_GNOME_CLASSIC = 'GNOME-CLASSIC:GNOME';
    XDG_DESKTOP_MATE = 'MATE';
    XDG_DESKTOP_CINNAMON = 'X-CINNAMON';
    XDG_DESKTOP_KDE = 'KDE';
    XDG_DESKTOP_XFCE = 'XFCE';
    XDG_DESKTOP_LXDE = 'LXDE';
  private
    FXDGCurrentDesktop : String;
  public
    constructor Create();
  public
    function Detect() : TDesktopEnvironment; override;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; override;
  end;


implementation

{ TWpcLinuxEnvironmentDetector }

constructor TWpcLinuxEnvironmentDetector.Create();
begin
  FXDGCurrentDesktop := UpperCase(GetEnvironmentVariable(XDG_CURRENT_DESKTOP_ENV_VARIABLE_KEY));
end;

function TWpcLinuxEnvironmentDetector.Detect(): TDesktopEnvironment;
begin
  case (FXDGCurrentDesktop) of
    XDG_DESKTOP_GNOME:
      Result := DE_GNOME;
    XDG_DESKTOP_GNOME_CLASSIC:
      Result := DE_GNOME_CLASSIC;
    XDG_DESKTOP_MATE:
      Result := DE_MATE;
    XDG_DESKTOP_CINNAMON:
      Result := DE_CINNAMON;
    XDG_DESKTOP_KDE:
      Result := DE_KDE;
    XDG_DESKTOP_XFCE:
      Result := DE_XFCE;
    XDG_DESKTOP_LXDE:
      Result := DE_LXDE;
    else
      Result := DE_UNKNOWN;
  end;
end;

function TWpcLinuxEnvironmentDetector.GetSupportedEnvironments(): TDesktopEnvironmentsSet;
begin
  Result := SUPPORTED_ENVIRONMETS;
end;


end.

