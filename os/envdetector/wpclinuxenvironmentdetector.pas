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
      DE_LXDE,
      // DE_LXQT,
      DE_UNITY,
      DE_PANTHEON,
      // DE_ENLIGHTENMENT,
      DE_BUDGIE
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
    XDG_DESKTOP_PANTHEON = 'PANTHEON';
    XDG_DESKTOP_BUDGIE = 'BUDGIE:GNOME';
    XDG_DESKTOP_UNITY = 'UNITY';
  private
    {
      Some examples of XDG_CURRENT_DESKTOP values:
        XFCE
        X-Cinnamon
        ubuntu:GNOME
        Budgie:GNOME
        GNOME-Classic:GNOME
        Unity:Unity7:ubuntu
    }
    FXDGCurrentDesktop : String;
  public
    constructor Create();
  public
    function Detect() : TDesktopEnvironment; override;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; override;
  private
    function GetEnvironmentByPartialMatch(DesktopEnvironmentData : String) : TDesktopEnvironment;
  end;


implementation

{ TWpcLinuxEnvironmentDetector }

constructor TWpcLinuxEnvironmentDetector.Create();
begin
  FXDGCurrentDesktop := UpperCase(GetEnvironmentVariable(XDG_CURRENT_DESKTOP_ENV_VARIABLE_KEY));
end;

function TWpcLinuxEnvironmentDetector.Detect() : TDesktopEnvironment;
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
    XDG_DESKTOP_PANTHEON:
      Result := DE_PANTHEON;
    XDG_DESKTOP_BUDGIE:
      Result := DE_BUDGIE;
    XDG_DESKTOP_UNITY:
      Result := DE_UNITY;
    else begin
      Result := GetEnvironmentByPartialMatch(FXDGCurrentDesktop);
    end;
  end;
end;

function TWpcLinuxEnvironmentDetector.GetSupportedEnvironments() : TDesktopEnvironmentsSet;
begin
  Result := SUPPORTED_ENVIRONMETS;
end;

{
  Searches for desktop environment name in given string.
  If nothing matches DE_UNKNOWN will be returned.
}
function TWpcLinuxEnvironmentDetector.GetEnvironmentByPartialMatch(DesktopEnvironmentData : String): TDesktopEnvironment;
var
  s : String;
begin
  s := UpperCase(DesktopEnvironmentData);

  if (Pos(XDG_DESKTOP_GNOME, s) <> 0) then begin
    if (Pos(XDG_DESKTOP_GNOME_CLASSIC, s) <> 0) then
      Result := DE_GNOME_CLASSIC
    else
      Result := DE_GNOME;
  end
  else if (Pos(XDG_DESKTOP_MATE, s) <> 0) then
    Result := DE_MATE
  else if (Pos(XDG_DESKTOP_CINNAMON, s) <> 0) then
    Result := DE_CINNAMON
  else if (Pos(XDG_DESKTOP_KDE, s) <> 0) then
    Result := DE_KDE
  else if (Pos(XDG_DESKTOP_XFCE, s) <> 0) then
    Result := DE_XFCE
  else if (Pos(XDG_DESKTOP_LXDE, s) <> 0) then
    Result := DE_LXDE
  else if (Pos(XDG_DESKTOP_PANTHEON, s) <> 0) then
    Result := DE_PANTHEON
  else if (Pos(XDG_DESKTOP_BUDGIE, s) <> 0) then
    Result := DE_BUDGIE
  else if (Pos(XDG_DESKTOP_UNITY, s) <> 0) then
    Result := DE_UNITY
  else
    Result := DE_UNKNOWN;
end;


end.

