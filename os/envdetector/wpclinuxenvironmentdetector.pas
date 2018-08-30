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
  public
    function Detect() : TDesktopEnvironment; override;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; override;
  end;


implementation

{ TWpcLinuxEnvironmentDetector }

function TWpcLinuxEnvironmentDetector.Detect(): TDesktopEnvironment;
begin
  // TODO implement
  //Result := DE_UNKNOWN;
  Result := DE_CUSTOM;
end;

function TWpcLinuxEnvironmentDetector.GetSupportedEnvironments(): TDesktopEnvironmentsSet;
begin
  Result := SUPPORTED_ENVIRONMETS;
end;


end.

