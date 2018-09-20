unit WpcWindowsEnvironmentDetector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcDesktopEnvironments,
  WpcEnvironmentDetector;

type

  { TWpcWindowsEnvironmentDetector }

  TWpcWindowsEnvironmentDetector = class(IWpcEnvironmentDetector)
  private const
    SUPPORTED_ENVIRONMETS = [
      DE_WINDOWS_10,
      DE_WINDOWS_8,
      DE_WINDOWS_7,
      // DE_WINDOWS_VISTA,
      DE_WINDOWS_XP,
      DE_REACTOS
    ];


  public
    function Detect() : TDesktopEnvironment; override;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; override;
  end;


implementation

{ TWpcWindowsEnvironmentDetector }

function TWpcWindowsEnvironmentDetector.Detect() : TDesktopEnvironment;
begin
  // TODO implement
  Result := DE_UNKNOWN;
end;

function TWpcWindowsEnvironmentDetector.GetSupportedEnvironments() : TDesktopEnvironmentsSet;
begin
  Result := SUPPORTED_ENVIRONMETS;
end;


end.

