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
  public
    function Detect() : TDesktopEnvironment; override;
  end;


implementation

{ TWpcWindowsEnvironmentDetector }

function TWpcWindowsEnvironmentDetector.Detect: TDesktopEnvironment;
begin
  // TODO implement
  Result := DE_UNKNOWN;
end;

end.

