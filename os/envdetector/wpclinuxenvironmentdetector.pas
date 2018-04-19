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
  public
    function Detect() : TDesktopEnvironment; override;
  end;


implementation

{ TWpcLinuxEnvironmentDetector }

function TWpcLinuxEnvironmentDetector.Detect(): TDesktopEnvironment;
begin
  // TODO implement
  // Result := DE_UNKNOWN;
  Result := DE_CUSTOM;
end;

end.

