unit WpcEnvironmentDetectorProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcEnvironmentDetector,
  {$IFDEF LINUX} WpcLinuxEnvironmentDetector {$ENDIF}
  {$IFDEF WINDOWS} WpcWindowsEnvironmentDetector {$ENDIF}
  ;

function GetEnvironmentDetector() : IWpcEnvironmentDetector;


implementation

function GetEnvironmentDetector() : IWpcEnvironmentDetector;
begin
  {$IFDEF LINUX}
  Result := TWpcLinuxEnvironmentDetector.Create();
  {$ENDIF}

  {$IFDEF WINDOWS}
  Result := TWpcWindowsEnvironmentDetector.Create();
  {$ENDIF}
end;


end.

