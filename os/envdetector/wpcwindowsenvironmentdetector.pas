unit WpcWindowsEnvironmentDetector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcDesktopEnvironments,
  WpcEnvironmentDetector,
  Windows;

type

  { TWpcWindowsEnvironmentDetector }

  TWpcWindowsEnvironmentDetector = class(IWpcEnvironmentDetector)
  private const
    SUPPORTED_ENVIRONMETS = [
      DE_WINDOWS_10,
      DE_WINDOWS_8_1,
      DE_WINDOWS_8,
      DE_WINDOWS_7,
      // DE_WINDOWS_VISTA,
      DE_WINDOWS_XP
      // DE_WINDOWS_2000,
      // DE_REACTOS
    ];
  public
    function Detect() : TDesktopEnvironment; override;
    function GetSupportedEnvironments() : TDesktopEnvironmentsSet; override;
  end;


implementation

{ TWpcWindowsEnvironmentDetector }

function TWpcWindowsEnvironmentDetector.Detect() : TDesktopEnvironment;
var
  WindowsVersionInfo : TOSVersionInfo;
begin
  WindowsVersionInfo.dwOSVersionInfoSize := SizeOf(WindowsVersionInfo);
  GetVersionEx(WindowsVersionInfo);

  if (WindowsVersionInfo.dwPlatformID = VER_PLATFORM_WIN32_NT) then begin
    case (WindowsVersionInfo.dwMajorVersion) of
      10:
        Result := DE_WINDOWS_10;
      6:
        case (WindowsVersionInfo.dwMinorVersion) of
          3:
            Result := DE_WINDOWS_8_1;
          2:
            Result := DE_WINDOWS_8;
          1:
            Result := DE_WINDOWS_7;
          0:
            Result := DE_WINDOWS_VISTA;
          else
            Result := DE_UNKNOWN;
        end;
      5:
        case (WindowsVersionInfo.dwMinorVersion) of
          2:
            Result := DE_WINDOWS_XP; // 64 bit or server
          1:
            Result := DE_WINDOWS_XP;
          0:
            Result := DE_WINDOWS_2000;
          else
            Result := DE_UNKNOWN;
        end;
      else
        Result := DE_UNKNOWN;
    end;
  end
  else
    Result := DE_UNKNOWN;
end;

function TWpcWindowsEnvironmentDetector.GetSupportedEnvironments() : TDesktopEnvironmentsSet;
begin
  Result := SUPPORTED_ENVIRONMETS;
end;


end.

