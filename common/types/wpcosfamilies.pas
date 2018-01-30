unit WpcOSFamilies;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  OSF_WINDOWS_ID = 'WINDOWS';
  OSF_LINUX_ID = 'LINUX';
  OSF_UNKNOWN_ID = 'UNKNOWN';

type

  TOsFamily = (OSF_WINDOWS, OSF_LINUX, OSF_UNKNOWN);

function OsFamilyToStr(OsFamily : TOsFamily) : String;
function StrToOsFamily(OsFamily : String) : TOsFamily;

implementation

function OsFamilyToStr(OsFamily : TOsFamily) : String;
begin
  case (OsFamily) of
    OSF_WINDOWS: Result := OSF_WINDOWS_ID;
    OSF_LINUX:   Result := OSF_LINUX_ID;
    OSF_UNKNOWN: Result := OSF_UNKNOWN_ID;
  end;
end;

function StrToOsFamily(OsFamily : String) : TOsFamily;
begin
  case (UpperCase(OsFamily)) of
    OSF_WINDOWS_ID: Result := OSF_WINDOWS;
    OSF_LINUX_ID:   Result := OSF_LINUX;
    else
      Result := OSF_UNKNOWN;
  end;
end;


end.

