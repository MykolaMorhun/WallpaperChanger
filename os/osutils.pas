unit OSUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TOSFamily = (F_WINDOWS, F_LINUX, F_MAC, F_UNKNOWN);

const
  LINE_BREAK = LineEnding;
  PATH_SEPARATOR = PathDelim;
  OS_FAMILY =
    {$IFDEF LINUX} F_LINUX; {$ENDIF}
    {$IFDEF WINDOWS} F_WINDOWS; {$ENDIF}

  // All known path separators from all supported OS-es
  ALL_PATH_SEPARATORS = [ '/', '\' ];

function IsAbsolutePath(Path : String) : Boolean;
function GetAbsolutePath(Path : String) : String;


implementation


function IsAbsolutePath(Path : String): Boolean;
const
  ALPHA_CHARS = ['A'..'Z', 'a'..'z'];
begin
  if (Path = '') then begin
    Result := False;
    exit;
  end;

  case OS_FAMILY of
    F_LINUX,
    F_MAC:
      Result := Path[1] = '/';
    F_WINDOWS:
      begin
        if (Length(Path) >=3) then
          Result := (Path[1] in ALPHA_CHARS) and (Path[2] = ':') and (Path[3] = PATH_SEPARATOR)
        else
          Result := False;
      end
    else
      Result := False;
  end;
end;

{
  Resolves relative path against executable location.
  Does not modifies path if it is already absolute.
}
function GetAbsolutePath(Path : String) : String;
begin
  if (not IsAbsolutePath(Path)) then
    Path := GetCurrentDir() + PATH_SEPARATOR + Path;

  Result := Path;
end;


end.

