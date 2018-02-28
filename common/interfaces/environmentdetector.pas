unit EnvironmentDetector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcDesktopEnvironments;

type

  IWpcEnvironmentDetector = class abstract(TObject)
  public
    function Detect() : TDesktopEnvironment; virtual; abstract;
  end;

implementation


end.

