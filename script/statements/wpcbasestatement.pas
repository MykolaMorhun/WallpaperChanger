unit WpcBaseStatement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcScriptCommons;

type

  { IWpcBaseScriptStatement }

  IWpcBaseScriptStatement = class
  public
    function GetId() : TWpcStatemetId; virtual; abstract;
  end;


implementation


end.

