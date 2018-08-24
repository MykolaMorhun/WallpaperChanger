unit WpcLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  IWpcLogger = class abstract(TObject)
    // Logs given message.
    // If AddLineBreak is true then end of line symbol will be added automatically to the message.
    procedure LogMessage(Message : String; AddLineBreak : Boolean = true); virtual; abstract;
  end;

implementation

end.

