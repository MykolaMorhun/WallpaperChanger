unit WpcExceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  OSUtils;

type

  TWpcException = class(Exception);

  TWpcIllegalArgumentException = class(TWpcException);

  TWpcUseErrorException = class(TWpcException);

  TWpcRuntimeException = class(TWpcException);

  { TWpcScriptParseException }

  TWpcScriptParseException = class(TWpcException)
  public const
    UNKNOWN_LINE = -1;
    UNKNOWN_WORD_NUMBER = -2;
  private
    FLine       : Integer;
    FWordNumber : Integer;
    FMessage    : String;
  private
    function GetPrettyMessage() : String;
  public
    property Line          : Integer read FLine;
    property WordNumer     : Integer read FWordNumber;
    property Message       : String  read FMessage;
    property PrettyMessage : String  read GetPrettyMessage;
  public
    constructor Create(ErrMessage: String);
    constructor Create(ErrMessage : String; ErrLine : Integer);
    constructor Create(ErrMessage : String; ErrLine : Integer; ErrWordNumber : Integer);
  end;


implementation


{ TWpcScriptParseException }

constructor TWpcScriptParseException.Create(ErrMessage: String);
begin
  FLine := UNKNOWN_LINE;
  FWordNumber:= UNKNOWN_WORD_NUMBER;
  FMessage := ErrMessage;
end;

constructor TWpcScriptParseException.Create(ErrMessage: String; ErrLine: Integer);
begin
  FLine := ErrLine;
  FWordNumber := UNKNOWN_WORD_NUMBER;
  FMessage := ErrMessage;
end;

constructor TWpcScriptParseException.Create(ErrMessage: String; ErrLine: Integer; ErrWordNumber : Integer);
begin
  FLine := ErrLine;
  FWordNumber := ErrWordNumber;
  FMessage := ErrMessage;
end;

function TWpcScriptParseException.GetPrettyMessage(): String;
begin
  Result := Concat('Failed to parse script: ', LINE_BREAK, FMessage);
  if (FLine <> UNKNOWN_LINE) then
    Result := Concat(Result, LINE_BREAK, 'Line: ', IntToStr(FLine + 1));
  if (FWordNumber <> UNKNOWN_WORD_NUMBER) then
    Result := Concat(Result, ' word: ', IntToStr(FWordNumber + 1));
end;


end.

