unit WpcExceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
  public
    property Line      : Integer read FLine;
    property WordNumer : Integer read FWordNumber;
    property Message   : String  read FMessage;
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

end.

