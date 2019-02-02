unit WpcWallpaperStatement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcBaseStatement,
  WpcStatementProperties,
  WpcScriptCommons,
  WpcImage,
  WpcWallpaperStyles,
  WpcExceptions;

type

  { TWpcWallpaperStatement }

  TWpcWallpaperStatement = class(IWpcBaseScriptStatement)
  private
    FImage       : TWpcImage;
    FStyle       : TWpcWallpaperStyle;
    FDelay       : TWpcDelayStatementProperty;
    FProbability : TWpcProbabilityStatementProperty;
  public
    constructor Create(Image : TWpcImage);
    destructor Destroy(); override;
  public
    procedure SetImage(Image : TWpcImage);
    function GetImage() : TWpcImage;
    procedure SetStyle(Style : TWpcWallpaperStyle);
    function GetStyle() : TWpcWallpaperStyle;
    procedure SetDelay(Delay : LongWord; IsStatic : Boolean = True);
    function GetDelay() : LongWord;
    function GetOriginalDelayValue() : LongWord;
    function IsDelayStatic() : Boolean;
    procedure SetProbability(Probability : Byte);
    function GetProbability() : Byte;

    function GetId() : TWpcStatemetId; override;
  end;


implementation

{ TWpcWallpaperStatement }

function TWpcWallpaperStatement.GetId() : TWpcStatemetId;
begin
  Result := WPC_WALLPAPER_STATEMENT_ID;
end;

constructor TWpcWallpaperStatement.Create(Image : TWpcImage);
begin
  if (Image = nil) then
    raise TWpcIllegalArgumentException.Create('Wallpaper image is mandatory.');
  FImage := Image;
  FDelay := TWpcDelayStatementProperty.Create();
  FProbability := TWpcProbabilityStatementProperty.Create();
end;

destructor TWpcWallpaperStatement.Destroy();
begin
  FImage.Free();
  FDelay.Free();
  FProbability.Free();
  inherited Destroy();
end;

procedure TWpcWallpaperStatement.SetImage(Image : TWpcImage);
begin
  if (Image <> nil) then
    FImage := Image;
end;

function TWpcWallpaperStatement.GetImage() : TWpcImage;
begin
  Result := FImage;
end;

procedure TWpcWallpaperStatement.SetStyle(Style : TWpcWallpaperStyle);
begin
  FStyle := Style;
end;

function TWpcWallpaperStatement.GetStyle() : TWpcWallpaperStyle;
begin
  Result := FStyle;
end;

procedure TWpcWallpaperStatement.SetDelay(Delay : LongWord; IsStatic : Boolean);
begin
  FDelay.Delay := Delay;
  FDelay.IsStatic := IsStatic;
end;

function TWpcWallpaperStatement.GetDelay() : LongWord;
begin
  Result := FDelay.Delay;
end;

function TWpcWallpaperStatement.GetOriginalDelayValue() : LongWord;
begin
  Result := FDelay.HoldingValue;
end;

function TWpcWallpaperStatement.IsDelayStatic(): Boolean;
begin
  Result := FDelay.IsStatic;
end;

procedure TWpcWallpaperStatement.SetProbability(Probability : Byte);
begin
  FProbability.Probability := Probability;
end;

function TWpcWallpaperStatement.GetProbability() : Byte;
begin
  Result := FProbability.Probability;
end;


end.

