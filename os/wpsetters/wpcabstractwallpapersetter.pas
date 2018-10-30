unit WpcAbstractWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments,
  WpcExceptions;

type

  { TWpcAbstractWallpaperSetter }

  TWpcAbstractWallpaperSetter = class abstract(IWpcWallpaperSetter)
  protected
    // Following values should be set in descendant class.

    // Desktop environment for which current setter is designed.
    TargetDesktopEnvironment : TDesktopEnvironment;
    // List of supported wallpaper styles.
    SupportedStyles : TWpcSetOfWallpaperStyles;
    // Wallpaper style which will be used in case if given is not supported.
    DefaultWallpaperStyle : TWpcWallpaperStyle;
    // List of images file extensions (lowercase) which could be handled by the setter.
    // Optional. If not set, everything is valid in IsWallpaperTypeSupported
    SupportedImageTypes : TStrings;
  public
    function GetWallpaperStylesSupported() : TWpcSetOfWallpaperStyles; override;
    function IsWallpaperStyleSupported(Style : TWpcWallpaperStyle) : Boolean; override;
    function IsWallpaperTypeSupported(Image : String) : Boolean; override;
    function GetEnvironmet() : TDesktopEnvironment; override;
  protected
    procedure Validate(Path : String; var Style : TWpcWallpaperStyle);
  end;

implementation

{ TWpcAbstractWallpaperSetter }

function TWpcAbstractWallpaperSetter.GetWallpaperStylesSupported() : TWpcSetOfWallpaperStyles;
begin
  Result := SupportedStyles;
end;

function TWpcAbstractWallpaperSetter.IsWallpaperStyleSupported(Style : TWpcWallpaperStyle) : Boolean;
begin
  Result := Style in SupportedStyles;
end;

function TWpcAbstractWallpaperSetter.IsWallpaperTypeSupported(Image : String) : Boolean;
begin
  if (Assigned(SupportedImageTypes)) then
    Result := SupportedImageTypes.IndexOf(LowerCase(ExtractFileExt(Image))) <> -1
  else
    Result := True;
end;

function TWpcAbstractWallpaperSetter.GetEnvironmet() : TDesktopEnvironment;
begin
  Result := TargetDesktopEnvironment;
end;

procedure TWpcAbstractWallpaperSetter.Validate(Path : String; var Style : TWpcWallpaperStyle);
begin
  if (not FileExists(Path)) then
    raise TWpcUseErrorException.Create('Cannot set unexisting image "' + Path + '" as a wallpaper.');

  if (not (Style in SupportedStyles)) then
    Style := DefaultWallpaperStyle;
end;


end.

