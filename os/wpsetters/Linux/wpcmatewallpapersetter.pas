unit WpcMateWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractLinuxWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments,
  Process,
  Unix;

type

  { TWpcMateWallpaperSetter }

  TWpcMateWallpaperSetter = class(TWpcAbstractLinuxWallpaperSetter)
  private const
    // Newer versions
    SET_WALLPAPER_COMMAND_TEMPLATE = 'gsettings set org.mate.desktop.background picture-filename ';
    SET_WALLPAPER_STYLE_COMMAND_TEMPLATE = 'gsettings set org.mate.desktop.background picture-options ';

    // Older versions
    SET_WALLPAPER_COMMAND_TEMPLATE_OLD = 'gsettings set org.mate.background picture-filename ';
    SET_WALLPAPER_STYLE_COMMAND_TEMPLATE_OLD = 'gsettings set org.mate.background picture-options ';
  private
    SetWallpaperCommandTemplate : String;
    SetWallpaperStyleCommandTemplate : String;
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  private
    procedure DetectScheme(); inline;

    function WpcWallpaperStyleToMateWallpaperStyle(Style : TWpcWallpaperStyle) : String;
  end;


implementation

constructor TWpcMateWallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_MATE;
  SupportedStyles := [ CENTERED, SCALED, STRETCHED, SPANNED, ZOOMED, TILED ];
  DefaultWallpaperStyle := ZOOMED;

  DetectScheme();
end;

procedure TWpcMateWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
begin
  Validate(Path, Style);

  fpSystem(SetWallpaperCommandTemplate + '"' + Path + '" && ' +
           SetWallpaperStyleCommandTemplate + WpcWallpaperStyleToMateWallpaperStyle(Style));
end;

{
  Sets command template for current Mate version.
}
procedure TWpcMateWallpaperSetter.DetectScheme();
var
  Output : String;
begin
  RunCommand('/bin/sh', ['-c',
             'gsettings', 'get', 'org.mate.desktop.background', 'picture-filename'],
             Output,
             [ poWaitOnExit ]);
  if (Output.IndexOf('No such schema') <> -1) then begin
    // New schema
    SetWallpaperCommandTemplate := SET_WALLPAPER_COMMAND_TEMPLATE;
    SetWallpaperStyleCommandTemplate := SET_WALLPAPER_STYLE_COMMAND_TEMPLATE;
  end
  else begin
    // Old schema
    SetWallpaperCommandTemplate := SET_WALLPAPER_COMMAND_TEMPLATE_OLD;
    SetWallpaperStyleCommandTemplate := SET_WALLPAPER_STYLE_COMMAND_TEMPLATE_OLD;
  end;
end;

function TWpcMateWallpaperSetter.WpcWallpaperStyleToMateWallpaperStyle(Style : TWpcWallpaperStyle): String;
begin
  case (Style) of
    SCALED:    Result := 'scaled';
    SPANNED:   Result := 'spanned';
    ZOOMED:    Result := 'zoom';
    TILED:     Result := 'wallpaper';
    CENTERED:  Result := 'centered';
    STRETCHED: Result := 'stretched';
  end;
end;


end.

