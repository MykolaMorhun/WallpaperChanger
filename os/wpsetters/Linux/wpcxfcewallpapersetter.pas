unit WpcXfceWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Unix,
  Process,
  StrUtils,
  WpcAbstractWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments;

type

  { TWpcXfceWallpaperSetter }

  {
    To get XFCE version:
      xfce4-about --version

    To set wallpaper in XFCE 4.12 and higer one can use commands:
      xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitor0/workspace0/last-image --set /path/to/image.jpg
      xfconf-query --channel xfce4-desktop --property /backdrop/screen0/monitor0/workspace0/image-style --set 1

    Note, that monitor name could be different, for example monitorLVDS1 or monitorVGA1
  }
  TWpcXfceWallpaperSetter = class(TWpcAbstractWallpaperSetter)
  private const
    LIST_SCREEN_PROPERTIES_QUERY = 'xfconf-query --channel xfce4-desktop --property /backdrop/screen0 --list';
    MONITOR_NAME_PATH_PREFIX = '/backdrop/screen0/';
    SET_WALLPAPER_IMAGE_QUERY_TEMPLATE = 'xfconf-query --channel xfce4-desktop --property /backdrop/screen0/%s/workspace0/last-image --set %s';
    SET_WALLPAPER_STYLE_QUERY_TEMPLATE = 'xfconf-query --channel xfce4-desktop --property /backdrop/screen0/%s/workspace0/image-style --set %s';
  private
    // Names of all monitors registered in xfconf
    FMonitors : TStringList;
  public
    constructor Create();
    destructor Destroy(); override;
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  private
    procedure DetectAllMonitors();

    function WpcWallpaperStyleToXfceWallpaperStyle(Style : TWpcWallpaperStyle) : String;
  end;


implementation

{ TWpcXfceWallpaperSetter }

constructor TWpcXfceWallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_XFCE;
  SupportedStyles := [ CENTERED, TILED, STRETCHED, SCALED, ZOOMED ];
  DefaultWallpaperStyle := ZOOMED;

  FMonitors := nil;
  DetectAllMonitors();
end;

destructor TWpcXfceWallpaperSetter.Destroy();
begin
  if (FMonitors <> nil) then FMonitors.Free();

  inherited Destroy();
end;

{
  Sets given wallpaper and style for all monitors.
}
procedure TWpcXfceWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
var
  Command            : String;
  MonitorName        : String;
  XfceWallpaperStyle : String;
begin
  Validate(Path, Style);

  XfceWallpaperStyle := WpcWallpaperStyleToXfceWallpaperStyle(Style);

  // no-op shell command
  Command := ':';
  for MonitorName in FMonitors do begin
    Command := Command + ' && ' +
      Format(SET_WALLPAPER_IMAGE_QUERY_TEMPLATE, [MonitorName, Path]) + ' && ' +
      Format(SET_WALLPAPER_STYLE_QUERY_TEMPLATE, [MonitorName, XfceWallpaperStyle]);
  end;
  fpSystem(Command);
end;

{
  Gets names of all known to xfconf monitors.
  Result is stored in FMonitors field.
}
procedure TWpcXfceWallpaperSetter.DetectAllMonitors();
var
  ScreenPropertiesQueryOutput : String;
  Offset                      : DWord;
  MonitorNameBeginPos         : DWord;
  MonitorNameEndPos           : DWord;
  MonitorName                 : String;
begin
  if (FMonitors <> nil) then
    FMonitors.Free();
  FMonitors := TStringList.Create();

  // Query xfconf for all screen related properties.
  // Example of output:
  //   /backdrop/screen0/monitorLVDS1/workspace0/color-style
  //   /backdrop/screen0/monitorLVDS1/workspace0/image-style
  //   /backdrop/screen0/monitorLVDS1/workspace0/last-image
  //   /backdrop/screen0/monitorVGA1/workspace0/color-style
  //   /backdrop/screen0/monitorVGA1/workspace0/image-style
  //   /backdrop/screen0/monitorVGA1/workspace0/last-image
  RunCommand('/bin/sh', ['-c', LIST_SCREEN_PROPERTIES_QUERY], ScreenPropertiesQueryOutput, [poWaitOnExit]);

  Offset := 1;
  while True do begin
    Offset := PosEx(MONITOR_NAME_PATH_PREFIX, ScreenPropertiesQueryOutput, Offset);
    if (Offset = 0) then
      // Next occurrance not found, end of output reached.
      break;

    MonitorNameBeginPos := Offset + Length(MONITOR_NAME_PATH_PREFIX);
    MonitorNameEndPos := PosEx('/', ScreenPropertiesQueryOutput, MonitorNameBeginPos);
    MonitorName := Copy(ScreenPropertiesQueryOutput, MonitorNameBeginPos, MonitorNameEndPos - MonitorNameBeginPos);

    // Add monitor name if it hasn't been found before
    if (FMonitors.IndexOf(MonitorName) = -1) then
      FMonitors.Add(MonitorName);

    // Continue search after current occurrence
    Offset := MonitorNameEndPos;
  end;
end;

function TWpcXfceWallpaperSetter.WpcWallpaperStyleToXfceWallpaperStyle(Style : TWpcWallpaperStyle) : String;
begin
  case (Style) of
    CENTERED:  Result := '1';
    TILED:     Result := '2';
    STRETCHED: Result := '3';
    SCALED:    Result := '4';
    ZOOMED:    Result := '5';
    // NONE:      Result := '0';
  end;
end;


end.

