unit WpcKdePlasmaWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractWallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments,
  Unix;

type

  { TWpcKdePlasmaWallpaperSetter }

  (*
    KDE allows wallpaper changing via KDE shell scripts.
    For more information see docs:
      https://userbase.kde.org/KDE_System_Administration/PlasmaDesktopScripting

    To play with scripts use KDE desktop shell scripting console. Execute from terminal:
      qdbus org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.showInteractiveConsole

    For working with wallpapers, wallpaper plugins should be used.
    Following script will show available wallpaper plugins:
      for (var plugin in knownWallpaperPlugins()) {
        print(JSON.stringify(plugin));
      }

    Simple wallpaper setter script:
      desktops().forEach(function(desktop) {
        // print(JSON.stringify(desktop));
        desktop.wallpaperPlugin = "org.kde.image";
        desktop.currentConfigGroup = Array("Wallpaper", "General");
        desktop.writeConfig("Image", "file:///path/to/image.jpg");
      });
    However, I failed to find a way to change wallpaper style, so style parameter
    is ignored in this setter, instead style which is set via UI will be used.
    TODO implement respection of wallpaper style.

    Inline script might be invoked via qdbus:
      qdbus org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.evaluateScript 'SCRIPT_HERE'
    or via dbus:
      dbus-send --session --dest=org.kde.plasmashell --type=method_call /PlasmaShell org.kde.PlasmaShell.evaluateScript 'string: SCRIPT_HERE'
  *)
  TWpcKdePlasmaWallpaperSetter = class(TWpcAbstractWallpaperSetter)
  private const
    SET_WALLPAPER_SCRIPT_TEMPLATE =
      'qdbus org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.evaluateScript ''' +
        'desktops().forEach(function(desktop) { ' +
          'desktop.wallpaperPlugin = "org.kde.image";' +
          'desktop.currentConfigGroup = Array("Wallpaper", "General");' +
          'desktop.writeConfig("Image", "file://%s");' +
        '});' +
      '''';
  public
    constructor Create();
  public
    procedure SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle); override;
  private
    function WpcWallpaperStyleToKdePlasmaWallpaperStyle(Style : TWpcWallpaperStyle) : String;
  end;

implementation

{ TWpcKdePlasmaWallpaperSetter }

constructor TWpcKdePlasmaWallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_KDE;
  SupportedStyles := [ ZOOMED, STRETCHED, SCALED, CENTERED, TILED ];
  DefaultWallpaperStyle := ZOOMED;
end;

procedure TWpcKdePlasmaWallpaperSetter.SetDesktopWallpaper(Path : String; Style : TWpcWallpaperStyle);
begin
  Validate(Path, Style);

  fpSystem(Format(SET_WALLPAPER_SCRIPT_TEMPLATE, [Path]));
end;

function TWpcKdePlasmaWallpaperSetter.WpcWallpaperStyleToKdePlasmaWallpaperStyle(Style : TWpcWallpaperStyle) : String;
begin
  // TODO implement
end;


end.

