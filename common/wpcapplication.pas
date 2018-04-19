unit WpcApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcOptions,
  WpcWallpaperSetterFactory,
  WpcDesktopEnvironments,
  WpcWallpaperSetter, WpcWallpaperSetterFactoryProvider,
  WpcEnvironmentDetector, WpcEnvironmentDetectorProvider,
  WpcScriptParser,
  WpcExceptions;

const
  SETTINGS_FILE = 'WPCSettings.ini';
  STATE_FILE = 'WPCState.ini';

type

  { TWPCApplication }

  // Singleton. Holds application state and modules objects.
  TWpcApplication = class(TObject)
  private
    ApplicationSettings : TWpcPersistentSettings;
    ApplicationStateSettings : TWpcStateSettings;

    EnvironmentDetector : IWpcEnvironmentDetector;
    WallpaperSetter : IWallpaperSetter;
    WallpaperSetterFactory : IWpcWallpaperSetterFactory;
  public
    constructor Create();
    destructor Destroy(); override;
  private
    procedure Initialize();

    procedure UpdateWallpaperSetter(DesktopEnvironment : TDesktopEnvironment);
  end;

implementation

{ TWpcApplication }

constructor TWpcApplication.Create();
begin
  Initialize();
end;

destructor TWpcApplication.Destroy();
begin
  ApplicationSettings.Free();
  ApplicationStateSettings.Free();

  if (WallpaperSetter <> nil) then FreeAndNil(WallpaperSetter);
  WallpaperSetterFactory.Free();
  if (EnvironmentDetector <> nil) then FreeAndNil(EnvironmentDetector);
end;

{
  Loads configs, creates required objects.
}
procedure TWpcApplication.Initialize();
begin
  ApplicationSettings := TWpcPersistentSettings.Create(SETTINGS_FILE);
  ApplicationSettings.ReadFromFile();

  ApplicationStateSettings := TWpcStateSettings.Create(STATE_FILE);
  ApplicationStateSettings.ReadFromFile();

  EnvironmentDetector := GetEnvironmentDetector();
  WallpaperSetterFactory := GetWallpaperSetterFactory();

  UpdateWallpaperSetter(ApplicationSettings.DesktopEnvironment);
end;

procedure TWpcApplication.UpdateWallpaperSetter(DesktopEnvironment : TDesktopEnvironment);
begin
  if (WallpaperSetter <> nil) then FreeAndNil(WallpaperSetter);

  if (DesktopEnvironment = DE_AUTODETECT) then begin
    DesktopEnvironment := EnvironmentDetector.Detect();
  end;

  if (DesktopEnvironment = DE_UNKNOWN) then begin
    // TODO show options dialog to set DE manually
  end;

  WallpaperSetter := WallpaperSetterFactory.GetWallpaperSetter(DesktopEnvironment);
end;


end.

