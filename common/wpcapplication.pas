unit WpcApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcOptions,
  WpcWallpaperSetterFactory,
  WpcWallpaperSetter,
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

    WallpaperSetter : IWallpaperSetter;
    WallpaperSetterFactory : IWpcWallpaperSetterFactory;

    EnvironmentDetector : IWpcEnvironmentDetector;
  public
    constructor Create();
    destructor Destroy(); override;
  private
    procedure Initialize();
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

  EnvironmentDetector.Free();
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
end;


end.

