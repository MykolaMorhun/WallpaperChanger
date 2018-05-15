unit WpcApplicationManager;

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
  WpcScript,
  WpcScriptExecutor;

const
  SETTINGS_FILE = 'WPCSettings.ini';
  STATE_FILE = 'WPCState.ini';

type

  { TWpcApplicationManager }

  // Singleton. Holds application state and modules objects.
  TWpcApplicationManager = class(TObject)
  private
    ApplicationSettings : TWpcPersistentSettings;
    ApplicationStateSettings : TWpcStateSettings;

    EnvironmentDetector : IWpcEnvironmentDetector;
    WallpaperSetter : IWallpaperSetter;
    WallpaperSetterFactory : IWpcWallpaperSetterFactory;
  public
    constructor Create();
    destructor Destroy(); override;
  public
    // Supposed to be changed only from options window or ApplicationManager.
    property CurrentSettings : TWpcPersistentSettings read ApplicationSettings;
    property CurrentState : TWpcStateSettings read ApplicationStateSettings;
  public
    procedure ApplySettings();

    procedure RunScript(PathToScript : String);
  private
    procedure ReadSettings();
  public
    procedure UpdateWallpaperSetter(DesktopEnvironment : TDesktopEnvironment);
  end;


implementation

{ TWpcApplicationManager }

constructor TWpcApplicationManager.Create();
begin
  ReadSettings();

  EnvironmentDetector := GetEnvironmentDetector();
  WallpaperSetterFactory := GetWallpaperSetterFactory();
end;

destructor TWpcApplicationManager.Destroy();
begin
  ApplicationSettings.Free();
  ApplicationStateSettings.Free();

  if (WallpaperSetter <> nil) then FreeAndNil(WallpaperSetter);
  WallpaperSetterFactory.Free();
  if (EnvironmentDetector <> nil) then FreeAndNil(EnvironmentDetector);
end;

{
  Loads configs.
}
procedure TWpcApplicationManager.ReadSettings();
begin
  ApplicationSettings := TWpcPersistentSettings.Create(SETTINGS_FILE);
  ApplicationSettings.ReadFromFile();

  ApplicationStateSettings := TWpcStateSettings.Create(STATE_FILE);
  ApplicationStateSettings.ReadFromFile();
end;

{
  Applays settings from ApplicationSettings object.
  Should be invoked for full initialization of the application.
}
procedure TWpcApplicationManager.ApplySettings();
begin
  UpdateWallpaperSetter(CurrentSettings.DesktopEnvironment);
end;

procedure TWpcApplicationManager.RunScript(PathToScript: String);
var
  ScriptContent  : TStringList;
  ScriptParser   : TWpcScriptParser;
  Script         : TWpcScript;
  ScriptExecutor : TWpcScriptExecutor;
begin
  // TODO start a new thread
  try
    ScriptContent := TStringList.Create();
    // TODO check file type
    ScriptContent.LoadFromFile(PathToScript);
    ScriptParser := TWpcScriptParser.Create(ScriptContent);
    Script := ScriptParser.Parse();
    ScriptExecutor := TWpcScriptExecutor.Create(Script, WallpaperSetter);
    ScriptExecutor.ExecuteScript();
  finally
    if (ScriptParser <> nil) then ScriptParser.Free();
    if (ScriptExecutor <> nil) then ScriptExecutor.Free();
    if (WallpaperSetter <> nil) then WallpaperSetter.Free();
    ScriptContent.Free();
  end;
end;

{
  Updates wallpaper setter according to given environment.
}
procedure TWpcApplicationManager.UpdateWallpaperSetter(DesktopEnvironment : TDesktopEnvironment);
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

