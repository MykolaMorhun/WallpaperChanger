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
  WpcScriptExecutor, WpcInThreadScriptExecutor;

const
  SETTINGS_FILE = 'WPCSettings.ini';
  STATE_FILE = 'WPCState.ini';

type

  { TWpcApplicationManager }

  // Singleton. Holds application state and modules objects.
  TWpcApplicationManager = class(TObject)
  private
    FApplicationSettings      : TWpcPersistentSettings;
    FApplicationStateSettings : TWpcStateSettings;

    FEnvironmentDetector    : IWpcEnvironmentDetector;
    FWallpaperSetter        : IWallpaperSetter;
    FWallpaperSetterFactory : IWpcWallpaperSetterFactory;

    FScriptExecutor : IWpcScriptExecutor;

    // Current script data
    FScriptContent : TStringList;
    FScriptParser  : TWpcScriptParser;
    FScript        : TWpcScript;
  public
    constructor Create();
    destructor Destroy(); override;
  public
    // Supposed to be changed only from options window or ApplicationManager.
    property CurrentSettings : TWpcPersistentSettings read FApplicationSettings;
    property CurrentState : TWpcStateSettings read FApplicationStateSettings;
  public
    procedure ApplySettings();

    procedure RunScript(PathToScript : String);
    procedure StopScript();
    function IsScriptRunning() : Boolean;
  private
    procedure OnScriptStoppedCallback();
  private
    procedure ReadSettings();
    procedure UpdateWallpaperSetter(DesktopEnvironment : TDesktopEnvironment);
  end;


implementation

{ TWpcApplicationManager }

constructor TWpcApplicationManager.Create();
begin
  ReadSettings();

  FEnvironmentDetector := GetEnvironmentDetector();
  FWallpaperSetterFactory := GetWallpaperSetterFactory();
end;

destructor TWpcApplicationManager.Destroy();
begin
  // Stop script execution if any
  if (FScriptExecutor.IsRunning()) then
    FScriptExecutor.Terminate();

  FApplicationSettings.Free();
  FApplicationStateSettings.Free();

  if (FWallpaperSetter <> nil) then FreeAndNil(FWallpaperSetter);
  FWallpaperSetterFactory.Free();
  if (FEnvironmentDetector <> nil) then FreeAndNil(FEnvironmentDetector);
end;

{
  Loads configs.
}
procedure TWpcApplicationManager.ReadSettings();
begin
  FApplicationSettings := TWpcPersistentSettings.Create(SETTINGS_FILE);
  FApplicationSettings.ReadFromFile();

  FApplicationStateSettings := TWpcStateSettings.Create(STATE_FILE);
  FApplicationStateSettings.ReadFromFile();
end;

{
  Applays settings from ApplicationSettings object.
  Should be invoked for full initialization of the application.
}
procedure TWpcApplicationManager.ApplySettings();
begin
  UpdateWallpaperSetter(CurrentSettings.DesktopEnvironment);
  FScriptExecutor := TWpcInThreadScriptExecutor.Create(FWallpaperSetter);
  FScriptExecutor.SetOnStopCallback(@OnScriptStoppedCallback);
end;

procedure TWpcApplicationManager.RunScript(PathToScript: String);
begin
  try
    FScriptContent := TStringList.Create();
    // TODO check file type
    FScriptContent.LoadFromFile(PathToScript);
    FScriptParser := TWpcScriptParser.Create(FScriptContent);
    FScript := FScriptParser.Parse();
    FScriptExecutor.RunScript(FScript);
  except
    on E: Exception do begin
      if (FScript <> nil) then FreeAndNil(FScript);
      if (FScriptParser <> nil) then FreeAndNil(FScriptParser);
      FreeAndNil(FScriptContent);
      raise;
    end;
  end;
end;

procedure TWpcApplicationManager.StopScript();
begin
  FScriptExecutor.Terminate();
end;

function TWpcApplicationManager.IsScriptRunning() : Boolean;
begin
  Result := FScriptExecutor.IsRunning();
end;

procedure TWpcApplicationManager.OnScriptStoppedCallback();
begin
  if (FScript <> nil) then FreeAndNil(FScript);
  if (FScriptParser <> nil) then FreeAndNil(FScriptParser);
  if (FScriptContent <> nil) then FreeAndNil(FScriptContent);
end;

{
  Updates wallpaper setter according to given environment.
}
procedure TWpcApplicationManager.UpdateWallpaperSetter(DesktopEnvironment : TDesktopEnvironment);
begin
  if (FWallpaperSetter <> nil) then FreeAndNil(FWallpaperSetter);

  if (DesktopEnvironment = DE_AUTODETECT) then begin
    DesktopEnvironment := FEnvironmentDetector.Detect();
  end;

  if (DesktopEnvironment = DE_UNKNOWN) then begin
    // TODO show options dialog to set DE manually
  end;

  FWallpaperSetter := FWallpaperSetterFactory.GetWallpaperSetter(DesktopEnvironment);
end;


end.

