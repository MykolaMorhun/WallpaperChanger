unit WpcApplicationManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcOptions,
  WpcWallpaperChangerAlgorithms,
  WpcDesktopEnvironments,
  WpcWallpaperSetterFactoryProvider,
  WpcWallpaperSetterFactory,
  WpcWallpaperSetter,
  WpcWallpaperStyles,
  WpcEnvironmentDetectorProvider,
  WpcEnvironmentDetector,
  WpcScriptParser,
  WpcScript,
  WpcScriptsGenerator,
  WpcScriptExecutor, WpcInThreadScriptExecutor,
  WpcImage, WpcDirectory,
  WpcExceptions;

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

    FScriptsGenerator : TWpcScriptsGenerator;
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

    procedure SetWallpaper(Image : TWpcImage);
    procedure SetWallpapersFromDirectory(Directory : TWpcDirectory);
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

  FScriptsGenerator := TWpcScriptsGenerator.Create();
end;

destructor TWpcApplicationManager.Destroy();
begin
  // Stop script execution if any
  if (FScriptExecutor.IsRunning()) then
    FScriptExecutor.Terminate();

  FApplicationSettings.Free();
  FApplicationStateSettings.Free();

  FScriptsGenerator.Free();

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

    FApplicationStateSettings.LastType := WPCA_SCRIPT;
    FApplicationStateSettings.LastScript := PathToScript;
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

procedure TWpcApplicationManager.SetWallpaper(Image : TWpcImage);
begin
  if (not FWallpaperSetter.IsWallpaperStyleSupported(FApplicationSettings.WallpaperStyle)) then
    raise TWpcUseErrorException.Create(WallpaperStyleToStr(FApplicationSettings.WallpaperStyle) + ' wallpaper style is not supported by current environment.');
  if (not FWallpaperSetter.IsWallpaperTypeSupported(Image.GetPath())) then
    raise TWpcUseErrorException.Create('Current environment doesn''t support file "' + ExtractFileName(Image.GetPath()) + '" as a wallpaper.');

  FWallpaperSetter.SetDesktopWallpaper(Image.GetPath(), FApplicationSettings.WallpaperStyle);

  FApplicationStateSettings.LastType := WPCA_IMAGE;
  FApplicationStateSettings.LastWallpaper := Image.GetPath();
end;

procedure TWpcApplicationManager.SetWallpapersFromDirectory(Directory : TWpcDirectory);
var
  Script : TWpcScript;
begin
  if (IsScriptRunning()) then
    raise TWpcUseErrorException.Create('Script is alredy running.');

  Script := FScriptsGenerator.GenerateDirectoryStatementScript(Directory, FApplicationSettings);
  FScriptExecutor.RunScript(Script);

  FApplicationStateSettings.LastType := WPCA_DIRECTORY;
  FApplicationStateSettings.LastDirectory := Directory.GetPath();
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

