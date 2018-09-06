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
  WpcExceptions,

  WpcOptionsForm,
  WpcScriptEditorForm,
  WpcAboutForm;

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
    FWallpaperSetter        : IWpcWallpaperSetter;
    FWallpaperSetterFactory : IWpcWallpaperSetterFactory;

    FScriptsGenerator : TWpcScriptsGenerator;
    FScriptExecutor : IWpcScriptExecutor;

    // Current script data
    FScriptContent : TStringList;
    FScriptParser  : TWpcScriptParser;
    FScript        : TWpcScript;
  private
    FOptionsWindow : TOptionsForm;
    FScriptEditorWindow : TScriptEditorForm;
  public
    constructor Create();
    destructor Destroy(); override;
  public
    // Supposed to be changed only from options window or ApplicationManager.
    property CurrentSettings : TWpcPersistentSettings read FApplicationSettings;
    property CurrentState : TWpcStateSettings read FApplicationStateSettings;

    property EnvironmentDetector : IWpcEnvironmentDetector read FEnvironmentDetector;
    property WallpaperSetter : IWpcWallpaperSetter read FWallpaperSetter;
    property WallpaperSetterFactory : IWpcWallpaperSetterFactory read FWallpaperSetterFactory;
  public
    procedure ApplySettings();

    procedure RunScript(PathToScript : String);
    procedure StopScript();
    function IsScriptRunning() : Boolean; inline;

    procedure SetWallpaper(Image : TWpcImage);
    procedure SetWallpapersFromDirectory(Directory : TWpcDirectory);

    procedure SetNextWallpaper();
  public
    procedure OpenOptionsForm(ForceSetEnvironment : Boolean = False);
    procedure OpenScriptEditorForm();
    procedure OpenAboutForm();
  private
    procedure OnScriptStoppedCallback(ExitStatus : TWpcScriptExecutionExitStatus);
  private
    procedure ReadSettings();
  end;


implementation

{ TWpcApplicationManager }

constructor TWpcApplicationManager.Create();
begin
  ReadSettings();

  FEnvironmentDetector := GetEnvironmentDetector();
  FWallpaperSetterFactory := GetWallpaperSetterFactory();

  FScriptsGenerator := TWpcScriptsGenerator.Create();

  FOptionsWindow := nil;
  FScriptEditorWindow := nil;
end;

destructor TWpcApplicationManager.Destroy();
begin
  if (FOptionsWindow <> nil) then FOptionsWindow.Free();
  if (FScriptEditorWindow <> nil) then FScriptEditorWindow.Free();

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
var
  DesktopEnvironment : TDesktopEnvironment;
begin
  // Update wallpaper setter
  if (FWallpaperSetter <> nil) then FreeAndNil(FWallpaperSetter);

  DesktopEnvironment := CurrentSettings.DesktopEnvironment;
  if (DesktopEnvironment = DE_AUTODETECT) then begin
    DesktopEnvironment := FEnvironmentDetector.Detect();
  end;
  if (DesktopEnvironment = DE_UNKNOWN) then begin
    OpenOptionsForm(true);
    exit;
  end;
  FWallpaperSetter := FWallpaperSetterFactory.GetWallpaperSetter(DesktopEnvironment);

  // Update script executor
  if (FScriptExecutor <> nil) then FScriptExecutor.Free();

  FScriptExecutor := TWpcInThreadScriptExecutor.Create(FWallpaperSetter);
  FScriptExecutor.SetOnStopCallback(@OnScriptStoppedCallback);
end;

(* Engine *)

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
  if (not IsScriptRunning()) then
    raise TWpcUseErrorException.Create('Script is not running.');

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

procedure TWpcApplicationManager.SetNextWallpaper();
begin
  if (not IsScriptRunning()) then
    raise TWpcUseErrorException.Create('Script is not running.');

  FScriptExecutor.SkipCurrentDelay();
end;

(* UI *)

procedure TWpcApplicationManager.OpenOptionsForm(ForceSetEnvironment : Boolean = False);
begin
  if (FOptionsWindow = nil) then
    FOptionsWindow := TOptionsForm.Create(nil);

  FOptionsWindow.ShowOptinsForm(ForceSetEnvironment);
end;

procedure TWpcApplicationManager.OpenScriptEditorForm();
begin
  // Create new from each time. It will destroy itself on close.
  FScriptEditorWindow := TScriptEditorForm.Create(nil);
  FScriptEditorWindow.Show();
end;

procedure TWpcApplicationManager.OpenAboutForm();
var
  AboutForm : TWpcAboutForm;
begin
  AboutForm := TWpcAboutForm.Create(nil);
  try
    AboutForm.ShowModal();
  finally
    AboutForm.Free();
  end;
end;

procedure TWpcApplicationManager.OnScriptStoppedCallback(ExitStatus : TWpcScriptExecutionExitStatus);
begin
  if (FScript <> nil) then FreeAndNil(FScript);
  if (FScriptParser <> nil) then FreeAndNil(FScriptParser);
  if (FScriptContent <> nil) then FreeAndNil(FScriptContent);
end;


end.

