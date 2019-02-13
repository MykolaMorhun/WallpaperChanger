unit WpcApplicationManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fgl,
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
  OSUtils,
  WpcDocumentationOpener,

  MainForm,
  WpcOptionsForm,
  WpcScriptEditorForm,
  WpcAboutForm;

const
  SETTINGS_FILE = 'WPCSettings.ini';
  STATE_FILE = 'WPCState.ini';
  SCRIPT_EDITOR_SETTINGS_FILE = 'WPCScriptEditorSettings.ini';

  DOCS_DIR = 'docs';
  DOCS_LANG = 'en';

type

  TWpcScriptEditorsList = specialize TFPGList<TScriptEditorForm>;

  { TWpcApplicationManager }

  // Singleton. Holds application state and modules objects.
  TWpcApplicationManager = class(TObject)
  private
    FApplicationSettings      : TWpcPersistentSettings;
    FApplicationStateSettings : TWpcStateSettings;
    FScriptEditorSettings     : TWpcScriptEditorSettings;

    FEnvironmentDetector    : IWpcEnvironmentDetector;
    FWallpaperSetter        : IWpcWallpaperSetter;
    FWallpaperSetterFactory : IWpcWallpaperSetterFactory;

    FScriptsGenerator : TWpcScriptsGenerator;
    FScriptExecutor : IWpcScriptExecutor;

    FDocsOpener : TWpcDocumentationOpener;

    // Current script data
    FScriptContent : TStringList;
    FScriptParser  : TWpcScriptParser;
    FScript        : TWpcScript;
  private
    FMainForm : TBannerForm;

    // Holds opened script editors and is used for closing them on application exit if any.
    // When an editor is closed during runtime then callback will remove the editor form the list.
    FOpenedScriptEditors : TWpcScriptEditorsList;

    FOptionsForm : TOptionsForm;
    FIsOptionsFormOpen : Boolean;

    FAboutForm : TWpcAboutForm;
    FIsAboutFormOpen : Boolean;
  public
    constructor Create(MainForm : TBannerForm);
    destructor Destroy(); override;
  private
    function GetDocumetationOpener() : TWpcDocumentationOpener;
  public
    // Supposed to be changed only from options window or ApplicationManager.
    property CurrentSettings : TWpcPersistentSettings read FApplicationSettings;
    property CurrentState : TWpcStateSettings read FApplicationStateSettings;
    property ScriptEditorState : TWpcScriptEditorSettings read FScriptEditorSettings;

    property EnvironmentDetector : IWpcEnvironmentDetector read FEnvironmentDetector;
    property WallpaperSetter : IWpcWallpaperSetter read FWallpaperSetter;
    property WallpaperSetterFactory : IWpcWallpaperSetterFactory read FWallpaperSetterFactory;

    property DocumentationOpener : TWpcDocumentationOpener read GetDocumetationOpener;
  public
    procedure ApplySettings();

    procedure RunScript(PathToScript : String);
    procedure StopScript();
    function IsScriptRunning() : Boolean; inline;

    procedure SetWallpaper(Image : TWpcImage);
    procedure SetWallpapersFromDirectory(Directory : TWpcDirectory);

    procedure SetNextWallpaper();

    procedure ReRunLastTask();
  public
    procedure OpenScriptEditorForm(PathToScript : String = ''; Line : Integer = 1);
    procedure OpenOptionsForm(ForceSetEnvironment : Boolean = False);
    procedure OpenAboutForm();
  private
    procedure OnScriptStartedCallback();
    procedure OnScriptStoppedCallback(ExitStatus : TWpcScriptExecutionExitStatus);
    procedure OnScriptEditorWindowClosedCallback(ScriptEditorWindow : TScriptEditorForm);
  private
    procedure ReadSettings();
  end;


implementation

{ TWpcApplicationManager }

constructor TWpcApplicationManager.Create(MainForm : TBannerForm);
begin
  FMainForm := MainForm;

  ReadSettings();

  Randomize();

  FEnvironmentDetector := GetEnvironmentDetector();
  FWallpaperSetterFactory := GetWallpaperSetterFactory();

  FScriptsGenerator := TWpcScriptsGenerator.Create();

  FDocsOpener := nil;

  FOpenedScriptEditors := TWpcScriptEditorsList.Create();
  FIsOptionsFormOpen := False;
  FIsAboutFormOpen := False;

  ApplySettings(); // complete initialization

  if (FApplicationSettings.RunLastTaskOnStart) then
    ReRunLastTask()
  else if (FApplicationSettings.RunTerminatedTaskOnStart and FApplicationStateSettings.TaskWasRunningOnExit) then
    ReRunLastTask();
end;

destructor TWpcApplicationManager.Destroy();
var
  ScriptEditorWindow : TScriptEditorForm;
begin
  // Close all opened script editors if any
  for ScriptEditorWindow in FOpenedScriptEditors do
    ScriptEditorWindow.Free();
  FOpenedScriptEditors.Free();

  // Save state
  if (IsScriptRunning()) then
    CurrentState.TaskWasRunningOnExit := True
  else
    CurrentState.TaskWasRunningOnExit := False;

  try
    FApplicationStateSettings.SaveIntoFile();
  except
    // Do nothing if failed to save application state.
  end;

  if (FScriptEditorSettings <> nil) then
    try
      FScriptEditorSettings.SaveIntoFile();
    except
      // Do nothing if failed to save script editor settings.
    end;

  // Stop script execution if any
  if (FScriptExecutor.IsRunning()) then
    FScriptExecutor.Terminate();

  // Clean up resources
  FApplicationSettings.Free();
  FApplicationStateSettings.Free();
  if (FScriptEditorSettings <> nil) then FScriptEditorSettings.Free();

  if (FDocsOpener <> nil) then FDocsOpener.Free();

  FScriptsGenerator.Free();

  if (FWallpaperSetter <> nil) then FreeAndNil(FWallpaperSetter);
  FWallpaperSetterFactory.Free();
  if (FEnvironmentDetector <> nil) then FreeAndNil(FEnvironmentDetector);
end;

function TWpcApplicationManager.GetDocumetationOpener() : TWpcDocumentationOpener;
begin
  if (FDocsOpener = nil) then
    FDocsOpener := TWpcDocumentationOpener.Create(GetAbsolutePath(DOCS_DIR), DOCS_LANG);

  Result := FDocsOpener;
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

  // Read file if requested.
  FScriptEditorSettings := nil;
end;

{
  Applays settings from ApplicationSettings object.
  Should be invoked for full initialization of the application.
}
procedure TWpcApplicationManager.ApplySettings();
var
  DesktopEnvironment : TDesktopEnvironment;

  ReRunTusk : Boolean;
begin
  // If options are changed when a task is running then rerun it with new options.
  ReRunTusk := False;
  if ((FScriptExecutor <> nil) and IsScriptRunning()) then begin
    ReRunTusk := True;
    StopScript();
  end;

  // Update wallpaper setter
  if (FWallpaperSetter <> nil) then FreeAndNil(FWallpaperSetter);

  DesktopEnvironment := CurrentSettings.DesktopEnvironment;
  if (DesktopEnvironment = DE_AUTODETECT) then begin
    DesktopEnvironment := FEnvironmentDetector.Detect();
  end;
  if (DesktopEnvironment = DE_UNKNOWN) then begin
    OpenOptionsForm(True);
    exit;
  end;
  FWallpaperSetter := FWallpaperSetterFactory.GetWallpaperSetter(DesktopEnvironment);

  // Update script executor
  if (FScriptExecutor <> nil) then FScriptExecutor.Free();

  FScriptExecutor := TWpcInThreadScriptExecutor.Create(FWallpaperSetter);
  FScriptExecutor.SetOnStartCallback(@OnScriptStartedCallback);
  FScriptExecutor.SetOnStopCallback(@OnScriptStoppedCallback);

  if (ReRunTusk) then
    ReRunLastTask();
end;

(* Engine *)

procedure TWpcApplicationManager.RunScript(PathToScript : String);
begin
  if (FIsOptionsFormOpen and (FWallpaperSetter = nil)) then begin
    // User is asked to set desktop environment manually because autodetect failed.
    // Do not allow to run scripts in this state.
    FOptionsForm.SetFocus();
    exit;
  end;

  FApplicationStateSettings.LastType := WPCA_SCRIPT;
  FApplicationStateSettings.LastScript := PathToScript;

  try
    FScriptContent := TStringList.Create();
    // TODO check file type
    FScriptContent.LoadFromFile(PathToScript);
    FScriptParser := TWpcScriptParser.Create(FScriptContent);
    FScriptParser.BasePath := ExtractFilePath(PathToScript);
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

procedure TWpcApplicationManager.ReRunLastTask();
var
  Directory : TWpcDirectory;
  Image     : TWpcImage;
begin
  try
    case (FApplicationStateSettings.LastType) of
      WPCA_SCRIPT:
        if (FApplicationStateSettings.LastScript <> '') then begin
          RunScript(FApplicationStateSettings.LastScript);
          // This check is needed for case when the script above exits right after start
          if (IsScriptRunning()) then
            FMainForm.UpdateUIOnScriptStart();
        end;
      WPCA_DIRECTORY:
        if (FApplicationStateSettings.LastDirectory <> '') then begin
          Directory := TWpcDirectory.Create(FApplicationStateSettings.LastDirectory);
          try
            SetWallpapersFromDirectory(Directory);
            FMainForm.UpdateUIOnScriptStart();
          finally
            Directory.Free();
          end;
        end;
      WPCA_IMAGE:
        if (FApplicationStateSettings.LastWallpaper <> '') then begin
          Image := TWpcImage.Create(FApplicationStateSettings.LastWallpaper);
          try
            SetWallpaper(Image);
          finally
            Image.Free();
          end;
        end;
    end;
  except
    on E : Exception do
      FMainForm.ShowBalloonMessage(E.Message, 'Failed to rerun last tusk');
  end;
end;

(* Windows managment *)

procedure TWpcApplicationManager.OpenScriptEditorForm(PathToScript : String = ''; Line : Integer = 1);
var
  ScriptEditorWindow : TScriptEditorForm;
begin
  if (FScriptEditorSettings = nil) then begin
    FScriptEditorSettings := TWpcScriptEditorSettings.Create(SCRIPT_EDITOR_SETTINGS_FILE);
    FScriptEditorSettings.ReadFromFile();
  end;

  // Create new from each time.
  // It will destroy itself on close or Application Manager will destroy it on exit.
  ScriptEditorWindow := TScriptEditorForm.Create(nil);
  ScriptEditorWindow.SetOnCloseCallback(@OnScriptEditorWindowClosedCallback);
  FOpenedScriptEditors.Add(ScriptEditorWindow);
  ScriptEditorWindow.OpenScript(PathToScript, Line);
  ScriptEditorWindow.Show();
end;

procedure TWpcApplicationManager.OpenOptionsForm(ForceSetEnvironment : Boolean = False);
begin
  if (FIsOptionsFormOpen) then begin
    FOptionsForm.SetFocus();
    exit;
  end;

  FIsOptionsFormOpen := True;
  FOptionsForm := TOptionsForm.Create(nil);
  try
    FOptionsForm.ShowModalOptionsForm(ForceSetEnvironment);
  finally
    FOptionsForm.Free();
    FIsOptionsFormOpen := False;
  end;
end;

procedure TWpcApplicationManager.OpenAboutForm();
begin
  if (FIsAboutFormOpen) then begin
    FAboutForm.SetFocus();
    exit;
  end;

  FIsAboutFormOpen := True;
  FAboutForm := TWpcAboutForm.Create(nil);
  try
    FAboutForm.ShowModal();
  finally
    FAboutForm.Free();
    FIsAboutFormOpen := False;
  end;
end;

(* Callbacks *)

procedure TWpcApplicationManager.OnScriptStartedCallback();
begin
  FMainForm.UpdateUIOnScriptStart();
end;

procedure TWpcApplicationManager.OnScriptStoppedCallback(ExitStatus : TWpcScriptExecutionExitStatus);
begin
  if (FScript <> nil) then FreeAndNil(FScript);
  if (FScriptParser <> nil) then FreeAndNil(FScriptParser);
  if (FScriptContent <> nil) then FreeAndNil(FScriptContent);

  FMainForm.UpdateUIOnScriptStop();
end;

procedure TWpcApplicationManager.OnScriptEditorWindowClosedCallback(ScriptEditorWindow : TScriptEditorForm);
begin
  FOpenedScriptEditors.Remove(ScriptEditorWindow);
end;


end.

