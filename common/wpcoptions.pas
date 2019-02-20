unit WpcOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IniFiles,
  WpcExceptions,
  WpcWallpaperStyles,
  WpcDesktopEnvironments,
  WpcWallpaperChangerAlgorithms,
  WpcMainManuActions;

type

  { TWpcAbstractSettings }

  TWpcAbstractSettings = class abstract(TObject)
  protected
    PathToIniFile : String;
  public
    constructor Create(IniFile : String; IsFullPath : Boolean = false);
  public
    procedure ReadFromFile(); virtual; abstract;
    procedure SaveIntoFile(); virtual; abstract;
  end;

  { TWpcPersistentSettings }

  TWpcPersistentSettings = class(TWpcAbstractSettings)
  private const
    ENGINE_SECTION = 'Engine';
    ENVIRONMENT_SECTION = 'Environment';
    WALLAPAPER_SECTION = 'Wallpaper';
    SIMPLE_CHANGER_SECTION = 'SimpleChanger';

    CUSTOM_SETTER_KEY = 'ExternalSetter';
    RUN_TERMINATED_TASK_ON_START_KEY = 'RunTerminatedTaskOnStart';
    RUN_LAST_TASK_ON_START_KEY = 'RunLastTaskOnStart';

    TRAY_CLICK_ACTION_KEY = 'TrayClickAction';

    DESKTOP_ENVIRONMENT_KEY = 'DesktopEnvironment';

    WALLPAPER_STYLE_KEY = 'DefaultStyle';

    SIMPLE_CHANGER_USE_CONSTANT_DELAY_KEY = 'UseConstantDelay';
    SIMPLE_CHANGER_CONSTANT_DELAY_VALUE_KEY = 'ConstantDelayValue';
    SIMPLE_CHANGER_MINIMAL_DELAY_KEY = 'MinimalDelayValue';
    SIMPLE_CHANGER_MAXIMAL_DELAY_KEY = 'MaximalDelayValue';
    SIMPLE_CHANGER_KEEP_ORDER_KEY = 'KeepOrder';
    SIMPLE_CHANGER_SEARCH_IN_SUBDIRECTORIES_KEY = 'Subdirectories';
  private const
    DEFAULT_CUSTOM_SETTER = '';
    DEFAULT_RUN_TERMINATED_ON_START = true;
    DEFAULT_RUN_ON_START = false;

    DEFAULT_TRAY_CLICK_ACTION = MM_SHOW_MAIN_MENU_STRING;

    DEFAULT_DESKTOP_ENVIRONMENT = DE_AUTODETECT;

    DEFAULT_WALLPAPER_STYLE = CENTERED;

    DEFAULT_USE_CONSTANT_DELAY = true;
    DEFAULT_CONSTANT_DELAY = 60 * 60 * 1000;
    DEFAULT_MINIMAL_DELAY = 5 * 60 * 1000;
    DEFAULT_MAXIMAL_DELAY = 2 * 60 * 60 * 1000;
    DEFAULT_KEEP_ORDER = true;
    DEFAULT_SEARCH_IN_SUBDIRECTORIES = false;
  private
    FCustomSetter : String;
    FRunTerminatedTaskOnStart : Boolean;
    FRunLastTaskOnStart : Boolean;

    FTrayClickAction : EMainManuActions;

    FDesktopEnvironment : TDesktopEnvironment;

    FWallpaperStyle : TWpcWallpaperStyle;

    FUseConstantDelay       : Boolean;
    FConstantDelay          : Integer;
    FMinimalDelay           : Integer;
    FMaximalDelay           : Integer;
    FKeepOrder              : Boolean;
    FSearchInSubDirectories : Boolean;
  private
    procedure SetConstantDelay(Delay : Integer);
    procedure SetMinimalDelay(Delay : Integer);
    procedure SetMaximalDelay(Delay : Integer);

    procedure ValidateDelay(Delay : Integer);
  public
    // External (provided by user) utility for setting wallpaper
    property CustomSetter : String read FCustomSetter write FCustomSetter;
    // Whether terminated by appliction exit script should be run on application start
    property RunTerminatedTaskOnStart : Boolean read FRunTerminatedTaskOnStart write FRunTerminatedTaskOnStart;
    // Whether last used script should be run on application start
    property RunLastTaskOnStart : Boolean read FRunLastTaskOnStart write FRunLastTaskOnStart;

    // Holds action index to be executed on click on Wallpaper Changer in tray
    property TrayClickAction : EMainManuActions read FTrayClickAction write FTrayClickAction;

    // Graphical desktop environment, e.g. XFCE
    property DesktopEnvironment : TDesktopEnvironment read FDesktopEnvironment write FDesktopEnvironment;

    // Default style of the wallapaper, e.g. TILE
    property WallpaperStyle : TWpcWallpaperStyle read FWallpaperStyle write FWallpaperStyle;

    // Determines whether constant delay should be used between current and next wallpaper set up.
    property UseConstantDelay : Boolean read FUseConstantDelay write FUseConstantDelay;
    // Delay between change of wallpaper. Used only if UseConstantDelay is true
    property ConstantDelay : Integer read FConstantDelay write SetConstantDelay;
    // Determines minimal and maximal delay between change of wallpaper. Used only if UseConstantDelay is false
    property MinimalDelay : Integer read FMinimalDelay write SetMinimalDelay;
    property MaximalDelay : Integer read FMaximalDelay write SetMaximalDelay;
    // If true all images in specified folder will be added into list and setter will use them in a circle with correct navigation next/prev.
    // If false each time new image from specified folder will be picked. It is possible to have the same image more then one time.
    property KeepOrder : Boolean read FKeepOrder write FKeepOrder;
    // If true all images from all subdirectories of specific folder will be added recursively into the list.
    // If false only images from specified folder will be added to the list.
    property SearchInSubdirectories : Boolean read FSearchInSubDirectories write FSearchInSubDirectories;
  public
    procedure SetMinMaxDelay(MinDelay : Integer; MaxDelay : Integer);
  public
    procedure ReadFromFile(); override;
    procedure SaveIntoFile(); override;

    procedure ResetToDefault();
  end;

  { TWpcStateSettings }

  TWpcStateSettings = class(TWpcAbstractSettings)
  private const
    ENGINE_SECTION = 'Engine';
    SCRIPT_SECTION = 'Script';
    SIMPLE_CHANGER_SECTION = 'SimpleChanger';
    WALLPAPER_SECTION = 'Wallpaper';

    TASK_WAS_RUNNING_ON_EXIT_KEY = 'TaskWasRunningOnApplicationExit';
    LAST_SET_TYPE_KEY = 'LastType';
    LAST_RUN_SCRIPT_KEY = 'LastScript';
    LAST_SET_DIR_KEY = 'LastDir';
    LAST_WALLPAPER_KEY = 'LastWallpaper';
  private
    FTaskWasRunningOnExit : Boolean;
    FLastType : TWpcWallpaperChangerAlgorithm;
    FLastScript : String;
    FLastDirectory : String;
    FLastWallpaper : String;
  public
    constructor Create(IniFile : String; IsFullPath : Boolean = false);
  public
    // Shows whether a task was terminated on last application exit
    property TaskWasRunningOnExit : Boolean read FTaskWasRunningOnExit write FTaskWasRunningOnExit;
    // Determines last action (script or directory was set)
    property LastType : TWpcWallpaperChangerAlgorithm read FLastType write FLastType;
    // Path to the last run script
    property LastScript : String read FLastScript write FLastScript;
    // Path to the last set directory
    property LastDirectory : String read FLastDirectory write FLastDirectory;
    // Path to the last set image
    property LastWallpaper : String read FLastWallpaper write FLastWallpaper;
  public
    procedure ReadFromFile(); override;
    procedure SaveIntoFile(); override;
  end;

  { TWpcScriptEditorSettings }

  TWpcScriptEditorSettings = class(TWpcAbstractSettings)
  private const
    EDITOR_SECTION = 'Editor';
    STATE_SECTION = 'State';

    SHOW_EDITOR_TOOLBAR_KEY = 'ShowEditorToolBar';

    EDITOR_FONT_FAMILY_KEY = 'FontName';
    EDITOR_FONT_SIZE_KEY = 'FontSize';

    WINDOW_WIDTH_KEY = 'WindowWidth';
    WINDOW_HEIGHT_KEY = 'WindowHeight';
    WINDOW_POSITION_LEFT_KEY = 'WindowPositionLeft';
    WINDOW_POSITION_TOP_KEY = 'WindowPositionTop';

    DEFAULT_SHOW_EDITOR_TOOLBAR = True;

    DEFAULT_EDITOR_FONT_FAMILY = 'DejaVu Sans Mono';
    DEFAULT_EDITOR_FONT_SIZE = 11;

    DEFAULT_WINDOW_WIDTH = 650;
    DEFAULT_WINDOW_HEIGHT = 400;
    DEFAULT_WINDOW_POSITION_LEFT = 100;
    DEFAULT_WINDOW_POSITION_TOP = 100;
  private
    FShowEditorToolBar : Boolean;

    FFontName  : String;
    FFontSize  : Integer;
    FFontStyle : Integer;

    FWindowWidth  : Integer;
    FWindowHeight : Integer;
    FWindowPositionLeft : Integer;
    FWindowPositionTop  : Integer;
  public
    // Whether show toolbar for script editor
    property ShowEditorToolBar : Boolean read FShowEditorToolBar write FShowEditorToolBar;

    // Script editor font family, e.g. Arial, Liberation Serif
    property FontName : String read FFontName write FFontName;
    // Size of font in script editor
    property FontSize : Integer read FFontSize write FFontSize;
    // Attributes of script editor font, e.g. bold, italic
    property FontStyle : Integer read FFontStyle write FFontStyle;

    // Last script editor window width (pixels)
    property WindowWidth : Integer read FWindowWidth write FWindowWidth;
    // Last script editor window height (pixels)
    property WindowHeight : Integer read FWindowHeight write FWindowHeight;
    // Last position of script editor window from the screen left (pixels)
    property WindowPositionLeft : Integer read FWindowPositionLeft write FWindowPositionLeft;
    // Last position of script editor window from the screen top (pixels)
    property WindowPositionTop : Integer read FWindowPositionTop write FWindowPositionTop;
  public
    procedure ReadFromFile(); override;
    procedure SaveIntoFile(); override;
  end;


implementation


{ TWpcAbstractSettings }

constructor TWpcAbstractSettings.Create(IniFile : String; IsFullPath : Boolean);
begin
  if (IsFullPath) then
    PathToIniFile := IniFile
  else
    PathToIniFile := ExtractFilePath(ParamStr(0)) + IniFile;
end;

{ TWpcPersistentSettings }

procedure TWpcPersistentSettings.ReadFromFile();
var
  SettingsFile : TIniFile;
begin
  SettingsFile := TIniFile.Create(PathToIniFile);
  try
    FCustomSetter := SettingsFile.ReadString(ENGINE_SECTION, CUSTOM_SETTER_KEY, DEFAULT_CUSTOM_SETTER);
    FRunTerminatedTaskOnStart := SettingsFile.ReadBool(ENGINE_SECTION, RUN_TERMINATED_TASK_ON_START_KEY, DEFAULT_RUN_TERMINATED_ON_START);
    FRunLastTaskOnStart := SettingsFile.ReadBool(ENGINE_SECTION, RUN_LAST_TASK_ON_START_KEY, DEFAULT_RUN_ON_START);

    FTrayClickAction := StrToMainMenuAction(SettingsFile.ReadString(ENGINE_SECTION, TRAY_CLICK_ACTION_KEY, DEFAULT_TRAY_CLICK_ACTION));

    FDesktopEnvironment := StrToDesktopEnvironment(SettingsFile.ReadString(ENVIRONMENT_SECTION, DESKTOP_ENVIRONMENT_KEY, DE_AUTODETECT_ID));

    FWallpaperStyle := StrToWallpaperStyle(SettingsFile.ReadString(WALLAPAPER_SECTION, WALLPAPER_STYLE_KEY, WPST_CENTERED));
    if (FWallpaperStyle = UNKNOWN) then
      FWallpaperStyle := CENTERED;

    FUseConstantDelay := SettingsFile.ReadBool(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_USE_CONSTANT_DELAY_KEY, DEFAULT_USE_CONSTANT_DELAY);
    FConstantDelay := SettingsFile.ReadInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_CONSTANT_DELAY_VALUE_KEY, DEFAULT_CONSTANT_DELAY);
    FMinimalDelay := SettingsFile.ReadInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_MINIMAL_DELAY_KEY, DEFAULT_MINIMAL_DELAY);
    FMaximalDelay := SettingsFile.ReadInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_MAXIMAL_DELAY_KEY, DEFAULT_MAXIMAL_DELAY);
    FKeepOrder := SettingsFile.ReadBool(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_KEEP_ORDER_KEY, DEFAULT_KEEP_ORDER);
    FSearchInSubDirectories := SettingsFile.ReadBool(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_SEARCH_IN_SUBDIRECTORIES_KEY, DEFAULT_SEARCH_IN_SUBDIRECTORIES);
  finally
    SettingsFile.Free();
  end;
end;

procedure TWpcPersistentSettings.SaveIntoFile();
var
  SettingsFile : TIniFile;
begin
  SettingsFile := TIniFile.Create(PathToIniFile);
  SettingsFile.CacheUpdates := true;
  try
    SettingsFile.WriteString(ENGINE_SECTION, CUSTOM_SETTER_KEY, FCustomSetter);
    SettingsFile.WriteBool(ENGINE_SECTION, RUN_TERMINATED_TASK_ON_START_KEY, FRunTerminatedTaskOnStart);
    SettingsFile.WriteBool(ENGINE_SECTION, RUN_LAST_TASK_ON_START_KEY, FRunLastTaskOnStart);

    SettingsFile.WriteString(ENGINE_SECTION, TRAY_CLICK_ACTION_KEY, MainManuActionToStr(FTrayClickAction));

    SettingsFile.WriteString(ENVIRONMENT_SECTION, DESKTOP_ENVIRONMENT_KEY, DesktopEnvironmentToStr(FDesktopEnvironment));

    SettingsFile.WriteString(WALLAPAPER_SECTION, WALLPAPER_STYLE_KEY, WallpaperStyleToStr(FWallpaperStyle));

    SettingsFile.WriteBool(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_USE_CONSTANT_DELAY_KEY, FUseConstantDelay);
    SettingsFile.WriteInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_CONSTANT_DELAY_VALUE_KEY, FConstantDelay);
    SettingsFile.WriteInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_MINIMAL_DELAY_KEY, FMinimalDelay);
    SettingsFile.WriteInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_MAXIMAL_DELAY_KEY, FMaximalDelay);
    SettingsFile.WriteBool(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_KEEP_ORDER_KEY, FKeepOrder);
    SettingsFile.WriteBool(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_SEARCH_IN_SUBDIRECTORIES_KEY, FSearchInSubDirectories);

    SettingsFile.UpdateFile();
  finally
    SettingsFile.Free();
  end;
end;

procedure TWpcPersistentSettings.ResetToDefault();
begin
  FCustomSetter := DEFAULT_CUSTOM_SETTER;
  FRunTerminatedTaskOnStart := DEFAULT_RUN_TERMINATED_ON_START;
  FRunLastTaskOnStart := DEFAULT_RUN_ON_START;

  FDesktopEnvironment := DEFAULT_DESKTOP_ENVIRONMENT;

  FWallpaperStyle := DEFAULT_WALLPAPER_STYLE;

  FUseConstantDelay := DEFAULT_USE_CONSTANT_DELAY;
  FConstantDelay := DEFAULT_CONSTANT_DELAY;
  FMinimalDelay := DEFAULT_MINIMAL_DELAY;
  FMaximalDelay := DEFAULT_MAXIMAL_DELAY;
  FKeepOrder := DEFAULT_KEEP_ORDER;
  FSearchInSubDirectories := DEFAULT_SEARCH_IN_SUBDIRECTORIES;
end;

procedure TWpcPersistentSettings.SetConstantDelay(Delay : Integer);
begin
  ValidateDelay(Delay);
  FConstantDelay := Delay;
end;

procedure TWpcPersistentSettings.SetMinimalDelay(Delay : Integer);
begin
  ValidateDelay(Delay);
  if (Delay > FMaximalDelay) then
    raise TWpcIllegalArgumentException.Create('Minimal delay should be less then maximal.');
  FMinimalDelay := Delay;
end;

procedure TWpcPersistentSettings.SetMaximalDelay(Delay : Integer);
begin
  ValidateDelay(Delay);
  if (Delay < FMinimalDelay) then
    raise TWpcIllegalArgumentException.Create('Mmaximal delay should be greater then minimal.');
  FMaximalDelay := Delay;
end;

procedure TWpcPersistentSettings.ValidateDelay(Delay : Integer);
begin
  if (Delay < 0) then
    raise TWpcIllegalArgumentException.Create('Delay sholud be positive.');
end;

procedure TWpcPersistentSettings.SetMinMaxDelay(MinDelay : Integer; MaxDelay : Integer);
begin
  if (MinDelay > MaxDelay) then
    raise TWpcIllegalArgumentException.Create('Minimal delay should be less then maximal.');

  FMinimalDelay := MinDelay;
  FMaximalDelay := MaxDelay;
end;

{ TWpcStateSettings }

constructor TWpcStateSettings.Create(IniFile: String; IsFullPath: Boolean);
begin
  inherited Create(IniFile, IsFullPath);

  FTaskWasRunningOnExit := False;
  FLastType := WPCA_SCRIPT;
  FLastDirectory := '';
  FLastScript := '';
  FLastWallpaper := '';
end;

procedure TWpcStateSettings.ReadFromFile();
var
  SettingsFile : TIniFile;
begin
  SettingsFile := TIniFile.Create(PathToIniFile);
  try
    FTaskWasRunningOnExit := SettingsFile.ReadBool(ENGINE_SECTION, TASK_WAS_RUNNING_ON_EXIT_KEY, False);
    FLastType := StrToWallpaperChangerAlgorithm(SettingsFile.ReadString(ENGINE_SECTION, LAST_SET_TYPE_KEY, WPCA_SCRIPT_ID));
    FLastScript := SettingsFile.ReadString(SCRIPT_SECTION, LAST_RUN_SCRIPT_KEY, '');
    FLastDirectory := SettingsFile.ReadString(SIMPLE_CHANGER_SECTION, LAST_SET_DIR_KEY, '');
    FLastWallpaper := SettingsFile.ReadString(WALLPAPER_SECTION, LAST_WALLPAPER_KEY, '');
  finally
    SettingsFile.Free();
  end;
end;

procedure TWpcStateSettings.SaveIntoFile();
var
  SettingsFile : TIniFile;
begin
  SettingsFile := TIniFile.Create(PathToIniFile);
  SettingsFile.CacheUpdates := true;
  try
    SettingsFile.WriteBool(ENGINE_SECTION, TASK_WAS_RUNNING_ON_EXIT_KEY, FTaskWasRunningOnExit);
    SettingsFile.WriteString(ENGINE_SECTION, LAST_SET_TYPE_KEY, WallpaperChangerAlgorithmToStr(FLastType));
    SettingsFile.WriteString(SCRIPT_SECTION, LAST_RUN_SCRIPT_KEY, FLastScript);
    SettingsFile.WriteString(SIMPLE_CHANGER_SECTION, LAST_SET_DIR_KEY, FLastDirectory);
    SettingsFile.WriteString(WALLPAPER_SECTION, LAST_WALLPAPER_KEY, FLastWallpaper);

    SettingsFile.UpdateFile();
  finally
    SettingsFile.Free();
  end;
end;

{ TWpcScriptEditorSettings }

procedure TWpcScriptEditorSettings.ReadFromFile();
var
  ScriptEditorSettingsFile : TIniFile;
begin
  ScriptEditorSettingsFile := TIniFile.Create(PathToIniFile);
  try
    FShowEditorToolBar := ScriptEditorSettingsFile.ReadBool(EDITOR_SECTION, SHOW_EDITOR_TOOLBAR_KEY, DEFAULT_SHOW_EDITOR_TOOLBAR);

    FFontName := ScriptEditorSettingsFile.ReadString(EDITOR_SECTION, EDITOR_FONT_FAMILY_KEY, DEFAULT_EDITOR_FONT_FAMILY);
    FFontSize := ScriptEditorSettingsFile.ReadInteger(EDITOR_SECTION, EDITOR_FONT_SIZE_KEY, DEFAULT_EDITOR_FONT_SIZE);

    FWindowWidth := ScriptEditorSettingsFile.ReadInteger(STATE_SECTION, WINDOW_WIDTH_KEY, DEFAULT_WINDOW_WIDTH);
    FWindowHeight := ScriptEditorSettingsFile.ReadInteger(STATE_SECTION, WINDOW_HEIGHT_KEY, DEFAULT_WINDOW_HEIGHT);
    FWindowPositionLeft := ScriptEditorSettingsFile.ReadInteger(STATE_SECTION, WINDOW_POSITION_LEFT_KEY, DEFAULT_WINDOW_POSITION_LEFT);
    FWindowPositionTop := ScriptEditorSettingsFile.ReadInteger(STATE_SECTION, WINDOW_POSITION_TOP_KEY, DEFAULT_WINDOW_POSITION_TOP);
  finally
    ScriptEditorSettingsFile.Free();
  end;
end;

procedure TWpcScriptEditorSettings.SaveIntoFile();
var
  ScriptEditorSettingsFile : TIniFile;
begin
  ScriptEditorSettingsFile := TIniFile.Create(PathToIniFile);
  ScriptEditorSettingsFile.CacheUpdates := True;
  try
    ScriptEditorSettingsFile.WriteBool(EDITOR_SECTION, SHOW_EDITOR_TOOLBAR_KEY, FShowEditorToolBar);

    ScriptEditorSettingsFile.WriteString(EDITOR_SECTION, EDITOR_FONT_FAMILY_KEY, FFontName);
    ScriptEditorSettingsFile.WriteInteger(EDITOR_SECTION, EDITOR_FONT_SIZE_KEY, FFontSize);

    ScriptEditorSettingsFile.WriteInteger(STATE_SECTION, WINDOW_WIDTH_KEY, FWindowWidth);
    ScriptEditorSettingsFile.WriteInteger(STATE_SECTION, WINDOW_HEIGHT_KEY, FWindowHeight);
    ScriptEditorSettingsFile.WriteInteger(STATE_SECTION, WINDOW_POSITION_LEFT_KEY, FWindowPositionLeft);
    ScriptEditorSettingsFile.WriteInteger(STATE_SECTION, WINDOW_POSITION_TOP_KEY, FWindowPositionTop);
  finally
    ScriptEditorSettingsFile.Free();
  end;
end;


end.

