unit WpcOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IniFiles,
  WpcExceptions,
  WpcWallpaperStyles, WpcDesktopEnvironments, WpcWallpaperChangerAlgorithms;

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

    USE_INTERNAL_SETTER_KEY = 'UseInternal';
    EXTERNAL_SETTER_KEY = 'ExternalSetter';
    RUN_LAST_SCRIPT_ON_START_KEY = 'RunOnStart';

    DESKTOP_ENVIRONMENT_KEY = 'DesktopEnvironment';

    WALLPAPER_STYLE_KEY = 'Style';

    SIMPLE_CHANGER_USE_CONSTANT_DELAY_KEY = 'UseConstantDelay';
    SIMPLE_CHANGER_CONSTANT_DELAY_VALUE_KEY = 'ConstantDelayValue';
    SIMPLE_CHANGER_MINIMAL_DELAY_KEY = 'MinimalDelayValue';
    SIMPLE_CHANGER_MAXIMAL_DELAY_KEY = 'MaximalDelayValue';
  private const
    DEFAULT_USE_INTERNAL_SETTER = true;
    DEFAULT_EXTERNAL_SETTER = '';
    DEFAULT_RUN_ON_START = true;

    DEFAULT_DESKTOP_ENVIRONMENT = DE_UNKNOWN;

    DEFAULT_WALLPAPER_STYLE = CENTER;

    DEFAULT_USE_CONSTANT_DELAY = true;
    DEFAULT_CONSTANT_DELAY = 60 * 60 * 1000;
    DEFAULT_MINIMAL_DELAY = 60 * 5 * 1000;
    DEFAULT_MAXIMAL_DELAY = 60 * 60 * 10 * 1000;
  private
    FUseInternalSetter : Boolean;
    FExternalSetter    : String;
    FRunOnStart        : Boolean;

    FDesktopEnvironment : TDesktopEnvironment;

    FWallpaperStyle : TWallpaperStyle;

    FUseConstantDelay : Boolean;
    FConstantDelay    : Integer;
    FMinimalDelay     : Integer;
    FMaximalDelay     : Integer;
  private
    procedure SetConstantDelay(Delay : Integer);
    procedure SetMinimalDelay(Delay : Integer);
    procedure SetMaximalDelay(Delay : Integer);

    procedure ValidateDelay(Delay : Integer);
  public
    // Determines whether internal wallpaper setter should be used
    property UseInternalSetter : Boolean read FUseInternalSetter write FUseInternalSetter;
    // External (provided by user) utility for setting wallpaper
    property ExternalSetter : String read FExternalSetter write FExternalSetter;
    // Whether last script should be run on application start
    property RunOnStart : Boolean read FRunOnStart write FRunOnStart;

    // Graphical desktop environment, e.g. XFCE
    property DesktopEnvironment : TDesktopEnvironment read FDesktopEnvironment write FDesktopEnvironment;

    // Default style of the wallapaper, e.g. TILE
    property WallpaperStyle : TWallpaperStyle read FWallpaperStyle write FWallpaperStyle;

    // Determines whether constant delay should be used between current and next wallpaper set up.
    property UseConstantDelay : Boolean read FUseConstantDelay write FUseConstantDelay;
    // Delay between change of wallpaper. Used only if UseConstantDelay is true
    property ConstantDelay : Integer read FConstantDelay write SetConstantDelay;
    // Determines minimal and maximal delay between change of wallpaper. Used only if UseConstantDelay is false
    property MinimalDelay : Integer read FMinimalDelay write SetMinimalDelay;
    property MaximalDelay : Integer read FMaximalDelay write SetMaximalDelay;
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

    LAST_SET_TYPE_KEY = 'LastType';
    LAST_RUN_SCRIPT_KEY = 'LastScript';
    LAST_SET_DIR_KEY = 'LastDir';
  private
    FLastType : TWpcWallpaperChangerAlgorithm;
    FLastScript : String;
    FLastDirectory : String;
  public
    constructor Create(IniFile : String; IsFullPath : Boolean = false);
  public
    // Determines last action (script or directory was set)
    property LastType : TWpcWallpaperChangerAlgorithm read FLastType write FLastType;
    // Path to the last run script
    property LastScript : String read FLastScript write FLastScript;
    // Path to the last set directory
    property LastDirectory : String read FLastDirectory write FLastDirectory;
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
    FUseInternalSetter := SettingsFile.ReadBool(ENGINE_SECTION, USE_INTERNAL_SETTER_KEY, DEFAULT_USE_INTERNAL_SETTER);
    FExternalSetter := SettingsFile.ReadString(ENGINE_SECTION, EXTERNAL_SETTER_KEY, DEFAULT_EXTERNAL_SETTER);
    FRunOnStart := SettingsFile.ReadBool(ENGINE_SECTION, RUN_LAST_SCRIPT_ON_START_KEY, DEFAULT_RUN_ON_START);

    FDesktopEnvironment := StrToDesktopEnvironment(SettingsFile.ReadString(ENVIRONMENT_SECTION, DESKTOP_ENVIRONMENT_KEY, DE_UNKNOWN_ID));

    FWallpaperStyle := StrToWallpaperStyle(SettingsFile.ReadString(WALLAPAPER_SECTION, WALLPAPER_STYLE_KEY, WPST_CENTER));

    FUseConstantDelay := SettingsFile.ReadBool(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_USE_CONSTANT_DELAY_KEY, DEFAULT_USE_CONSTANT_DELAY);
    FConstantDelay := SettingsFile.ReadInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_USE_CONSTANT_DELAY_KEY, DEFAULT_CONSTANT_DELAY);
    FMinimalDelay := SettingsFile.ReadInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_MINIMAL_DELAY_KEY, DEFAULT_MINIMAL_DELAY);
    FMaximalDelay := SettingsFile.ReadInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_MAXIMAL_DELAY_KEY, DEFAULT_MAXIMAL_DELAY);
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
    SettingsFile.WriteBool(ENGINE_SECTION, USE_INTERNAL_SETTER_KEY, FUseInternalSetter);
    SettingsFile.WriteString(ENGINE_SECTION, EXTERNAL_SETTER_KEY, FExternalSetter);
    SettingsFile.WriteBool(ENGINE_SECTION, RUN_LAST_SCRIPT_ON_START_KEY, FRunOnStart);

    SettingsFile.WriteString(ENVIRONMENT_SECTION, DESKTOP_ENVIRONMENT_KEY, DesktopEnvironmentToStr(FDesktopEnvironment));

    SettingsFile.WriteString(WALLAPAPER_SECTION, WALLPAPER_STYLE_KEY, WallpaperStyleToStr(FWallpaperStyle));

    SettingsFile.WriteBool(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_USE_CONSTANT_DELAY_KEY, FUseConstantDelay);
    SettingsFile.WriteInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_CONSTANT_DELAY_VALUE_KEY, FConstantDelay);
    SettingsFile.WriteInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_MINIMAL_DELAY_KEY, FMinimalDelay);
    SettingsFile.WriteInteger(SIMPLE_CHANGER_SECTION, SIMPLE_CHANGER_MAXIMAL_DELAY_KEY, FMinimalDelay);

    SettingsFile.UpdateFile();
  finally
    SettingsFile.Free();
  end;
end;

procedure TWpcPersistentSettings.ResetToDefault();
begin
  FUseInternalSetter := DEFAULT_USE_INTERNAL_SETTER;
  FExternalSetter := DEFAULT_EXTERNAL_SETTER;
  FRunOnStart := DEFAULT_RUN_ON_START;

  FDesktopEnvironment := DEFAULT_DESKTOP_ENVIRONMENT;

  FWallpaperStyle := DEFAULT_WALLPAPER_STYLE;

  FUseConstantDelay := DEFAULT_USE_CONSTANT_DELAY;
  FConstantDelay := DEFAULT_CONSTANT_DELAY;
  FMinimalDelay := DEFAULT_MINIMAL_DELAY;
  FMaximalDelay := DEFAULT_MAXIMAL_DELAY;
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
    raise TWpcIllegalArgumentException.Create('Minimal delay shild be less then maximal.');
  FMinimalDelay := Delay;
end;

procedure TWpcPersistentSettings.SetMaximalDelay(Delay : Integer);
begin
  ValidateDelay(Delay);
  if (Delay < FMinimalDelay) then
    raise TWpcIllegalArgumentException.Create('Mmaximal delay shild be greater then minimal.');
  FMaximalDelay := Delay;
end;

procedure TWpcPersistentSettings.ValidateDelay(Delay : Integer);
begin
  if (Delay < 0) then
    raise TWpcIllegalArgumentException.Create('Delay sholud be positive.');
end;

{ TWpcStateSettings }

constructor TWpcStateSettings.Create(IniFile: String; IsFullPath: Boolean);
begin
  inherited Create(IniFile, IsFullPath);

  FLastType := WPCA_SCRIPT;
  FLastDirectory := '';
  FLastScript := '';
end;

procedure TWpcStateSettings.ReadFromFile();
var
  SettingsFile : TIniFile;
begin
  SettingsFile := TIniFile.Create(PathToIniFile);
  try
    FLastType := StrToWallpaperChangerAlgorithm(SettingsFile.ReadString(ENGINE_SECTION, LAST_SET_TYPE_KEY, WPCA_SCRIPT_ID));
    FLastScript := SettingsFile.ReadString(SCRIPT_SECTION, LAST_RUN_SCRIPT_KEY, '');
    FLastDirectory := SettingsFile.ReadString(SIMPLE_CHANGER_SECTION, LAST_SET_DIR_KEY, '');
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
    SettingsFile.WriteString(ENGINE_SECTION, LAST_SET_TYPE_KEY, WallpaperChangerAlgorithmToStr(FLastType));
    SettingsFile.WriteString(SCRIPT_SECTION, LAST_RUN_SCRIPT_KEY, FLastScript);
    SettingsFile.WriteString(SIMPLE_CHANGER_SECTION, LAST_SET_DIR_KEY, FLastDirectory);

    SettingsFile.UpdateFile();
  finally
    SettingsFile.Free();
  end;
end;


end.

