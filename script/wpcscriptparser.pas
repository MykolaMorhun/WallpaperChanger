unit WpcScriptParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Fgl, StrUtils,
  WpcScript,
  WpcBaseStatement,
  WpcBranchStatement,
  WpcBranchActionsStatements,
  WpcWallpaperStatement,
  WpcDirectoryStatement,
  WpcChooserStatements,
  WpcWaitStatement,
  WpcStopStatement,
  WpcStatementProperties,
  WpcScriptCommons,
  WpcWallpaperStyles,
  WpcTimeMeasurementUnits,
  WpcTimeUtils,
  WpcImage,
  WpcDirectory,
  WpcExceptions,
  OSUtils;

const
  WHITESPACE_SET = [ #32, #9 ];
  ALLOWED_NAME_SYMBOLS = [ 'A'..'Z', 'a'..'z', '0'..'9', '_' ];

  END_OF_SCRIPT = '_END_OF_WPC_SCRIPT_';

  VARIABLE_START_SYMBOL = '$';
  COMMENTARY_SYMBOL = '#';
  QUOTE_SYMBOL = '"';

  DIRECTORIES_KEYWORD = 'DIRECTORIES';
  DIRECTORY_KEYWORD = 'DIRECTORY';
  IMAGES_KEYWORD = 'IMAGES';
  DELAYS_KEYWORD = 'DELAYS';
  DELAY_KEYWORD = 'DELAY';
  DEFAULTS_KEYWORD = 'DEFAULTS';
  UNITS_KEYWORD = 'UNITS';
  BRANCH_KEYWORD = 'BRANCH';
  END_KEYWORD = 'END';
  WAIT_KEYWORD = 'WAIT';
  STOP_KEYWORD = 'STOP';
  CHOOSE_KEYWORD = 'CHOOSE';
  BY_KEYWORD = 'BY';
  FROM_KEYWORD = 'FROM';
  SWITCH_KEYWORD = 'SWITCH';
  USE_KEYWORD = 'USE';
  SET_KEYWORD = 'SET';
  WALLPAPER_KEYWORD = 'WALLPAPER';
  STYLE_KEYWORD = 'STYLE';
  WITH_KEYWORD = 'WITH';
  PROBABILITY_KEYWORD = 'PROBABILITY';
  FOR_KEYWORD = 'FOR';
  TIMES_KEYWORD = 'TIMES';
  TO_KEYWORD = 'TO';
  ORDERED_KEYWORD = 'ORDERED';
  RECURSIVE_KEYWORD = 'RECURSIVE';

  WEIGHT_KEYWORD = 'WEIGHT';
  SEASON_KEYWORD = 'SEASON';
  WEEKDAY_KEYWORD = 'WEEKDAY';
  MONTH_KEYWORD = 'MONTH';
  DATE_KEYWORD = 'DATE';
  TIME_KEYWORD = 'TIME';
  DATETIME_KEYWORD = 'DATETIME';

  SEASONS : Array[1..4] of String = (
    'WINTER', 'SPRING', 'SUMMER', 'AUTUMN'
  );
  MONTHS : Array[1..12] of String = (
    'JANUARY', 'FEBRUARY', 'MARCH', 'APRIL', 'MAY', 'JUNE',
    'JULY', 'AUGUST','SEPTEMBER', 'OCTOBER', 'NOVEMBER', 'DECEMBER'
  );
  DAYS_OF_WEEK : Array[1..7] of String = (
    'SUNDAY', 'MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY', 'FRIDAY', 'SATURDAY'
  );

  WPC_DATE_SEPARATOR = '.';
  WPC_TIME_SEPARATOR = ':';
  WPC_DATE_TIME_SEPARATOR = '-';
type

  { TWpcScriptParser }

  TWpcScriptParser = class(TObject)
  private
    type
      TMapStringString = specialize TFPGMap<String, String>;
      TStatementPropertiesSet = set of TWpcStatementPropertyId;
      TStatementProperties = record
        Times : LongWord;
        Probability : Byte;
        Delay : LongWord;
        Style : TWpcWallpaperStyle;
      end;

  private
    FScript : TWpcScript;

    FLines : TStringList;
    FCurrentLine : Integer;

    FCheckScriptResources : Boolean;

    FDirsVariables   : TMapStringString;
    FImagesVariables : TMapStringString;
    FDelaysVariables : TMapStringString;

    FDefaultDelay           : LongWord;
    FDefaultDelayUnits      : TWpcTimeMeasurementUnits;
    FDefaultWallpaperStyle  : TWpcWallpaperStyle;
    FBasePath               : String; // used to resolve relative paths

    FComputedDefaultDelay : LongWord;
  private
    procedure SetBasePath(Path : String);
  public
    property CheckScriptResources : Boolean read FCheckScriptResources write FCheckScriptResources;
    property BasePath : String read FBasePath write SetBasePath;
  public
    constructor Create(ScriptLines : TStringList);
    destructor Destroy(); override;
  public
    function Parse() : TWpcScript;

    function GetDirPath(DirVariableName : String) : String;
    function GetImage(ImageVariableName : String) : String;
    function GetDelay(DelayVariableName : String) : String;

    function GetDefaultDelay() : LongWord;
    function GetDefaultDelayUnits() : TWpcTimeMeasurementUnits;
    function GetDefaultWallpaperStyle() : TWpcWallpaperStyle;
  private
    procedure ParseHeaders();
    procedure ParseDirsVariables();
    procedure ParseImagesVariables();
    procedure ParseDaleyVariables();
    procedure ParseDefaults();
    procedure ParseBranches();
    procedure Validate();

    procedure ParseVariableDefinition(Line : String; out VariableName : String; out VariableValue : String);
    function ApplyParsedDirectoriesVariables(VariableValue : String) : String;
    function ApplyParsedImagesVariables(Image : String) : String;
    function ApplyParsedDelayVariables(Delay : String) : String;

    function ParseNextBranch() : TWpcBranchStatement;

    function ParseWaitStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
    function ParseWallpaperStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
    function ParseDirectoryStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
    function ParseStopStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
    function ParseSwitchBranchStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
    function ParseUseBranchStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
    function ParseWallpaperChooserStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
    function ParseBranchToUseChooserStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
    function ParseBranchToSwitchChooserStatement(LineWords : TStringList) : IWpcBaseScriptStatement;

    function ParseWallpaperStatementData(LineWords : TStringList; Index : Integer) : TWpcWallpaperStatement;
    function ParseDirectoryStatementData(LineWords : TStringList; Index : Integer) : TWpcDirectoryStatement;
    function ParseSwitchBranchStatementData(LineWords : TStringList; Index : Integer) : TWpcSwitchBranchStatement;
    function ParseUseBranchStatementData(LineWords : TStringList; Index : Integer) : TWpcUseBranchStatement;

    function ParseDirectory(LineWords : TStringList; var Index : Integer) : String;
    function ParseImage(LineWords : TStringList; var Index : Integer) : TWpcImage;

    function GetDefaultStatementProperties() : TStatementProperties;
    function ParseStatementProperties(AllowedProperties : TStatementPropertiesSet; LineWords : TStringList; var Index : Integer) : TStatementProperties;
    function ParseStatementPropertiesAndEnsureEndOfLine(AllowedProperties : TStatementPropertiesSet; LineWords : TStringList; Index : Integer) : TStatementProperties;

    function ParseDelayProperty(LineWords : TStringList; WordIndex : Integer) : Integer;
    function ParseTimesProperty(LineWords : TStringList; WordIndex : Integer) : Integer;
    function ParseProbabilityProperty(LineWords : TStringList; WordIndex : Integer) : Byte;
    function ParseWallpaperStyleProperty(LineWords : TStringList; WordIndex : Integer) : TWpcWallpaperStyle;

    function ParseAndRemoveSelector(LineWords : TStringList; Selector : TWpcSelector) : TWpcSelectorValueHolder;
    function ParseSequentialNumberWithAlias(Value : String; Aliases : Array of String) : Integer;

    function ParseDelayValue(Delay : String) : LongWord;
    function ParseDelayMeasurmentUnitsValue(MeasurementUnits : String) : TWpcTimeMeasurementUnits;
    function ParseProbabilityValue(Probability : String) : Byte;
    function ParseTimesValue(Times : String) : LongWord;
    function ParseWallpaperStyleValue(Style : String) : TWpcWallpaperStyle;
    function ParseSelectorValue(Selector : String) : TWpcSelector;
    function ParseWeightValue(Weight : String) : LongWord;
    function ParseSeasonValue(Season : String) : Integer;
    function ParseWeekdayValue(Weekday : String) : Integer;
    function ParseMonthValue(Month : String) : Integer;
    function ParseDateValue(Date : String) : TDateTime;
    function ParseTimeValue(Time : String) : TDateTime;
    function ParseDateTimeValue(DateTime : String) : TDateTime;

    function ValidateName(Name : String) : Boolean;
    procedure ValidateBranchNames();

    function GetLine(LineNumber : Integer) : String;
    function GetNextLine() : String;
    function SplitLine(Line : String) : TStringList;
    function ProbeStatementType(LineWords : TStringList) : TWpcStatemetId;
    function ProbePropertyType(LineWords : TStringList; WordIndex : Integer) : TWpcStatementPropertyId;

    function IsWordANumber(AWord : String) : Boolean;
    function CheckKeyWord(Given : String; Expected : String) : Boolean;
    procedure EnsureKeyWord(LineWords : TStringList; Index : Integer; ExpectedKeyWord : String);
    procedure EnsureKeyWordsLine(GivenKeyWords : TStringList; ExpectedKeyWords : Array of String);
    procedure EnsureKeyWordsLineAndFreeResources(GivenKeyWords : TStringList; ExpectedKeyWords : Array of String);
    procedure EnsureSelectorKeyword(LineWords : TStringList; SelectorKeyword : String);
    procedure EnsureEndOfLine(LineWords : TStringList; WordIndex : Integer);

    function SafeGet(LineWords : TStringList; Index : Integer) : String;

    procedure SearchWordInScript(TheWord : String; CaseSensitive : Boolean; var Line : Integer; var Index : Integer);

    function ToAbsolutePath(Path : String) : String;
  end;

implementation

{ TWpcScriptParser }

constructor TWpcScriptParser.Create(ScriptLines : TStringList);
begin
  FLines := ScriptLines;
  FScript := TWpcScript.Create();

  FDirsVariables := TMapStringString.Create();
  FImagesVariables := TMapStringString.Create();
  FDelaysVariables := TMapStringString.Create();

  FDefaultDelay := 5;
  FDefaultDelayUnits := MINUTES;
  FDefaultWallpaperStyle := CENTERED;
  FBasePath := '';

  FComputedDefaultDelay := ConvertToMilliseconds(FDefaultDelay, FDefaultDelayUnits);

  FCheckScriptResources := True;
end;

destructor TWpcScriptParser.Destroy();
begin
  FDirsVariables.Free();
  FImagesVariables.Free();
  FDelaysVariables.Free();

  inherited Destroy();
end;

{
  Parses Wallpaper Changer script.
  The script has following format:
  <Headers section>
  <Branches section>
  Where:
   - headers section is optional.
   - branches section must have Main branch.
  Comments are allowed. A comment line has to start with # symbol.
  Syntax:

  # Headers section
  # Each part of headers section isn't mandatory.

  DEFAULTS
    [<key> <value>]
    ...
  END DEFAULTS

  DIRECTORIES
    [<key> <value>]
    ...
  END DIRECTORIES

  IMAGES
    [<key> <value>]
    ...
  END IMAGES

  DELAYS
    [<key> <value>]
    ...
  END DELAYS

  # Branches section

  BRANCH Main
    <Statement>
    ...
  END BRANCH

  # Other branches are optional

  BRANCH <name>
    <Statement>
    ...
  END BRANCH
}
function TWpcScriptParser.Parse() : TWpcScript;
begin
  FCurrentLine := -1;

  ParseHeaders();
  ParseBranches();

  Validate();

  Result := FScript;
end;

{
  Returns directory variable value or empty string if variable with given name doesn't exist.
}
function TWpcScriptParser.GetDirPath(DirVariableName : String) : String;
var
  Index : Integer;
begin
  Index := FDirsVariables.IndexOf(DirVariableName);
  if (Index <> -1) then
    Result := FDirsVariables.Data[Index]
  else
    Result := '';
end;

{
  Returns image variable value or empty string if variable with given name doesn't exist.
}
function TWpcScriptParser.GetImage(ImageVariableName : String) : String;
var
  Index : Integer;
begin
  Index := FImagesVariables.IndexOf(ImageVariableName);
  if (Index <> -1) then
    Result := FImagesVariables.Data[Index]
  else
    Result := '';
end;

{
  Returns delay variable value or empty line if variable with given name doesn't exist.
}
function TWpcScriptParser.GetDelay(DelayVariableName : String) : String;
var
  Index : Integer;
begin
  Index := FDelaysVariables.IndexOf(DelayVariableName);
  if (Index <> -1) then
    Result := FDelaysVariables.Data[Index]
  else
    Result := '';
end;

function TWpcScriptParser.GetDefaultDelay() : LongWord;
begin
  Result := FDefaultDelay;
end;

function TWpcScriptParser.GetDefaultDelayUnits() : TWpcTimeMeasurementUnits;
begin
  Result := FDefaultDelayUnits;
end;

function TWpcScriptParser.GetDefaultWallpaperStyle() : TWpcWallpaperStyle;
begin
  Result := FDefaultWallpaperStyle;
end;

{
  Parses script headers i.e. directories, images, delays, default settings.
}
procedure TWpcScriptParser.ParseHeaders();
var
  LineWords         : TStringList;
  AWord             : String;
  ParseSettingsFlag : Boolean;
begin
  ParseSettingsFlag := True;
  while (ParseSettingsFlag) do begin
    LineWords := SplitLine(GetNextLine());
    AWord := UpperCase(SafeGet(LineWords, 0));
    LineWords.Free();
    Dec(FCurrentLine); // Returns current line into queue for future analyzing
    case (AWord) of
      DIRECTORIES_KEYWORD:
        ParseDirsVariables();
      IMAGES_KEYWORD:
        ParseImagesVariables();
      DELAYS_KEYWORD:
        ParseDaleyVariables();
      DEFAULTS_KEYWORD:
        ParseDefaults();
      BRANCH_KEYWORD:
        ParseSettingsFlag := False; // End of settings sections, branches section is reached.
      else
        raise TWpcScriptParseException.Create('Unexpected word "' + AWord + '".', FCurrentLine, 0);
    end;
  end;
end;

{
  Syntax:
  DIRECTORIES
     [<Directory name> <Path>]
     ...
  END DIRECTORIES
  Note, path could be absolute or relative, but result value is always absolete path.
  Also path could contain previous directories variable at the begining.
}
procedure TWpcScriptParser.ParseDirsVariables();
var
  Line                 : String;
  LineWords            : TStringList;
  Key                  : String;
  Value                : String;
  ParseDirectoriesFlag : Boolean;
begin
  EnsureKeyWordsLineAndFreeResources(SplitLine(GetNextLine()), [DIRECTORIES_KEYWORD]);

  ParseDirectoriesFlag := True;
  while (ParseDirectoriesFlag) do begin
    try
      Line := GetNextLine();
      LineWords := SplitLine(Line);
      if (not CheckKeyWord(SafeGet(LineWords, 0), END_KEYWORD)) then begin
        ParseVariableDefinition(Line, Key, Value);
        Value := ApplyParsedDirectoriesVariables(Value);
        Value := ToAbsolutePath(Value);
        FDirsVariables.Add(Key, Value);
      end
      else begin
        EnsureKeyWordsLine(LineWords, [END_KEYWORD, DIRECTORIES_KEYWORD]);
        ParseDirectoriesFlag := False;
      end;
    finally
      LineWords.Free();
    end;
  end;
end;

{
  Syntax:
  IMAGES
     [<Image name> <Path to image>]
     ...
  END IMAGES
  Note, path to image could be absolute or relative, but result value is always absolete path.
  Also path to image could contain directory variable at the begining.
}
procedure TWpcScriptParser.ParseImagesVariables();
var
  Line            : String;
  LineWords       : TStringList;
  Key             : String;
  Value           : String;
  ParseImagesFlag : Boolean;
begin
  EnsureKeyWordsLineAndFreeResources(SplitLine(GetNextLine()), [IMAGES_KEYWORD]);

  ParseImagesFlag := True;
  while (ParseImagesFlag) do begin
    try
      Line := GetNextLine();
      LineWords := SplitLine(Line);
      if (not CheckKeyWord(SafeGet(LineWords, 0), END_KEYWORD)) then begin
        ParseVariableDefinition(Line, Key, Value);
        Value := ApplyParsedDirectoriesVariables(Value);
        Value := ToAbsolutePath(Value);
        FImagesVariables.Add(Key, Value);
      end
      else begin
        EnsureKeyWordsLine(LineWords, [END_KEYWORD, IMAGES_KEYWORD]);
        ParseImagesFlag := False;
      end;
    finally
      LineWords.Free();
    end;
  end;
end;

{
  Syntax:
  DELAYS
    <Delay name> <Time>
  END DELAYS
  Where:
   - Time: <n[Time unit]>
   - Time unit: <ms|s|m|h|d>
}
procedure TWpcScriptParser.ParseDaleyVariables();
var
  Line            : String;
  LineWords       : TStringList;
  Key             : String;
  Value           : String;
  ParseDelaysFlag : Boolean;
begin
  EnsureKeyWordsLineAndFreeResources(SplitLine(GetNextLine()), [DELAYS_KEYWORD]);

  ParseDelaysFlag := True;
  while (ParseDelaysFlag) do begin
    try
      Line := GetNextLine();
      LineWords := SplitLine(Line);
      if (not CheckKeyWord(SafeGet(LineWords, 0), END_KEYWORD)) then begin
        ParseVariableDefinition(Line, Key, Value);
        FDelaysVariables.Add(Key, Value);
      end
      else begin
        EnsureKeyWordsLine(LineWords, [END_KEYWORD, DELAYS_KEYWORD]);
        ParseDelaysFlag := False;
      end;
    finally
      LineWords.Free();
    end;
  end;
end;

{
  Syntax:
  DEFAULTS
    [DELAY <Time>]
    [DELAY UNITS <Time unit>]
    [WALLPAPER STYLE <Style>]
    [DIRECTORY <Path>]
  END DEFAULTS
  Where:
   - Time: <n[Time unit]>
   - Time unit: <ms|s|m|h|d>
   - Style: one of styles from TWpcWallpaperStyle
     Note, that each system has support for some subset of the styles.
     In case when a style is not supported by system, default style should be used.
   - directory: absolute or ralative path to base images dir, e.g. C:\Wallpapers
     Note, this prefix path is added only for relative paths.
     If relative value given, base path is the path to the executable.
   Note, that:
    - Each value has its default in constructor.
    - Default delay is used in case of WAIT
    - Default delay units is used in case of WAIT X
}
procedure TWpcScriptParser.ParseDefaults();
var
  LineWords         : TStringList;
  AWord             : String;
  CurrentWordIndex  : Integer;
  ParseDefaultsFlag : Boolean;
begin
  EnsureKeyWordsLineAndFreeResources(SplitLine(GetNextLine()), [DEFAULTS_KEYWORD]);

  try
    ParseDefaultsFlag := True;
    while (ParseDefaultsFlag) do begin
      LineWords := SplitLine(GetNextLine());
      CurrentWordIndex := 0;
      case (SafeGet(LineWords, CurrentWordIndex)) of
        DELAY_KEYWORD:
          begin
            Inc(CurrentWordIndex);
            if (CheckKeyWord(SafeGet(LineWords, CurrentWordIndex), UNITS_KEYWORD)) then begin
              Inc(CurrentWordIndex);

              AWord := SafeGet(LineWords, CurrentWordIndex);
              if (AWord = '') then
                raise TWpcScriptParseException.Create('Delay measurment unit is expected, but nothing found.', FCurrentLine, CurrentWordIndex);

              FDefaultDelayUnits := ParseDelayMeasurmentUnitsValue(AWord);
            end
            else begin
              AWord := SafeGet(LineWords, CurrentWordIndex);
              if (AWord = '') then
                raise TWpcScriptParseException.Create('Default delay is expected, but nothing found.', FCurrentLine, CurrentWordIndex);

              FDefaultDelay := ParseDelayValue(AWord);
            end;

            Inc(CurrentWordIndex);
            EnsureEndOfLine(LineWords, CurrentWordIndex);
          end;
        WALLPAPER_KEYWORD:
          begin
            Inc(CurrentWordIndex);
            if (not CheckKeyWord(SafeGet(LineWords, CurrentWordIndex), STYLE_KEYWORD)) then
              raise TWpcScriptParseException.Create(STYLE_KEYWORD + ' keyword expected, but got "' + SafeGet(LineWords, CurrentWordIndex) + '".', FCurrentLine, CurrentWordIndex);

            Inc(CurrentWordIndex);
            AWord := SafeGet(LineWords, CurrentWordIndex);
            if (AWord = '') then
              raise TWpcScriptParseException.Create('Wallpaper style is expected, but nothing found.', FCurrentLine, CurrentWordIndex);

            FDefaultWallpaperStyle := ParseWallpaperStyleValue(AWord);

            Inc(CurrentWordIndex);
            EnsureEndOfLine(LineWords, CurrentWordIndex);
          end;
        DIRECTORY_KEYWORD:
          begin
            Inc(CurrentWordIndex);
            AWord := SafeGet(LineWords, CurrentWordIndex);
            if (AWord = '') then
              raise TWpcScriptParseException.Create('Path to base directory is expected, but nothing found.', FCurrentLine, CurrentWordIndex);

            SetBasePath(ParseDirectory(LineWords, CurrentWordIndex));

            Inc(CurrentWordIndex);
            EnsureEndOfLine(LineWords, CurrentWordIndex);
          end;
        END_KEYWORD:
          begin
            EnsureKeyWordsLine(LineWords, [END_KEYWORD, DEFAULTS_KEYWORD]);
            ParseDefaultsFlag := False;
          end
        else
          raise TWpcScriptParseException.Create('Unexpected word "' + SafeGet(LineWords, CurrentWordIndex) + '".', FCurrentLine);
      end;

      FreeAndNil(LineWords);
    end;
  finally
    if (LineWords <> nil) then
      LineWords.Free();
  end;
end;

procedure TWpcScriptParser.ParseBranches();
var
  Branch     : TWpcBranchStatement;
  NextBranch : Boolean;
begin
  NextBranch := True;
  while (NextBranch) do begin
    Branch := ParseNextBranch();
    if (Branch <> nil) then
      FScript.AddBranch(Branch.GetName(), Branch)
    else
      NextBranch := False;
  end;
end;

{
  Checks that references in the script are valid.
}
procedure TWpcScriptParser.Validate();
begin
  if (FScript.GetBranch(MAIN_BARNCH) = nil) then
    raise TWpcScriptParseException.Create('Entry point "' + MAIN_BARNCH + '" branch not found.');
  if (FScript.GetBranch(MAIN_BARNCH).GetBranchStatements().Count = 0) then
    raise TWpcScriptParseException.Create('Entry point branch "' + MAIN_BARNCH + '" cannot be empty.');

  ValidateBranchNames();
end;

{
  Syntax:
  <Variable name> <Variable value>
  Note, that:
   - Variable value could contain spaces.
   - Variable name and Variable value could be separated with many whitespaces.
}
procedure TWpcScriptParser.ParseVariableDefinition(Line : String; out VariableName : String; out VariableValue : String);
var
  ParsedVariableName : String;
  ParsedVarialeValue : String;
  i                  : Integer;
  Len                : Integer;
begin
  Line := TrimSet(FLines[FCurrentLine], WHITESPACE_SET);
  Len := Length(Line);

  i := 0;
  while ((i < Len) and (not (Line[i] in WHITESPACE_SET))) do
    Inc(i);

  ParsedVariableName := copy(Line, 0, i-1);
  if (not ValidateName(ParsedVariableName)) then
    raise TWpcScriptParseException.Create('Valid variable name is expectd, but "' + ParsedVariableName + '" found.', FCurrentLine);

  while ((i < Len) and (Line[i] in WHITESPACE_SET)) do
    Inc(i);
  if (i = Len) then
    raise TWpcScriptParseException.Create('Variable "' + ParsedVariableName + '" should have a value', FCurrentLine);

  ParsedVarialeValue := copy(Line, i, Len - i + 1);

  VariableName := ParsedVariableName;
  VariableValue := ParsedVarialeValue;
end;

{
  Replaces Directory variable with its value if a variable exists in the given value.
  Syntax:
    <[$<Directory variable>]Rest of variable value>
  Example:
    $WALLPAPERS/lake.png
    will be translated in
    /home/user/Pictures/Wallpapers/lake.png
  Note, that end of variable name is file separator (e.g. / or \)
}
function TWpcScriptParser.ApplyParsedDirectoriesVariables(VariableValue : String) : String;
var
  i                     : Integer;
  VariableNameStartPos  : Integer;
  Len                   : Integer;
  DirectoryVariableName : String;
  ResolvedVariableValue : String;
begin
  if (VariableValue[1] <> VARIABLE_START_SYMBOL) then begin
    Result := VariableValue;
    exit;
  end;

  i := Length(VARIABLE_START_SYMBOL) + 1;
  Len := Length(VariableValue);
  if (i > Len) then
    raise TWpcScriptParseException.Create('Variable name is expected.', FCurrentLine);

  VariableNameStartPos := i;
  while ((i <= Len) and (VariableValue[i] <> PATH_SEPARATOR)) do begin
    Inc(i);
  end;
  DirectoryVariableName := copy(VariableValue, VariableNameStartPos, i - VariableNameStartPos);

  ResolvedVariableValue := GetDirPath(DirectoryVariableName);
  if (ResolvedVariableValue = '') then
    raise TWpcScriptParseException.Create('Unknown directory variable "' + DirectoryVariableName + '".', FCurrentLine);

  if (i = Len) then
    Result := ResolvedVariableValue
  else
    Result := Concat(ResolvedVariableValue, copy(VariableValue, i, Len - i + 1));
end;

{
  Returns images variable value or given value if no variable found.
}
function TWpcScriptParser.ApplyParsedImagesVariables(Image : String) : String;
var
  ResolvedImage : String;
begin
  if (Image[1] <> VARIABLE_START_SYMBOL) then begin
    Result := Image;
    exit;
  end;

  Delete(Image, 1, Length(VARIABLE_START_SYMBOL));
  ResolvedImage := GetImage(Image);
  if (ResolvedImage = '') then
    raise TWpcScriptParseException.Create('Unknown image variable "' + Image + '".', FCurrentLine);

  Result := ResolvedImage;
end;

{
  Returns delay variable value or given value if no variable found.
}
function TWpcScriptParser.ApplyParsedDelayVariables(Delay : String) : String;
var
  ResolvedDelay : String;
begin
  if (Delay[1] <> VARIABLE_START_SYMBOL) then begin
    Result := Delay;
    exit;
  end;

  Delete(Delay, 1, Length(VARIABLE_START_SYMBOL));
  ResolvedDelay := GetDelay(Delay);
  if (ResolvedDelay = '') then
    raise TWpcScriptParseException.Create('Unknown delay variable "' + Delay + '".', FCurrentLine);

  Result := ResolvedDelay;
end;

{
  Syntax:
  BRANCH <Name>
    [Statements]
  END BRANCH

  Returns branch or nil if end of script reached.
}
function TWpcScriptParser.ParseNextBranch() : TWpcBranchStatement;
var
  Branch : TWpcBranchStatement;

  BranchHeader : TStringList;
  BranchName   : String;

  Line            : String;
  LineWords       : TStringList;
  StatementId     : TWpcStatemetId;
  BranchStatement : IWpcBaseScriptStatement;
begin
  Line := GetNextLine();
  if (Line = END_OF_SCRIPT) then begin
    Result := nil;
    exit;
  end;

  BranchHeader := SplitLine(Line);
  try
    EnsureKeyWord(BranchHeader, 0, BRANCH_KEYWORD);

    if (BranchHeader.Count < 2) then
      raise TWpcScriptParseException.Create('Branch name expected.', FCurrentLine);

    BranchName := BranchHeader[1];
    if (not ValidateName(BranchName)) then
      raise TWpcScriptParseException.Create('Branch name contains not allowed characters.', FCurrentLine, 2);

    if (FScript.GetBranch(BranchName) <> nil) then
      raise TWpcScriptParseException.Create('Duplicate branch name "' + BranchName + '".', FCurrentLine, 2);

    if (BranchHeader.Count > 2) then
      raise TWpcScriptParseException.Create('Unexpected word "' + BranchHeader[2] + '" after branch name.', FCurrentLine, 3);
  finally
    BranchHeader.Free();
  end;

  Branch := TWpcBranchStatement.Create(BranchName);
  try
    while (True) do begin
      LineWords := SplitLine(GetNextLine());
      StatementId := ProbeStatementType(LineWords);
      case (StatementId) of
        WPC_WAIT_STATEMENT_ID:
          BranchStatement := ParseWaitStatement(LineWords);
        WPC_WALLPAPER_STATEMENT_ID:
          BranchStatement := ParseWallpaperStatement(LineWords);
        WPC_DIRECTORY_STATEMENT_ID:
          BranchStatement := ParseDirectoryStatement(LineWords);
        WPC_STOP_STATEMENT_ID:
          BranchStatement := ParseStopStatement(LineWords);
        WPC_SWITCH_BRANCH_STATEMENT_ID:
          BranchStatement := ParseSwitchBranchStatement(LineWords);
        WPC_USE_BRANCH_STATEMENT_ID:
          BranchStatement := ParseUseBranchStatement(LineWords);
        WPC_WALLPAPER_CHOOSER_STATEMENT_ID:
          BranchStatement := ParseWallpaperChooserStatement(LineWords);
        WPC_BRANCH_TO_USE_CHOOSER_STATEMENT_ID:
          BranchStatement := ParseBranchToUseChooserStatement(LineWords);
        WPC_BRANCH_TO_SWITCH_CHOOSER_STATEMENT_ID:
          BranchStatement := ParseBranchToSwitchChooserStatement(LineWords);
        WPC_END_OF_BLOCK_STATEMENT:
          break;
        else
          raise TWpcScriptParseException.Create('Unknown statement: ' + LineWords.CommaText, FCurrentLine);
      end;
      LineWords.Free();
      Branch.AddStatement(BranchStatement);
    end;
  except
    LineWords.Free();
    Branch.Free();
    raise;
  end;

  Result := Branch;
end;

{
  Syntax:
  WAIT [FOR <Time> | <Time>] [WITH PROBABILITY <0-100>] [<1-n> TIMES]
  Note:
   - when Time unit and/or Time isn't set, then default values should be used.
   - Time could be a delay variable
}
function TWpcScriptParser.ParseWaitStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
  function CheckAndParseDelayValue(LineWords : TStringList; Index : Integer) : LongWord;
  var
    AWord : String;
  begin
    AWord := SafeGet(LineWords, Index);
    if (AWord = '') then
      raise TWpcScriptParseException.Create('Delay value is expected.', FCurrentLine, Index);

    Result := ParseDelayValue(ApplyParsedDelayVariables(AWord));
  end;

var
  WaitStatement       : TWpcWaitStatement;
  Delay               : LongWord;
  StatementProperties : TStatementProperties;
  CurrentWordIndex    : Integer;
begin
  Delay := FComputedDefaultDelay;
  StatementProperties := GetDefaultStatementProperties();

  CurrentWordIndex := 0;
  EnsureKeyWord(LineWords, CurrentWordIndex, WAIT_KEYWORD);

  if (LineWords.Count <> 1) then begin
    Inc(CurrentWordIndex);

    if (CheckKeyWord(SafeGet(LineWords, CurrentWordIndex), FOR_KEYWORD)) then begin
      Inc(CurrentWordIndex);
      Delay := CheckAndParseDelayValue(LineWords, CurrentWordIndex);
    end
    else begin
      // Handle case: WAIT <A property> (default delay is supposed)
      if (ProbePropertyType(LineWords, CurrentWordIndex) = WPC_UNKNOWN_STATEMENT_PROPERTY) then
        Delay := CheckAndParseDelayValue(LineWords, CurrentWordIndex)
      else
        Dec(CurrentWordIndex); // Push back a property word
    end;

    Inc(CurrentWordIndex);
    StatementProperties := ParseStatementPropertiesAndEnsureEndOfLine(
      [WPC_PROBABILITY_STATEMENT_PROPERY_ID, WPC_TIMES_STATEMENT_PROPERY_ID],
      LineWords, CurrentWordIndex);
  end;

  WaitStatement := TWpcWaitStatement.Create(Delay);
  WaitStatement.SetProbability(StatementProperties.Probability);
  WaitStatement.SetTimes(StatementProperties.Times);
  Result := WaitStatement;
end;

{
  Syntax:
  SET WALLPAPER <File> [STYLE <Style>] [FOR <Time>] [WITH PROBABILITY <0-100>]
  Where:
    - File could be:
       * absolute or relative path to an image
       * $DirVariable/path/image.ext
       * $ImageVariable
    - Style: a value from TWpcWallpaperStyle
  When Time property is not set, then no delay.
}
function TWpcScriptParser.ParseWallpaperStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
var
  CurrentWordIndex : Integer;
begin
  CurrentWordIndex := 0;
  EnsureKeyWord(LineWords, CurrentWordIndex, SET_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, WALLPAPER_KEYWORD);

  Inc(CurrentWordIndex);
  Result := ParseWallpaperStatementData(LineWords, CurrentWordIndex);
end;

{
  Syntax:
  SET WALLPAPER FROM DIRECTORY <Path> [RECURSIVE] [ORDERED] [STYLE <Style>] [FOR <Time>] [WITH PROBABILITY <0-100>] [<1-n> TIMES]
  Where:
    - Directory could be:
       * absolute or relative path to a directory with wallpapers
       * $DirVariable
    - Recursive: flag. If set image from subdirectories will be used.
                 If not set only images within specified directory will be used.
    - Ordered: flag. If is set a list from images within the directory will be built
               and next wallpaper will be chosen as next item in the list;
               if not set then any wallpaper form the directory could be chosen including the same one.
    - Style: a value from TWpcWallpaperStyle
    - Time: specifies delay between wallpaper changes.
            When the property is not set, minimal delay will be used.
            It is recomended to always specify this property.
    - Times: defines how many times new image will be set as a wallpaper.
}
function TWpcScriptParser.ParseDirectoryStatement(LineWords : TStringList): IWpcBaseScriptStatement;
var
  CurrentWordIndex : Integer;
begin
  CurrentWordIndex := 0;
  EnsureKeyWord(LineWords, CurrentWordIndex, SET_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, WALLPAPER_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, FROM_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, DIRECTORY_KEYWORD);

  Inc(CurrentWordIndex);
  Result := ParseDirectoryStatementData(LineWords, CurrentWordIndex);

end;

{
  Syntax:
  STOP [WITH PROBABILITY <0-100>]
}
function TWpcScriptParser.ParseStopStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
var
  StopStatement       : TWpcStopStatement;
  StatementProperties : TStatementProperties;
  CurrentWordIndex    : Integer;
begin
  CurrentWordIndex := 0;
  EnsureKeyWord(LineWords, CurrentWordIndex, STOP_KEYWORD);

  Inc(CurrentWordIndex);
  StatementProperties := ParseStatementPropertiesAndEnsureEndOfLine(
    [WPC_PROBABILITY_STATEMENT_PROPERY_ID],
    LineWords, CurrentWordIndex);

  StopStatement := TWpcStopStatement.Create();
  StopStatement.SetProbability(StatementProperties.Probability);
  Result := StopStatement;
end;

{
  Syntax:
  SWITCH TO BRANCH <Name> [WITH PROBABILITY <0-100>]
}
function TWpcScriptParser.ParseSwitchBranchStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
var
  CurrentWordIndex : Integer;
begin
  CurrentWordIndex := 0;
  EnsureKeyWord(LineWords, CurrentWordIndex, SWITCH_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, TO_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, BRANCH_KEYWORD);

  Inc(CurrentWordIndex);
  Result := ParseSwitchBranchStatementData(LineWords, CurrentWordIndex);
end;

{
  Syntax:
  USE BRANCH <Name> [WITH PROBABILITY <0-100>] [<1-n> TIMES]
}
function TWpcScriptParser.ParseUseBranchStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
var
  CurrentWordIndex : Integer;
begin
  CurrentWordIndex := 0;
  EnsureKeyWord(LineWords, CurrentWordIndex, USE_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, BRANCH_KEYWORD);

  Inc(CurrentWordIndex);
  Result := ParseUseBranchStatementData(LineWords, CurrentWordIndex);
end;

{
  Syntax:
  CHOOSE WALLPAPER [BY <Selector>] FROM
    <Wallpaper 1 properties> <Selector> <Selector Value 1>
    <Wallpaper 2 properties> <Selector> <Selector Value 2>
    ...
  END CHOOSE
  Where:
   - Wallpaper n properties: the same as in Wallpaper statement, but without SET WALLPAPER prefix.
   - Selectors and its values:
      * WEIGHT <1-n>
      * SEASON <WINTER|SPRING|SAMMER|AUTUMN>
      * MOUNTH <JANUARY|...|DECEMBER | 1|...|12>
      * DATE <1-31>.<1-12>
      * TIME <0-23:0-59[:0-59]>
      * DATETIME <1-31>.<1-12>-<0-23:0-59[:0-59]>
   Note, that:
     - A chooser has to have at least two options.
     - Default selector is WEIGHT.
     - In case of WEIGHT selector (only) the WEIGHT keyword and its value is not mandatory. Default weight is 1
       Example of an option with WEIGHT selector:
       $MyWallpapers/nature/forest.jpg STYLE STRETCH FOR 8m
     - In case of non WEIGHT selector options are circled, for example, for chooser:
       CHOOSE WALLPAPER BY TIME FROM
         $RushHourCity TIME 6:30
         $NightCity TIME 21:00
       END CHOOSE
       which was invoked at 2:00 the second option will be executed.
}
function TWpcScriptParser.ParseWallpaperChooserStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
var
  WallpaperChooserStatement     : TWpcWallpaperChooserStatement;
  WallpaperChooserStatementItem : TWpcWallpaperStatement;
  Selector                      : TWpcSelector;
  SelectorValue                 : TWpcSelectorValueHolder;
  AWord                         : String;
  ParseChooserItemFlag          : Boolean;
  ChooserItemsCounter           : Integer;
  CurrentWordIndex              : Integer;
begin
  Selector := S_WEIGHT;

  // Parse chooser header
  CurrentWordIndex := 0;
  EnsureKeyWord(LineWords, CurrentWordIndex, CHOOSE_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, WALLPAPER_KEYWORD);

  Inc(CurrentWordIndex);
  if (CheckKeyWord(SafeGet(LineWords, CurrentWordIndex), BY_KEYWORD)) then begin
    Inc(CurrentWordIndex);
    AWord := SafeGet(LineWords, CurrentWordIndex);
    if (AWord = '') then
      raise TWpcScriptParseException.Create('Chooser selector expected, but nothing found.', FCurrentLine, CurrentWordIndex);

    Selector := ParseSelectorValue(AWord);
  end
  else
    Dec(CurrentWordIndex);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, FROM_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureEndOfLine(LineWords, CurrentWordIndex);

  WallpaperChooserStatement := TWpcWallpaperChooserStatement.Create(Selector);
  try
    // Parse chooser items
    ChooserItemsCounter := 0;
    try
      ParseChooserItemFlag := True;
      while (ParseChooserItemFlag) do begin
        LineWords := SplitLine(GetNextLine());
        if (CheckKeyWord(SafeGet(LineWords, 0), END_KEYWORD)) then begin
          ParseChooserItemFlag := False;
          Dec(FCurrentLine); // Push back for future analyzing.
        end
        else begin
          SelectorValue := ParseAndRemoveSelector(LineWords, Selector);
          WallpaperChooserStatementItem := ParseWallpaperStatementData(LineWords, 0);
          try
            WallpaperChooserStatement.AddItem(WallpaperChooserStatementItem, SelectorValue);
          except
            on E : TWpcException do
              raise TWpcScriptParseException.Create(E.Message, FCurrentLine);
          end;
          Inc(ChooserItemsCounter);
        end;
        FreeAndNil(LineWords);
      end;
    finally
      if (LineWords <> nil) then
        LineWords.Free();
    end;

    EnsureKeyWordsLineAndFreeResources(SplitLine(GetNextLine()), [END_KEYWORD, CHOOSE_KEYWORD]);

    if (ChooserItemsCounter < 2) then
      raise TWpcScriptParseException.Create('Chooser should contain at least 2 options.', FCurrentLine);
  except
    WallpaperChooserStatement.Free();
    raise;
  end;

  Result := WallpaperChooserStatement;
end;

{
  Syntax:
  CHOOSE BARNCH TO USE [BY <Selector>] FROM
    <Use branch 1 properties> <Selector> <Selector Value 1>
    <Use branch 2 properties> <Selector> <Selector Value 2>
    ...
  END CHOOSE
  Where:
   - Use branch properties: the same as in Use Branch statement but without USE BRANCH prefix.
   - Selectors and its values: the same as in Wallpaper Chooser statement.
}
function TWpcScriptParser.ParseBranchToUseChooserStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
var
  UseBranchChooserStatement     : TWpcUseBranchChooserStatement;
  UseBranchChooserStatementItem : TWpcUseBranchStatement;
  Selector                      : TWpcSelector;
  SelectorValue                 : TWpcSelectorValueHolder;
  AWord                         : String;
  ParseChooserItemFlag          : Boolean;
  ChooserItemsCounter           : Integer;
  CurrentWordIndex              : Integer;
begin
  Selector := S_WEIGHT;

  // Parse chooser header
  CurrentWordIndex := 0;
  EnsureKeyWord(LineWords, CurrentWordIndex, CHOOSE_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, BRANCH_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, TO_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, USE_KEYWORD);

  Inc(CurrentWordIndex);
  if (CheckKeyWord(SafeGet(LineWords, CurrentWordIndex), BY_KEYWORD)) then begin
    Inc(CurrentWordIndex);
    AWord := SafeGet(LineWords, CurrentWordIndex);
    if (AWord = '') then
      raise TWpcScriptParseException.Create('Chooser selector expected, but nothing found.', FCurrentLine, CurrentWordIndex);

    Selector := ParseSelectorValue(AWord);
  end
  else
    Dec(CurrentWordIndex);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, FROM_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureEndOfLine(LineWords, CurrentWordIndex);

  UseBranchChooserStatement := TWpcUseBranchChooserStatement.Create(Selector);
  try
    // Parse chooser items
    ChooserItemsCounter := 0;
    try
      ParseChooserItemFlag := True;
      while (ParseChooserItemFlag) do begin
        LineWords := SplitLine(GetNextLine());
        if (CheckKeyWord(SafeGet(LineWords, 0), END_KEYWORD)) then begin
          ParseChooserItemFlag := False;
          Dec(FCurrentLine); // Push back for future analyzing.
        end
        else begin
          SelectorValue := ParseAndRemoveSelector(LineWords, Selector);
          UseBranchChooserStatementItem := ParseUseBranchStatementData(LineWords, 0);
          try
            UseBranchChooserStatement.AddItem(UseBranchChooserStatementItem, SelectorValue);
          except
            on E : TWpcException do
              raise TWpcScriptParseException.Create(E.Message, FCurrentLine);
          end;
          Inc(ChooserItemsCounter);
        end;
        FreeAndNil(LineWords);
      end;
    finally
      if (LineWords <> nil) then
        LineWords.Free();
    end;

    EnsureKeyWordsLineAndFreeResources(SplitLine(GetNextLine()), [END_KEYWORD, CHOOSE_KEYWORD]);

    if (ChooserItemsCounter < 2) then
      raise TWpcScriptParseException.Create('Chooser should contain at least 2 options.', FCurrentLine);
  except
    UseBranchChooserStatement.Free();
    raise;
  end;

  Result := UseBranchChooserStatement;
end;

{
  Syntax:
  CHOOSE BARNCH TO SWITCH [BY <Selector>] FROM
    <Switch branch 1 properties> <Selector> <Selector Value 1>
    <Switch branch 2 properties> <Selector> <Selector Value 2>
    ...
  END CHOOSE
  Where:
   - Switch branch properties: the same as in Switch Branch statement but without SWITCH TO BRANCH prefix.
   - Selectors and its values: the same as in Wallpaper Chooser statement.
}
function TWpcScriptParser.ParseBranchToSwitchChooserStatement(LineWords : TStringList) : IWpcBaseScriptStatement;
var
  SwitchBranchChooserStatement     : TWpcSwitchBranchChooserStatement;
  SwitchBranchChooserStatementItem : TWpcSwitchBranchStatement;
  Selector                         : TWpcSelector;
  SelectorValue                    : TWpcSelectorValueHolder;
  AWord                            : String;
  ParseChooserItemFlag             : Boolean;
  ChooserItemsCounter              : Integer;
  CurrentWordIndex                 : Integer;
begin
  Selector := S_WEIGHT;

  // Parse chooser header
  CurrentWordIndex := 0;
  EnsureKeyWord(LineWords, CurrentWordIndex, CHOOSE_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, BRANCH_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, TO_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, SWITCH_KEYWORD);

  Inc(CurrentWordIndex);
  if (CheckKeyWord(SafeGet(LineWords, CurrentWordIndex), BY_KEYWORD)) then begin
    Inc(CurrentWordIndex);
    AWord := SafeGet(LineWords, CurrentWordIndex);
    if (AWord = '') then
      raise TWpcScriptParseException.Create('Chooser selector expected, but nothing found.', FCurrentLine, CurrentWordIndex);

    Selector := ParseSelectorValue(AWord);
  end
  else
    Dec(CurrentWordIndex);

  Inc(CurrentWordIndex);
  EnsureKeyWord(LineWords, CurrentWordIndex, FROM_KEYWORD);

  Inc(CurrentWordIndex);
  EnsureEndOfLine(LineWords, CurrentWordIndex);

  SwitchBranchChooserStatement := TWpcSwitchBranchChooserStatement.Create(Selector);
  try
    // Parse chooser items
    ChooserItemsCounter := 0;
    try
      ParseChooserItemFlag := True;
      while (ParseChooserItemFlag) do begin
        LineWords := SplitLine(GetNextLine());
        if (CheckKeyWord(SafeGet(LineWords, 0), END_KEYWORD)) then begin
          ParseChooserItemFlag := False;
          Dec(FCurrentLine); // Push back for future analyzing.
        end
        else begin
          SelectorValue := ParseAndRemoveSelector(LineWords, Selector);
          SwitchBranchChooserStatementItem := ParseSwitchBranchStatementData(LineWords, 0);
          try
            SwitchBranchChooserStatement.AddItem(SwitchBranchChooserStatementItem, SelectorValue);
          except
            on E : TWpcException do
              raise TWpcScriptParseException.Create(E.Message, FCurrentLine);
          end;
          Inc(ChooserItemsCounter);
        end;
        FreeAndNil(LineWords);
      end;
    finally
      if (LineWords <> nil) then
        LineWords.Free();
    end;

    EnsureKeyWordsLineAndFreeResources(SplitLine(GetNextLine()), [END_KEYWORD, CHOOSE_KEYWORD]);

    if (ChooserItemsCounter < 2) then
      raise TWpcScriptParseException.Create('Chooser should contain at least 2 options.', FCurrentLine);
  except
    SwitchBranchChooserStatement.Free();
    raise;
  end;

  Result := SwitchBranchChooserStatement;
end;

{
  Parses Wallpaper statement data.
  Syntax the same as for Wallpaper statement, but without SET WALLPAPER keywords:
  <File> [STYLE <Style>] [FOR <Time>] [WITH PROBABILITY <0-100>]
}
function TWpcScriptParser.ParseWallpaperStatementData(LineWords : TStringList; Index : Integer) : TWpcWallpaperStatement;
var
  WallpaperStatement  : TWpcWallpaperStatement;
  Image               : TWpcImage;
  StatementProperties : TStatementProperties;
  CurrentWordIndex    : Integer;
begin
  CurrentWordIndex := Index;

  Image := ParseImage(LineWords, CurrentWordIndex);
  try
    StatementProperties := ParseStatementPropertiesAndEnsureEndOfLine(
      [WPC_WALLPAPER_STYLE_PROPERTY_ID, WPC_PROBABILITY_STATEMENT_PROPERY_ID, WPC_DELAY_STATEMENT_PROPERY_ID],
      LineWords, CurrentWordIndex);
  except
    Image.Free();
    raise;
  end;

  WallpaperStatement := TWpcWallpaperStatement.Create(Image);
  WallpaperStatement.SetStyle(StatementProperties.Style);
  WallpaperStatement.SetProbability(StatementProperties.Probability);
  WallpaperStatement.SetDelay(StatementProperties.Delay);
  Result := WallpaperStatement;
end;

{
  Parses Directory statement data.
  Syntax the same as for Directory statement, but without SET WALLPAPER FROM DIRECTORY keywords:
  <Path> [RECURSIVE] [ORDERED] [STYLE <Style>] [FOR <Time>] [WITH PROBABILITY <0-100>] [<1-n> TIMES]
}
function TWpcScriptParser.ParseDirectoryStatementData(LineWords : TStringList; Index : Integer): TWpcDirectoryStatement;
var
  DirectoryStatement  : TWpcDirectoryStatement;
  DirectoryPath       : String;
  StatementProperties : TStatementProperties;
  IsOrdered           : Boolean;
  IsRecursive         : Boolean;
  CurrentWordIndex    : Integer;
begin
  CurrentWordIndex := Index;

  DirectoryPath := ParseDirectory(LineWords, CurrentWordIndex);

  if (CheckKeyWord(SafeGet(LineWords, CurrentWordIndex), ORDERED_KEYWORD)) then begin
    Inc(CurrentWordIndex);
    IsOrdered := True;
  end
  else
    IsOrdered := False;

  if (CheckKeyWord(SafeGet(LineWords, CurrentWordIndex), RECURSIVE_KEYWORD)) then begin
    Inc(CurrentWordIndex);
    IsRecursive := True;
  end
  else
    IsRecursive := False;

  StatementProperties := ParseStatementPropertiesAndEnsureEndOfLine(
    [WPC_WALLPAPER_STYLE_PROPERTY_ID, WPC_DELAY_STATEMENT_PROPERY_ID, WPC_PROBABILITY_STATEMENT_PROPERY_ID, WPC_TIMES_STATEMENT_PROPERY_ID],
    LineWords, CurrentWordIndex);

  DirectoryStatement := TWpcDirectoryStatement.Create(TWpcDirectory.Create(DirectoryPath), IsOrdered, IsRecursive);
  DirectoryStatement.SetStyle(StatementProperties.Style);
  DirectoryStatement.SetDelay(StatementProperties.Delay);
  DirectoryStatement.SetProbability(StatementProperties.Probability);
  DirectoryStatement.SetTimes(StatementProperties.Times);
  Result := DirectoryStatement;
end;

{
  Parses Switch Branch statement data.
  Syntax the same as for Switch Branch statement, but without SWITCH BRANCH keywords:
  <Name> [WITH PROBABILITY <0-100>]
}
function TWpcScriptParser.ParseSwitchBranchStatementData(LineWords : TStringList; Index : Integer) : TWpcSwitchBranchStatement;
var
  SwitchBranchStatement : TWpcSwitchBranchStatement;
  BranchName            : String;
  StatementProperties   : TStatementProperties;
  CurrentWordIndex      : Integer;
begin
  CurrentWordIndex := Index;
  BranchName := SafeGet(LineWords, CurrentWordIndex);
  if (BranchName = '') then
    raise TWpcScriptParseException.Create('Branch name expected.', FCurrentLine, CurrentWordIndex);
  if (not ValidateName(BranchName)) then
    raise TWpcScriptParseException.Create('Branch name contains not allowed characters.', FCurrentLine, CurrentWordIndex);

  Inc(CurrentWordIndex);
  StatementProperties := ParseStatementPropertiesAndEnsureEndOfLine(
    [WPC_PROBABILITY_STATEMENT_PROPERY_ID],
    LineWords, CurrentWordIndex);

  SwitchBranchStatement := TWpcSwitchBranchStatement.Create(BranchName);
  SwitchBranchStatement.SetProbability(StatementProperties.Probability);
  Result := SwitchBranchStatement;
end;

{
  Parses Use Branch statement data.
  Syntax the same as for Use Branch statement, but without USE BRANCH keywords:
  <Name> [WITH PROBABILITY <0-100>] [<1-n> TIMES]
}
function TWpcScriptParser.ParseUseBranchStatementData(LineWords : TStringList; Index : Integer) : TWpcUseBranchStatement;
var
  UseBranchStatement  : TWpcUseBranchStatement;
  BranchName          : String;
  StatementProperties : TStatementProperties;
  CurrentWordIndex    : Integer;
begin
  CurrentWordIndex := Index;
  BranchName := SafeGet(LineWords, CurrentWordIndex);
  if (BranchName = '') then
    raise TWpcScriptParseException.Create('Branch name expected.', FCurrentLine, CurrentWordIndex);
  if (not ValidateName(BranchName)) then
    raise TWpcScriptParseException.Create('Branch name contains not allowed characters.', FCurrentLine, CurrentWordIndex);

  Inc(CurrentWordIndex);
  StatementProperties := ParseStatementPropertiesAndEnsureEndOfLine(
    [WPC_PROBABILITY_STATEMENT_PROPERY_ID, WPC_TIMES_STATEMENT_PROPERY_ID],
    LineWords, CurrentWordIndex);

  UseBranchStatement := TWpcUseBranchStatement.Create(BranchName);
  UseBranchStatement.SetProbability(StatementProperties.Probability);
  UseBranchStatement.SetTimes(StatementProperties.Times);
  Result := UseBranchStatement;
end;

{
  Parses directory path.
  Syntax:
  <Path> | "<Path>"
  Where Path is absolute or relative path to a directory.
  Note, that, Path could contain spaces, but in that case it should be contained into double quotes.
}
function TWpcScriptParser.ParseDirectory(LineWords : TStringList; var Index : Integer) : String;
var
  PathReference : String;
begin
  PathReference := SafeGet(LineWords, Index);

  if (PathReference = '') then
    raise TWpcScriptParseException.Create('Path to a directory is expected.', FCurrentLine, Index);

  // Split keeps quoted words as single one, but with its quotes.
  if (PathReference[1] = QUOTE_SYMBOL) then begin
    if (PathReference[Length(PathReference)] <> QUOTE_SYMBOL) then
      raise TWpcScriptParseException.Create('End of quote is expexted, but end of line found.', FCurrentLine, Index);
    // Remove quotes
    PathReference := Copy(PathReference, 2, Length(PathReference) - 2);
  end;

  Inc(Index);

  if (FCheckScriptResources and (not DirectoryExists(PathReference))) then
    raise TWpcScriptParseException.Create('Directory "' + PathReference + '" does not exist.', FCurrentLine);

  if (PathReference.EndsWith(PATH_SEPARATOR)) then
    Delete(PathReference, Length(PathReference), 1);

  Result := PathReference;
end;

{
  Parses image path for wallpaper.
  Syntax:
  <File> | "<File>"
  Where File could be:
   - absolute or relative path to an image
   - $DirVariable/path/image.ext
   - $ImageVariable
  Note, that:
   - Path could contain spaces, but in that case it should be contained into double quotes.
     Example:
     "$Mywallpapers/some dir/image.jpg"
   - After function execution Index is set to the next word after path (might be out of range).
}
function TWpcScriptParser.ParseImage(LineWords : TStringList; var Index : Integer) : TWpcImage;
var
  ImageReference : String;
begin
  ImageReference := SafeGet(LineWords, Index);

  if (ImageReference = '') then
    raise TWpcScriptParseException.Create('Reference to an image is expected.', FCurrentLine, Index);

  // Split keeps quoted words as single one, but with its quotes.
  if (ImageReference[1] = QUOTE_SYMBOL) then begin
    if (ImageReference[Length(ImageReference)] <> QUOTE_SYMBOL) then
      raise TWpcScriptParseException.Create('End of quote is expexted, but end of line found.', FCurrentLine, Index);
    // Remove quotes
    ImageReference := Copy(ImageReference, 2, Length(ImageReference) - 2);
  end;

  Inc(Index);

  if (ImageReference[1] = VARIABLE_START_SYMBOL) then begin
    if (Pos(PATH_SEPARATOR, ImageReference) = 0) then
      // Image variable
      ImageReference := ApplyParsedImagesVariables(ImageReference)
    else
      // Directory variable and path
      ImageReference := ApplyParsedDirectoriesVariables(ImageReference);
  end
  else
    ImageReference := ToAbsolutePath(ImageReference);

  if (FCheckScriptResources and (not FileExists(ImageReference))) then
    raise TWpcScriptParseException.Create('Image with "' + ImageReference + '" path does not exist.', FCurrentLine);

  Result := TWpcImage.Create(ImageReference);
end;

function TWpcScriptParser.GetDefaultStatementProperties() : TStatementProperties;
var
  StatementProperties : TStatementProperties;
begin
  StatementProperties.Delay := 0;
  StatementProperties.Probability := 100;
  StatementProperties.Times := 1;
  StatementProperties.Style := FDefaultWallpaperStyle;

  Result := StatementProperties;
end;

{
  Parses the given statement properties from the given index.
  Sets index value to the next position after last recognized option.
  Allowed statement properties shouldn't be empty and shouldn't contain unknown statement property.
  Returns parsed values over defaults.
}
function TWpcScriptParser.ParseStatementProperties(AllowedProperties : TStatementPropertiesSet; LineWords : TStringList; var Index : Integer) : TStatementProperties;
var
  StatementProperties : TStatementProperties;
  AssumedNextProperty : TWpcStatementPropertyId;
begin
  StatementProperties := GetDefaultStatementProperties();

  while (Index < LineWords.Count) do begin
    AssumedNextProperty := ProbePropertyType(LineWords, Index);
    if (not (AssumedNextProperty in AllowedProperties)) then
      raise TWpcScriptParseException.Create('Unexpected word "' + LineWords[Index] + '".', FCurrentLine, Index);

    case (AssumedNextProperty) of
      WPC_DELAY_STATEMENT_PROPERY_ID:
        begin
          StatementProperties.Delay := ParseDelayProperty(LineWords, Index);
          Index := Index + 2;
        end;
      WPC_TIMES_STATEMENT_PROPERY_ID:
        begin
          StatementProperties.Times := ParseTimesProperty(LineWords, Index);
          Index := Index + 2;
        end;
      WPC_PROBABILITY_STATEMENT_PROPERY_ID:
        begin
          StatementProperties.Probability := ParseProbabilityProperty(LineWords, Index);
          Index := Index + 3;
        end;
      WPC_WALLPAPER_STYLE_PROPERTY_ID:
        begin
          StatementProperties.Style := ParseWallpaperStyleProperty(LineWords, Index);
          Index := Index + 2;
        end;
      WPC_UNKNOWN_STATEMENT_PROPERTY:
        raise TWpcUseErrorException.Create('Cannot parse unknown property. Unknown properties is not allowed.');
      else
        // Should never happen
        raise TWpcException.Create('Cannot handle property: ' + StatementPropertyIdToStr(AssumedNextProperty));
    end;
  end;

  Result := StatementProperties;
end;

{
  The same as ParseStatementProperties, but also checks for end of line after last property.
}
function TWpcScriptParser.ParseStatementPropertiesAndEnsureEndOfLine(AllowedProperties : TStatementPropertiesSet; LineWords : TStringList; Index : Integer) : TStatementProperties;
var
  StatementProperties : TStatementProperties;
begin
  StatementProperties := ParseStatementProperties(AllowedProperties, LineWords, Index);
  EnsureEndOfLine(LineWords, Index);
  Result := StatementProperties;
end;

{
  Syntax:
  FOR <Time>
}
function TWpcScriptParser.ParseDelayProperty(LineWords : TStringList; WordIndex : Integer) : Integer;
var
  DelayString : String;
begin
  EnsureKeyWord(LineWords, WordIndex, FOR_KEYWORD);

  Inc(WordIndex);
  DelayString := SafeGet(LineWords, WordIndex);
  if (DelayString = '') then
    raise TWpcScriptParseException.Create('Valid delay expected, but nothing found.', FCurrentLine, WordIndex);

  Result := ParseDelayValue(DelayString);
end;

{
  Syntax:
  <n> TIMES
  Where n > 0
}
function TWpcScriptParser.ParseTimesProperty(LineWords : TStringList; WordIndex : Integer) : Integer;
var
  TimesString : String;
begin
  TimesString := SafeGet(LineWords, WordIndex);
  if (TimesString = '') then
    raise TWpcScriptParseException.Create(TIMES_KEYWORD + ' property expected, but nothing found.', FCurrentLine, WordIndex);

  Inc(WordIndex);
  EnsureKeyWord(LineWords, WordIndex, TIMES_KEYWORD);

  Result := ParseTimesValue(TimesString);
end;

{
  Syntax:
  WITH PROBABILITY <0-100>
}
function TWpcScriptParser.ParseProbabilityProperty(LineWords : TStringList; WordIndex : Integer) : Byte;
var
  ProbabilityString : String;
begin
  EnsureKeyWord(LineWords, WordIndex, WITH_KEYWORD);

  Inc(WordIndex);
  EnsureKeyWord(LineWords, WordIndex, PROBABILITY_KEYWORD);

  Inc(WordIndex);
  ProbabilityString := SafeGet(LineWords, WordIndex);
  if (ProbabilityString = '') then
    raise TWpcScriptParseException.Create(PROBABILITY_KEYWORD + ' property expected, but value not found.', FCurrentLine, WordIndex);

  Result := ParseProbabilityValue(ProbabilityString);
end;

{
  Syntax:
  STYLE <Style>
  Where:
   - Style: one of TWpcWallpaperStyle
}
function TWpcScriptParser.ParseWallpaperStyleProperty(LineWords : TStringList; WordIndex : Integer) : TWpcWallpaperStyle;
var
  WallpaperStyleString : String;
begin
  EnsureKeyWord(LineWords, WordIndex, STYLE_KEYWORD);

  Inc(WordIndex);
  WallpaperStyleString := SafeGet(LineWords, WordIndex);
  if (WallpaperStyleString = '') then
    raise TWpcScriptParseException.Create(STYLE_KEYWORD + ' property expected, but value not found.', FCurrentLine, WordIndex);

  Result := ParseWallpaperStyleValue(WallpaperStyleString);
end;

{
  Parses selector of given type at the end of the line.
  Syntax:
  <*+>+ <Selector type> <Selector value>
  Note, that WEIGHT selector with default value (1) could be ommited.
}
function TWpcScriptParser.ParseAndRemoveSelector(LineWords : TStringList; Selector : TWpcSelector) : TWpcSelectorValueHolder;
  function SelectorToStr(Selector : TWpcSelector) : String;
  begin
    case (Selector) of
      S_WEIGHT:   Result := WEIGHT_KEYWORD;
      S_SEASON:   Result := SEASON_KEYWORD;
      S_WEEKDAY:  Result := WEEKDAY_KEYWORD;
      S_MONTH:    Result := MONTH_KEYWORD;
      S_DATE:     Result := DATE_KEYWORD;
      S_TIME:     Result := TIME_KEYWORD;
      S_DATETIME: Result := DATETIME_KEYWORD;
    end;
  end;

var
  Len           : Integer;
  SelectorValue : TWpcSelectorValueHolder;
begin
  Len := LineWords.Count;

  // Minimal option should consist from at least 3 words: Item value and Selector type and Selector value.
  // Exception is only default WEIGHT selector which could be ommited.
  if (Len < 3) then begin
    if (Selector = S_WEIGHT) then begin
      SelectorValue.Weight := 1; // Supposed default WEIGHT keyword and its value ommited
      Result := SelectorValue;
      exit;
    end;
    raise TWpcScriptParseException.Create('Chooser item value and valid selector expected.', FCurrentLine, Len-1);
  end;

  // Handle selector keyword without a value
  if (SafeGet(LineWords, Len-1) = SelectorToStr(Selector)) then
    raise TWpcScriptParseException.Create('Selector value expected.', FCurrentLine, Len-1);

  // Handle ommited WEIGHT selector and its default value.
  if ((Selector = S_WEIGHT) and (SafeGet(LineWords, Len-1 - 1) <> WEIGHT_KEYWORD)) then begin
     SelectorValue.Weight := 1; // Supposed default WEIGHT keyword and its value omitted
     Result := SelectorValue;
     exit;
  end;

  case (Selector) of
    S_WEIGHT:
      begin
        EnsureSelectorKeyword(LineWords, WEIGHT_KEYWORD);
        SelectorValue.Weight := ParseWeightValue(SafeGet(LineWords, Len - 1));
      end;
    S_SEASON:
      begin
        EnsureSelectorKeyword(LineWords, SEASON_KEYWORD);
        SelectorValue.Sequential := ParseSeasonValue(SafeGet(LineWords, Len - 1));
      end;
    S_WEEKDAY:
      begin
        EnsureSelectorKeyword(LineWords, WEEKDAY_KEYWORD);
        SelectorValue.Sequential := ParseWeekdayValue(SafeGet(LineWords, Len - 1));
      end;
    S_MONTH:
      begin
        EnsureSelectorKeyword(LineWords, MONTH_KEYWORD);
        SelectorValue.Sequential := ParseMonthValue(SafeGet(LineWords, Len - 1));
      end;
    S_DATE:
      begin
        EnsureSelectorKeyword(LineWords, DATE_KEYWORD);
        SelectorValue.DateTime := ParseDateValue(SafeGet(LineWords, Len - 1));
      end;
    S_TIME:
      begin
        EnsureSelectorKeyword(LineWords, TIME_KEYWORD);
        SelectorValue.DateTime := ParseTimeValue(SafeGet(LineWords, Len - 1));
      end;
    S_DATETIME:
      begin
        EnsureSelectorKeyword(LineWords, DATETIME_KEYWORD);
        SelectorValue.DateTime := ParseDateTimeValue(SafeGet(LineWords, Len - 1));
      end;
  end;

  // Delete 2 last words: selector and its value. This is safe because of Len >= 3 check.
  LineWords.Delete(Len - 1);
  LineWords.Delete(Len - 2);

  Result := SelectorValue;
end;

{
  Parses sequential selector value by number or alias.
  Example:
  ParseSequentialNumberWithAlias('B', ('A', 'B', 'C'))
  will return 2
  Note, non-empty upper-case array is mandatory.
}
function TWpcScriptParser.ParseSequentialNumberWithAlias(Value : String; Aliases : Array of String) : Integer;
var
  AliasesNumber         : Integer;
  ParsedSequentialValue : Integer;
  ApperCaseAliasValue   : String;
  i                     : Integer;
begin
  AliasesNumber := Length(Aliases);
  if (TryStrToInt(Value, ParsedSequentialValue)) then
    // Argument is just a number. Check range.
    if (ParsedSequentialValue in [1..AliasesNumber]) then begin
      Result := ParsedSequentialValue;
      exit;
    end
    else
      raise TWpcScriptParseException.Create('Value "' + Value + '" is out of range.', FCurrentLine);

  // Value is an alias.
  ApperCaseAliasValue := UpperCase(Value);
  for i:=0 to (AliasesNumber - 1) do
    if (ApperCaseAliasValue = Aliases[i]) then begin
      Result := i + 1; // Count from 1
      exit;
    end;

  raise TWpcScriptParseException.Create('Unknown value "' + Value + '".', FCurrentLine);
end;

{
  Syntax:
  <n[ms|s|m|h|d]>
  Where total dalay shouldn't exceed 32 days.
  Returns parsed delay in milliseconds.
  Note, no spaces alowed.
}
function TWpcScriptParser.ParseDelayValue(Delay : String) : LongWord;
var
  ParsedDelayValueMilliseconds : LongWord;
  ParsedDelayValue             : LongWord;
  ParsedDelayUnits             : TWpcTimeMeasurementUnits;
  DelayNumberString            : String;
  MeasurementUnitsString       : String;
  i                            : Integer;
  Len                          : Integer;
begin
  Len := Length(Delay);
  i := 1;
  while ((i <= Len) and (Delay[i] in ['0'..'9'])) do
    Inc(i);
  DelayNumberString := copy(Delay, 1, i-1);
  if (Length(DelayNumberString) < 1) then
    raise TWpcScriptParseException.Create('Failed to parse delay: "' + Delay + '". A positive number is expected', FCurrentLine);
  if (not TryStrToDWord(DelayNumberString, ParsedDelayValue)) then
    raise TWpcScriptParseException.Create('Failed to parse delay value: "' + DelayNumberString + '".', FCurrentLine);

  if (i <= Len) then begin
    MeasurementUnitsString := copy(Delay, i, Len - i + 1);
    ParsedDelayUnits := ParseDelayMeasurmentUnitsValue(MeasurementUnitsString);
  end
  else
    ParsedDelayUnits := GetDefaultDelayUnits();

  ParsedDelayValueMilliseconds := ConvertToMilliseconds(ParsedDelayValue, ParsedDelayUnits);
  if (ParsedDelayValueMilliseconds > TWpcDelayStatementProperty.MAX_DELAY_VALUE) then
    raise TWpcScriptParseException.Create('Too big delay: ' + Delay, FCurrentLine);

  Result := ParsedDelayValueMilliseconds;
end;

{
  Syntax:
  <ms|s|m|h|d>
}
function TWpcScriptParser.ParseDelayMeasurmentUnitsValue(MeasurementUnits : String) : TWpcTimeMeasurementUnits;
begin
  try
    Result := StrToTimeMeasurementUnit(MeasurementUnits);
  except
    on E : TWpcException do
      raise TWpcScriptParseException.Create('Failed to parse delay measurment units: "' + MeasurementUnits + '".', FCurrentLine);
  end;
end;

{
  Syntax:
  <n>
  Where n in 0-100
}
function TWpcScriptParser.ParseProbabilityValue(Probability : String) : Byte;
var
  ParsedProbabilityValue : Integer;
begin
  if (not TryStrToInt(Probability, ParsedProbabilityValue)) then
    raise TWpcScriptParseException.Create('Failed to parse probability value: "' + Probability + '".', FCurrentLine);

  if ((ParsedProbabilityValue < 0) or (ParsedProbabilityValue > 100)) then
    raise TWpcScriptParseException.Create('Probability value should be in [0-100]', FCurrentLine);

  Result := ParsedProbabilityValue;
end;

{
  Syntax:
  <n>
  Where n >= 0
}
function TWpcScriptParser.ParseTimesValue(Times : String) : LongWord;
var
  ParsedTimesValue : Integer;
begin
  if (not TryStrToInt(Times, ParsedTimesValue)) then
    raise TWpcScriptParseException.Create('Failed to parse times value: "' + Times + '".', FCurrentLine);

  if (ParsedTimesValue < 1) then
    raise TWpcScriptParseException.Create('Times value should be positive', FCurrentLine);

  Result := ParsedTimesValue;
end;

{
  Syntax:
  <Style>
  Where Style is string from TWpcWallpaperStyle.
}
function TWpcScriptParser.ParseWallpaperStyleValue(Style : String) : TWpcWallpaperStyle;
var
  WallpaperStyle : TWpcWallpaperStyle;
begin
  WallpaperStyle := StrToWallpaperStyle(Style);
  if (WallpaperStyle = UNKNOWN) then
    raise TWpcScriptParseException.Create('Unknown wallpaper style "' + Style + '".', FCurrentLine);

  Result := WallpaperStyle;
end;

{
  Syntax:
  <Selector>
  Where selector is TWpcSelector.
}
function TWpcScriptParser.ParseSelectorValue(Selector : String) : TWpcSelector;
begin
  case (Selector) of
   WEIGHT_KEYWORD:   Result := S_WEIGHT;
   SEASON_KEYWORD:   Result := S_SEASON;
   WEEKDAY_KEYWORD:  Result := S_WEEKDAY;
   MONTH_KEYWORD:    Result := S_MONTH;
   DATE_KEYWORD:     Result := S_DATE;
   TIME_KEYWORD:     Result := S_TIME;
   DATETIME_KEYWORD: Result := S_DATETIME;
   else
     raise TWpcScriptParseException.Create('Unknown selector type "' + Selector + '".', FCurrentLine);
  end;
end;

{
  Syntax:
  <n>
  Where n > 0
}
function TWpcScriptParser.ParseWeightValue(Weight : String) : LongWord;
var
  ParsedWeightValue : LongWord;
begin
  if (not TryStrToDWord(Weight, ParsedWeightValue)) then
    raise TWpcScriptParseException.Create('Failed to parse weight value: "' + Weight + '".', FCurrentLine);
  if (ParsedWeightValue < 1) then
    raise TWpcScriptParseException.Create('Weight selector value should be positive, bot got "' + Weight + '".', FCurrentLine);
  Result := ParsedWeightValue;
end;

{
  Syntax:
  <1-4>|<Season name>
  Where Winter is season with index 1
}
function TWpcScriptParser.ParseSeasonValue(Season : String) : Integer;
begin
  Result := ParseSequentialNumberWithAlias(Season, SEASONS);
end;

{
  Syntax:
  <1-7>|<Day name>
  Where Sunday is day with index 1
}
function TWpcScriptParser.ParseWeekdayValue(Weekday : String) : Integer;
begin
  Result := ParseSequentialNumberWithAlias(Weekday, DAYS_OF_WEEK);
end;

{
  Syntax:
  <1-12>|<Month name>
  Where January is month with index 1
}
function TWpcScriptParser.ParseMonthValue(Month : String) : Integer;
begin
  Result := ParseSequentialNumberWithAlias(Month, MONTHS);
end;

{
  Syntax:
  <dd.mm>
}
function TWpcScriptParser.ParseDateValue(Date :  String) : TDateTime;
var
  ParsedDateValue : TDateTime;
begin
  if (Date.StartsWith(WPC_DATE_SEPARATOR) or Date.EndsWith(WPC_DATE_SEPARATOR)) then
    raise TWpcScriptParseException.Create('Cannot parse date selector value: "' + Date + '". Both day and month should be specified.', FCurrentLine);
  if (Date.CountChar(WPC_DATE_SEPARATOR) <> 1) then
    raise TWpcScriptParseException.Create('Cannot parse date selector value: "' + Date + '". Date value should have only one separator', FCurrentLine);

  if (not TryStrToDate(Date, ParsedDateValue)) then
    raise TWpcScriptParseException.Create('Cannot parse date selector value: "' + Date + '". Invalid format.', FCurrentLine);

  Result := ParsedDateValue;
end;

{
  Syntax:
  <hh:mm[:ss]>
}
function TWpcScriptParser.ParseTimeValue(Time : String) : TDateTime;
var
  ParsedTimeValue : TDateTime;
begin
  if (not TryStrToTime(Time, ParsedTimeValue)) then
    raise TWpcScriptParseException.Create('Cannot parse time selector value: "' + Time + '". Invalid format.', FCurrentLine);

  Result := ParsedTimeValue;
end;

{
  Syntax:
  <dd.mm>-<hh:mm[:ss]>
}
function TWpcScriptParser.ParseDateTimeValue(DateTime : String) : TDateTime;
var
  CanonicalDateTime   : String;
  ParsedDateTimeValue : TDateTime;
  DashIndex           : Integer;
begin
  // This checks is needed to prevent parsion of logically incorrect strings.
  // For example '10. 10:12:53' will be parsed as 10.10.** 10:12:53
  if (DateTime.CountChar(WPC_DATE_TIME_SEPARATOR) <> 1) then
    raise TWpcScriptParseException.Create('Cannot parse date-time selector value: "' + DateTime + '". Date-time value should have separator', FCurrentLine);
  DashIndex := Pos(WPC_DATE_TIME_SEPARATOR, DateTime);
  if ((DashIndex = 1) or (DashIndex = Length(DateTime))) then
    raise TWpcScriptParseException.Create('Invalid date-time selector value: "' + DateTime + '"', FCurrentLine);
  if ((DateTime[DashIndex-1] = WPC_DATE_SEPARATOR) or (DateTime[DashIndex+1] = WPC_DATE_SEPARATOR) or
      (DateTime[DashIndex-1] = WPC_TIME_SEPARATOR) or (DateTime[DashIndex+1] = WPC_TIME_SEPARATOR)) then
    raise TWpcScriptParseException.Create('Invalid date-time selector value: "' + DateTime + '"', FCurrentLine);

  CanonicalDateTime := StringReplace(DateTime, WPC_DATE_TIME_SEPARATOR, ' ', []); // To be able to use StrToDateTime function
  if (not TryStrToDateTime(CanonicalDateTime, ParsedDateTimeValue)) then
    raise TWpcScriptParseException.Create('Cannot parse date and time selector value: "' + DateTime + '". Invalid format.', FCurrentLine);

  Result := ParsedDateTimeValue;
end;

{
  Returns true if name is valid, false otherwise.
}
function TWpcScriptParser.ValidateName(Name : String) : Boolean;
var
  c : Char;
begin
  for c in Name do
    if (not (c in ALLOWED_NAME_SYMBOLS)) then begin
      Result := False;
      exit;
    end;
  Result := True;
end;

{
  Checks that all Switch Branch and Use Branch statements use existing branches.
}
procedure TWpcScriptParser.ValidateBranchNames();
  procedure EnsureBranchExists(BranchName : String; BranchesNames : TStringList);
  var
    Line  : Integer;
    Index : Integer;
  begin
    if (BranchesNames.IndexOf(BranchName) = -1) then begin
      Line := 0;
      Index := 0;
      SearchWordInScript(BranchName, True, Line, Index);
      raise TWpcScriptParseException.Create('Specified branch "' + BranchName + '" doesn''t exist.', Line, Index);
    end;
  end;

var
  BranchesNames : TStringList;
  Statement     : IWpcBaseScriptStatement;
  ChooserItem   : TWpcChooserItem;
  i             : Integer;
begin
  BranchesNames := FScript.GetBranchNames();
  try
    for i:=0 to (BranchesNames.Count - 1) do
      for Statement in FScript.GetBranch(BranchesNames[i]).GetBranchStatements() do begin
        if (Statement.InheritsFrom(IWpcBranchReferrer.ClassType)) then begin
          EnsureBranchExists(IWpcBranchReferrer(Statement).GetBranchName(), BranchesNames);
        end
        else if ((Statement.ClassType = TWpcSwitchBranchChooserStatement.ClassType) or
                 (Statement.ClassType = TWpcUseBranchChooserStatement.ClassType)) then begin
          for ChooserItem in IWpcChooserItems(Statement).GetItems() do
            EnsureBranchExists(IWpcBranchReferrer(ChooserItem.Statement).GetBranchName(), BranchesNames);
        end;
      end;
  finally
    BranchesNames.Free();
  end;
end;

{
  Returns specified line of the script.
  In case of commentary or empty line empty string will be returned.
  In case if given line number is out of range END_OF_SCRIPT will be returned.
}
function TWpcScriptParser.GetLine(LineNumber : Integer) : String;
var
  Line : String;
begin
  if (FCurrentLine >= FLines.Count) then begin
    Result := END_OF_SCRIPT;
    exit;
  end;
  Line := TrimSet(FLines[LineNumber], WHITESPACE_SET);

  if ((Line <> '') and (Line[1] = COMMENTARY_SYMBOL)) then
    Line := '';

  Result := Line;
end;

{
  Returns next significant (i.e. non-empty and not comment) line of the script
  or END_OF_SCRIPT if end of the script is reached.
}
function TWpcScriptParser.GetNextLine() : String;
var
  Line : String;
begin
  repeat
    Inc(FCurrentLine);
    Line := GetLine(FCurrentLine);
  until (Line <> '');
  Result := Line;
end;

{
  Returns list of words in the given line.
  If incoming line is nil or empty TWpcUseErrorException will be rised.
  The list contains at least one word.
  A word from the list cannot be empty or nil.
  The list should be freed by invoker.
}
function TWpcScriptParser.SplitLine(Line : String) : TStringList;
var
  LineWords : TStringList;
begin
  if ((Line = '') or (@Line = nil)) then
    raise TWpcUseErrorException.Create('Cannot split empty line. Line number: ' + IntToStr(FCurrentLine));
  if (Line = END_OF_SCRIPT) then
    raise TWpcScriptParseException.Create('Unxpected end of script.', FCurrentLine);

  LineWords := TStringList.Create();
  ExtractStrings(WHITESPACE_SET, WHITESPACE_SET, PChar(Line), LineWords);
  Result := LineWords;
end;

{
  Probes statement from given line words.
  Returns:
   - Statement ID if statement detected
   - WPC_END_OF_BLOCK_STATEMENT if the line starts with END key word
   - WPC_UNKNOWN_STATEMENT if unknown
}
function TWpcScriptParser.ProbeStatementType(LineWords : TStringList) : TWpcStatemetId;
begin
  case (UpperCase(SafeGet(LineWords, 0))) of
    SET_KEYWORD:
      case (UpperCase(SafeGet(LineWords, 1))) of
        WALLPAPER_KEYWORD:
          case (UpperCase(SafeGet(LineWords, 2))) of
            FROM_KEYWORD:
              Result := WPC_DIRECTORY_STATEMENT_ID
            else
              Result := WPC_WALLPAPER_STATEMENT_ID
          end
        else
          Result := WPC_UNKNOWN_STATEMENT;
      end;
    WAIT_KEYWORD:
      Result := WPC_WAIT_STATEMENT_ID;
    SWITCH_KEYWORD:
      Result := WPC_SWITCH_BRANCH_STATEMENT_ID;
    USE_KEYWORD:
      Result := WPC_USE_BRANCH_STATEMENT_ID;
    STOP_KEYWORD:
      Result := WPC_STOP_STATEMENT_ID;
    CHOOSE_KEYWORD:
      case (UpperCase(SafeGet(LineWords, 1))) of
        WALLPAPER_KEYWORD:
          Result := WPC_WALLPAPER_CHOOSER_STATEMENT_ID;
        BRANCH_KEYWORD:
          case (UpperCase(SafeGet(LineWords, 3))) of
            SWITCH_KEYWORD:
              Result := WPC_BRANCH_TO_SWITCH_CHOOSER_STATEMENT_ID;
            USE_KEYWORD:
              Result := WPC_BRANCH_TO_USE_CHOOSER_STATEMENT_ID;
            else
              Result := WPC_UNKNOWN_STATEMENT;
          end
        else
          Result := WPC_UNKNOWN_STATEMENT;
      end;
    END_KEYWORD:
      Result := WPC_END_OF_BLOCK_STATEMENT;
    else
      Result := WPC_UNKNOWN_STATEMENT;
  end;
end;

{
  Probes statement property type at given index.
  Returns:
   - Property ID if property recognized
   - WPC_UNKNOWN_STATEMENT_PROPERTY if unknown
}
function TWpcScriptParser.ProbePropertyType(LineWords : TStringList; WordIndex : Integer) : TWpcStatementPropertyId;
begin
  case (UpperCase(SafeGet(LineWords, WordIndex))) of
    FOR_KEYWORD:
      Result := WPC_DELAY_STATEMENT_PROPERY_ID;
    WITH_KEYWORD:
      Result := WPC_PROBABILITY_STATEMENT_PROPERY_ID;
    STYLE_KEYWORD:
      Result := WPC_WALLPAPER_STYLE_PROPERTY_ID;
    else begin
      case (UpperCase(SafeGet(LineWords, WordIndex + 1))) of
        TIMES_KEYWORD:
          Result := WPC_TIMES_STATEMENT_PROPERY_ID;
        else
          Result := WPC_UNKNOWN_STATEMENT_PROPERTY;
      end;
    end;
  end;
end;

function TWpcScriptParser.IsWordANumber(AWord : String) : Boolean;
var
  i : Integer;
begin
  for i:=1 to Length(AWord) do
    if (not (AWord[i] in ['0'..'9'])) then begin
      Result := False;
      exit;
    end;
  Result := True;
end;

{
  Helper method to check mandatory keywords.
  The first parameter is a value to check.
  The second parameter have to be in upper case.
}
function TWpcScriptParser.CheckKeyWord(Given : String; Expected : String) : Boolean;
begin
  Result := UpperCase(Given) = Expected;
end;

{
  Checks that line has specified keyword at given index.
  Note, resources should be freed by invoker.
}
procedure TWpcScriptParser.EnsureKeyWord(LineWords : TStringList; Index : Integer; ExpectedKeyWord : String);
var
  AWord : String;
begin
 AWord := SafeGet(LineWords, Index);
 if (AWord = '') then
   raise TWpcScriptParseException.Create(ExpectedKeyWord + '" keyword expected, but nothing found', FCurrentLine, Index);
 if (UpperCase(AWord) <> ExpectedKeyWord) then
   raise TWpcScriptParseException.Create(ExpectedKeyWord + '" keyword expected, but got "' + AWord + '".', FCurrentLine, Index);
end;

{
  Checks that line consists only from given keywords.
  Note, resources should be freed by invoker.
}
procedure TWpcScriptParser.EnsureKeyWordsLine(GivenKeyWords: TStringList;
  ExpectedKeyWords: array of String);
var
  i : Integer;
begin
  for i:=0 to (Length(ExpectedKeyWords) - 1) do begin
    EnsureKeyWord(GivenKeyWords, i, ExpectedKeyWords[i]);
  end;
  EnsureEndOfLine(GivenKeyWords, i+1);
end;

{
  Checks that line consists only from given keywords.
  Note, this method free resources despite check result.
}
procedure TWpcScriptParser.EnsureKeyWordsLineAndFreeResources(
  GivenKeyWords: TStringList; ExpectedKeyWords: array of String);
begin
  try
    EnsureKeyWordsLine(GivenKeyWords, ExpectedKeyWords);
  finally
    GivenKeyWords.Free();
  end;
end;

{
  Checks second from the line end keyword.
}
procedure TWpcScriptParser.EnsureSelectorKeyword(LineWords : TStringList; SelectorKeyword : String);
var
  SecondWordFromLineEndIndex : Integer;
  i                          : Integer;
begin
  SecondWordFromLineEndIndex := LineWords.Count-1 - 1; // -1 because indexes are counted from 0
  if (not CheckKeyWord(SafeGet(LineWords, SecondWordFromLineEndIndex), SelectorKeyword)) then begin
    // syntax error
    for i:=SecondWordFromLineEndIndex downto 1 do
      if (CheckKeyWord(SafeGet(LineWords, i), SelectorKeyword)) then
        raise TWpcScriptParseException.Create('Selector keyword "' + SelectorKeyword + '" should be second from the end of an option line.', FCurrentLine, i);
    // no selector keyword found
    raise TWpcScriptParseException.Create(SelectorKeyword + ' selector expected, but "' + SafeGet(LineWords, SecondWordFromLineEndIndex + 1) + '" found.', FCurrentLine, SecondWordFromLineEndIndex + 1); // +1 to point on the last word in line
  end;
end;

{
  Checks that index points after the last word of the given line, i.e. no more words in the line.
  Note, resources should be freed by invoker.
}
procedure TWpcScriptParser.EnsureEndOfLine(LineWords : TStringList; WordIndex : Integer);
begin
  if (SafeGet(LineWords, WordIndex) <> '') then
    raise TWpcScriptParseException.Create('Unexpected word "' + SafeGet(LineWords, WordIndex) + '".', FCurrentLine, WordIndex);
end;

{
  Returns item with specified index from the given list
  or empty string if the list doesn't have a word under given index.
}
function TWpcScriptParser.SafeGet(LineWords : TStringList; Index : Integer) : String;
begin
  if ((Index < LineWords.Count) and (Index >= 0)) then
    Result := LineWords[Index]
  else
    Result := '';
end;

{
  Searches for specified word in whole script.
  Line and Index parameters sets start search point.
  Returns first occurrence or (-1,-1) if the given word not found.
}
procedure TWpcScriptParser.SearchWordInScript(TheWord : String; CaseSensitive : Boolean; var Line : Integer; var Index : Integer);
var
  LineString : String;
  LineWords  : TStringList;
  AWord      : String;
begin
  if (not CaseSensitive) then
    TheWord := UpperCase(TheWord);

  while (True) do begin
    LineString := GetLine(Line);
    if (LineString = END_OF_SCRIPT) then
      break;
    if (LineString <> '') then begin
      LineWords := SplitLine(LineString);
      try
        while (Index < (LineWords.Count - 1)) do begin
          if (CaseSensitive) then
            AWord := LineWords[Index]
          else
            AWord := UpperCase(LineWords[Index]);

          if (AWord = TheWord) then
            // Specified word found, current values of Line and Index points to it.
            exit;

          Inc(index);
        end;
      finally
        LineWords.Free();
      end;
    end;
    // Prepare for the next iteration
    Inc(Line);
    Index := 0;
  end;
  // End of script is reached, given word not found.
  Line := -1;
  Index := -1;
end;

{
  Sets base path for paths in script.
  If given path is empty path to executable will be used.
}
procedure TWpcScriptParser.SetBasePath(Path : String);
begin
  Path := GetAbsolutePath(Path);

  if (Path.EndsWith(PATH_SEPARATOR)) then
    FBasePath := Path
  else
    FBasePath := Path + PATH_SEPARATOR;
end;

{
  Converts given relative path to absolute relatively to script location.
  If script location is not specified then path to executable will be used as base path.
  If given path is already absolute it will be returned without changes.
}
function TWpcScriptParser.ToAbsolutePath(Path : String) : String;
begin
  if (IsAbsolutePath(Path)) then begin
    Result := Path;
    exit;
  end;

  if (FBasePath = '') then
    Result := GetAbsolutePath(Path)
  else
    Result := FBasePath + Path;
end;


initialization
  // Set default date and time format
  DefaultFormatSettings.DateSeparator := WPC_DATE_SEPARATOR;
  DefaultFormatSettings.TimeSeparator := WPC_TIME_SEPARATOR;


end.

