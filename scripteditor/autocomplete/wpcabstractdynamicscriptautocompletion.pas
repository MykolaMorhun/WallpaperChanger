unit WpcAbstractDynamicScriptAutocompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  WpcAbstractScriptAutocompletion,
  WpcScriptParser,
  WpcWallpaperStyles,
  WpcTimeMeasurementUnits,
  WpcExceptions;

type

  { TWpcAbstractDynamicScriptAutocompletion }

  TWpcAbstractDynamicScriptAutocompletion = class abstract(TWpcAbstractScriptAutocompletion)
  public
    const
      FILE_PATH_INSERTION = '<file>';
      DIR_PATH_INSERTION = '<dir>';
  protected
    type
      // Zero based caret position
      TWpcScriptAutocompleteCursorPosition = record
        Row : LongInt;
        Col : Longint;
      end;
  protected
    FScriptLines      : TStrings;
    FCursorPosition   : TWpcScriptAutocompleteCursorPosition;
    FCurrentLine      : String;
    FCurrentWord      : String;
    FCurrentWordIndex : Integer;
    FCurrentLineWords : TStrings;
  public
    function CreateAutocompleteList(FullScript : TStrings;
                                    CursorPosition : TPoint;
                                    CurrentWord : String) : TStrings; override;
  protected
    procedure CacheOption(Option : String);

    procedure AddToCacheAllSelectors(); inline;
    procedure AddToCacheAllWallpaperStyles(); inline;
    procedure AddToCacheAllMeasurementUnits(); inline;
    procedure AddToCacheSeasons(); inline;
    procedure AddToCacheMonths(); inline;
    procedure AddToCacheWeekdays(); inline;

    function GetLine(LineNumber : Integer) : String;
    function GetCurrentLine() : String; inline;
    function GetLineWords(Line : String) : TStrings;
    function SafeGet(LineWords : TStrings; Index : Integer) : String;
  protected
    // Provides possible completion options for given position.
    // Use CacheOption to add an option.
    procedure DeterminePossibleOptions(); virtual; abstract;
  private
    procedure InsertEmptyWordAtCursor(); inline;
    procedure FindCurrentWordIndex(); inline;
  end;



implementation


{ TWpcAbstractDynamicScriptAutocompletion }

{
  This is scaffold method for generation autocompletion list.
  It filters comments as lines with no completion.
  It inits the following fields:
    - FScriptLines: full script
    - FCursorPosition: current position of caret in editor
    - FCurrentWord: word under completion or empty string if cursor is between whitespaces
    - FCurrentLine: whitespace truncated line in which cursor is placed
    - FCurrentLineWords: list of words in the current line.
                         If current word is empty (cursor between whitespaces) empty string in its place will be added.
    - FCurrentWordIndex: Index of word under completion in the current line.
}
function TWpcAbstractDynamicScriptAutocompletion.CreateAutocompleteList(FullScript : TStrings;
                                                                        CursorPosition : TPoint;
                                                                        CurrentWord : String) : TStrings;
begin
  FCachedOptions.Clear();

  FScriptLines := FullScript;
  FCursorPosition.Row := CursorPosition.Y - 1;
  FCursorPosition.Col := CursorPosition.X - 1;
  FCurrentWord := CurrentWord;

  FCurrentLine := GetCurrentLine();
  if (FCurrentLine.StartsWith(COMMENTARY_SYMBOL)) then begin
    // No completion in comments.
    Result := TStringList.Create();
    exit;
  end;

  FCurrentLineWords := GetLineWords(FCurrentLine);
  try
    if (FCurrentWord = '') then
      InsertEmptyWordAtCursor();

    FindCurrentWordIndex();
    if (FCurrentWordIndex = -1) then
      raise TWpcUseErrorException.Create('Given word should be in the line under completion.');

    try
      DeterminePossibleOptions();
    except
      // Shouldn't happen. But if autocompletion failed, skip it, do not break program.
    end;
  finally
    FCurrentLineWords.Free();
  end;

  Result := UpdateAutocompleteList(FCurrentWord);
end;

{
  Adds given autocomplete option to options cache.
  Does nothing if such option already exists.
}
procedure TWpcAbstractDynamicScriptAutocompletion.CacheOption(Option : String);
begin
  if (FCachedOptions.IndexOf(Option) = -1) then
    FCachedOptions.Add(Option);
end;

procedure TWpcAbstractDynamicScriptAutocompletion.AddToCacheAllSelectors();
begin
  CacheOption(WEIGHT_KEYWORD);
  CacheOption(SEASON_KEYWORD);
  CacheOption(MONTH_KEYWORD);
  CacheOption(WEEKDAY_KEYWORD);
  CacheOption(DATE_KEYWORD);
  CacheOption(TIME_KEYWORD);
  CacheOption(DATETIME_KEYWORD);
end;

procedure TWpcAbstractDynamicScriptAutocompletion.AddToCacheAllWallpaperStyles();
var
  WallpaperStyle : TWpcWallpaperStyle;
begin
  for WallpaperStyle in TWpcWallpaperStyle do
    CacheOption(WallpaperStyleToStr(WallpaperStyle));
end;

procedure TWpcAbstractDynamicScriptAutocompletion.AddToCacheAllMeasurementUnits();
var
  MeasurementUnit : TWpcTimeMeasurementUnits;
begin
  for MeasurementUnit in TWpcTimeMeasurementUnits do
    CacheOption(TimeMeasurementUnitToStr(MeasurementUnit));
end;

procedure TWpcAbstractDynamicScriptAutocompletion.AddToCacheSeasons();
var
  Season : String;
begin
  for Season in SEASONS do
    CacheOption(Season);
end;

procedure TWpcAbstractDynamicScriptAutocompletion.AddToCacheMonths();
var
  Month : String;
begin
  for Month in MONTHS do
    CacheOption(Month);
end;

procedure TWpcAbstractDynamicScriptAutocompletion.AddToCacheWeekdays();
var
  Weekday : String;
begin
  for Weekday in DAYS_OF_WEEK do
    CacheOption(Weekday);
end;

{
  Returns trimmed line of script with given index.
  If number of line is greathed than last script line then empty string will be returned.
}
function TWpcAbstractDynamicScriptAutocompletion.GetLine(LineNumber: Integer): String;
begin
  if (LineNumber <= FScriptLines.Count) then
    Result := TrimSet(FScriptLines[LineNumber], WHITESPACE_SET)
  else
    Result := '';
end;

{
  Returns line in which cursor is set.
}
function TWpcAbstractDynamicScriptAutocompletion.GetCurrentLine(): String;
begin
  Result := GetLine(FCursorPosition.Row);
end;

{
  Returns list of words in the given string.
  Invoked must release the returned list.
}
function TWpcAbstractDynamicScriptAutocompletion.GetLineWords(Line : String) : TStrings;
var
  LineWords : TStrings;
begin
  LineWords := TStringList.Create();

  if (Line <> '') then
    ExtractStrings(WHITESPACE_SET, WHITESPACE_SET, PChar(Line), LineWords);

  Result := LineWords;
end;

function TWpcAbstractDynamicScriptAutocompletion.SafeGet(LineWords : TStrings; Index : Integer) : String;
begin
   if ((Index < LineWords.Count) and (Index >= 0)) then
    Result := LineWords[Index]
  else
    Result := '';
end;

procedure TWpcAbstractDynamicScriptAutocompletion.InsertEmptyWordAtCursor();
var
  i          : Integer;
  Index      : Integer;
  StartIndex : Integer;
  Word       : String;
begin
  if (FCurrentLine = '') then begin
    FCurrentLineWords.Add('');
    exit;
  end;

  if (FCursorPosition.Col > Length(FScriptLines[FCursorPosition.Row])) then begin
    FCurrentLineWords.Add('');
    FCurrentWordIndex := FCurrentLineWords.Count - 1;
    exit;
  end;

  StartIndex := 0;
  for i:=0 to (FCurrentLineWords.Count - 1) do begin
    Word := FCurrentLineWords[i];
    Index := FScriptLines[FCursorPosition.Row].IndexOf(Word, StartIndex);
    StartIndex := Index + Length(Word);
    if (Index >= FCursorPosition.Col) then begin
      FCurrentLineWords.Insert(i, '');
      break;
    end;
  end;
end;

procedure TWpcAbstractDynamicScriptAutocompletion.FindCurrentWordIndex();
var
  Line      : String;
  i         : Integer;
  Len       : Integer;
  WordIndex : Integer;
begin
  // If cursor is between whitespaces then an empty word is present in the current line words list.
  if (FCurrentWord = '') then begin
    FCurrentWordIndex := FCurrentLineWords.IndexOf('');
    exit;
  end;

  Line := FScriptLines[FCursorPosition.Row]; // leave whitespaces
  Len := Length(Line);
  WordIndex := 0;
  i := 0;
  repeat
    while ((i < Len) and (Line[i+1] in WHITESPACE_SET)) do
      Inc(i);
    while ((i < FCursorPosition.Col) and (i < Len)) do
      if (Line[i+1] in WHITESPACE_SET) then begin
        Inc(WordIndex);
        break;
      end
      else
        Inc(i);
  until not ((i < FCursorPosition.Col) and (i < Len));
  FCurrentWordIndex := WordIndex;
end;


end.

