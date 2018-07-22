unit WpcScriptSyntaxHighlighterConfigurrer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  SynHighlighterAny,
  WpcScriptParser,
  WpcTimeMeasurementUnits,
  WpcWallpaperStyles;

const
  KEYWORDS : Array[1..26] of String = (
    DIRECTORIES_KEYWORD,
    DIRECTORY_KEYWORD,
    IMAGES_KEYWORD,
    DELAYS_KEYWORD,
    DELAY_KEYWORD,
    DEFAULTS_KEYWORD,
    UNITS_KEYWORD,
    BRANCH_KEYWORD,
    END_KEYWORD,
    WAIT_KEYWORD,
    STOP_KEYWORD,
    CHOOSE_KEYWORD,
    BY_KEYWORD,
    FROM_KEYWORD,
    SWITCH_KEYWORD,
    USE_KEYWORD,
    SET_KEYWORD,
    WALLPAPER_KEYWORD,
    STYLE_KEYWORD,
    WITH_KEYWORD,
    PROBABILITY_KEYWORD,
    FOR_KEYWORD,
    TIMES_KEYWORD,
    TO_KEYWORD,
    ORDERED_KEYWORD,
    RECURSIVE_KEYWORD
  );

  SELECTORS : Array[1..7] of String = (
    WEIGHT_KEYWORD,
    SEASON_KEYWORD,
    WEEKDAY_KEYWORD,
    MONTH_KEYWORD,
    DATE_KEYWORD,
    TIME_KEYWORD,
    DATETIME_KEYWORD
  );


function CreateWallpaperChangerScriptHighlighter(Owner : TComponent) : TSynAnySyn;


implementation


procedure ConfigureKeyWords(Highlighter : TSynAnySyn); forward;
procedure ConfigureConstants(Highlighter : TSynAnySyn); forward;
procedure ConfigureObjects(Highlighter : TSynAnySyn); forward;
procedure ConfigureMiscellaneous(Highlighter : TSynAnySyn); forward;

function CreateWallpaperChangerScriptHighlighter(Owner: TComponent): TSynAnySyn;
var
  Highlighter : TSynAnySyn;
begin
  Highlighter := TSynAnySyn.Create(Owner);

  ConfigureKeyWords(Highlighter);
  ConfigureConstants(Highlighter);
  ConfigureObjects(Highlighter);
  ConfigureMiscellaneous(Highlighter);

  Result := Highlighter;
end;

procedure ConfigureKeyWords(Highlighter : TSynAnySyn); inline;
var
  KeyWord : String;
begin
  for KeyWord in KEYWORDS do
    Highlighter.KeyWords.Add(KeyWord);

  Highlighter.KeyAttri.Foreground := clMaroon;
  Highlighter.KeyAttri.Style := [ fsBold ];
end;

procedure ConfigureConstants(Highlighter : TSynAnySyn); inline;
var
  ConstantWord : String;
  MeasurementUnit : TWpcTimeMeasurementUnits;
begin
  for ConstantWord in SELECTORS do
    Highlighter.Constants.Add(ConstantWord);

  for MeasurementUnit in TWpcTimeMeasurementUnits do
    Highlighter.Constants.Add(UpperCase(TimeMeasurementUnitToStr(MeasurementUnit)));

  Highlighter.ConstantAttri.Foreground := clBlue;
  Highlighter.ConstantAttri.Style := [ fsBold ];
end;

procedure ConfigureObjects(Highlighter : TSynAnySyn); inline;
var
  WallpaperStyle : TWallpaperStyle;
  ConstantWord : String;
begin
  for WallpaperStyle in TWallpaperStyle do begin
    Highlighter.Objects.Add(WallpaperStyleToStr(WallpaperStyle));
    WriteLn(WallpaperStyleToStr(WallpaperStyle));
  end;

  for ConstantWord in SEASONS do
    Highlighter.Objects.Add(ConstantWord);

  for ConstantWord in MONTHS do
    Highlighter.Objects.Add(ConstantWord);

  for ConstantWord in DAYS_OF_WEEK do
    Highlighter.Objects.Add(ConstantWord);

  //Highlighter.ObjectAttri.Foreground := clOlive;
  Highlighter.ObjectAttri.Foreground := clTeal;
end;

procedure ConfigureMiscellaneous(Highlighter : TSynAnySyn); inline;
begin
  // Numbers
  Highlighter.NumberAttri.Foreground := clBlue;

  // Variables
  Highlighter.DollarVariables := True;
  Highlighter.VariableAttri.Foreground := clPurple;

  // Paths with spaces
  Highlighter.StringDelim := sdDoubleQuote;
  Highlighter.StringAttri.Background := clCream;

  // Comments
  Highlighter.Comments := [ ];
  Highlighter.DetectPreprocessor := True;
  Highlighter.PreprocessorAttri.Foreground := clGreen;
  Highlighter.PreprocessorAttri.Style := [ fsItalic ];
end;


end.

