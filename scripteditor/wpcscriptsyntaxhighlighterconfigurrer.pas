unit WpcScriptSyntaxHighlighterConfigurrer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  SynHighlighterAny,
  WpcScriptEditorConstantsList,
  WpcScriptParser,
  WpcTimeMeasurementUnits,
  WpcWallpaperStyles;


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
begin
  Highlighter.KeyWords.AddStrings(KEYWORDS);

  Highlighter.KeyAttri.Foreground := clMaroon;
  Highlighter.KeyAttri.Style := [ fsBold ];
end;

procedure ConfigureConstants(Highlighter : TSynAnySyn); inline;
var
  MeasurementUnit : TWpcTimeMeasurementUnits;
begin
  Highlighter.Constants.AddStrings(SELECTORS);

  for MeasurementUnit in TWpcTimeMeasurementUnits do
    Highlighter.Constants.Add(UpperCase(TimeMeasurementUnitToStr(MeasurementUnit)));

  Highlighter.ConstantAttri.Foreground := clBlue;
  Highlighter.ConstantAttri.Style := [ fsBold ];
end;

procedure ConfigureObjects(Highlighter : TSynAnySyn); inline;
var
  WallpaperStyle : TWpcWallpaperStyle;
begin
  for WallpaperStyle in TWpcWallpaperStyle do
    Highlighter.Objects.Add(WallpaperStyleToStr(WallpaperStyle));

  Highlighter.Objects.AddStrings(SEASONS);

  Highlighter.Objects.AddStrings(MONTHS);

  Highlighter.Objects.AddStrings(DAYS_OF_WEEK);

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

