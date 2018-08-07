unit WpcAdvancedScriptAutocompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractDynamicScriptAutocompletion,
  WpcScriptEditorConstantsList,
  WpcWallpaperStyles,
  WpcScriptParser,
  WpcTimeMeasurementUnits;

type
  // TODO THIS IS DRAFT. FINISH IMPLEMENTATION.
  { TWpcAdvancedScriptAutocompletion }
  // Provides full script code completion.
  // Takes into account position in script and statemet (e.g. a statement is not allowed outside a branch).
  // Suggest user defined variables and provides completion for path values.
  TWpcAdvancedScriptAutocompletion = class(TWpcAbstractDynamicScriptAutocompletion)
  protected
    procedure DeterminePossibleOptions(); override;
  private
    procedure AddHeadersCompletionOptions();
    procedure AddBetweenHeadersAndBranchesCompletionOptions();
    procedure AddBranchesCompletionOptions();

    function FindHeaderSectionKeyWord(LineNumber : Integer) : String;
    procedure AddNotUsedHeaderSectionKeyWords();
    procedure AddHeaderEndSectionOptions();
    procedure AddImagePathCompletionOptions();
    procedure AddFolderPathCompletionOptions();
    procedure AddDelayCompletionOptions();
    procedure AddDefaultsCompletionOptions();
  end;


implementation


{ TWpcAdvancedScriptAutocompletion }

{
  Determines and stores in cache possible autocomplete options for current position.
}
procedure TWpcAdvancedScriptAutocompletion.DeterminePossibleOptions();
var
  i    : Integer;
  Line : String;
begin
  FCachedOptions.Clear();

  for i := 0 to FCursorPosition.Row do begin
    Line := GetLine(i);
    if (Line.StartsWith(BRANCH_KEYWORD)) then begin
      AddBranchesCompletionOptions();
      exit;
    end;
  end;

  while ((i < FScriptLines.Count) and (GetLine(i) = '')) do
    Inc(i);
  if ((i = FScriptLines.Count) or GetLine(i).StartsWith(BRANCH_KEYWORD)) then
    AddBetweenHeadersAndBranchesCompletionOptions()
  else
    AddHeadersCompletionOptions();
end;

procedure TWpcAdvancedScriptAutocompletion.AddHeadersCompletionOptions();
var
  SectionKeyWord : String;
begin
  if ((SafeGet(FCurrentLineWords, 0) = END_KEYWORD) and (FCurrentLineWords.Count <= 2)) then
    AddHeaderEndSectionOptions()
  else begin
    SectionKeyWord := FindHeaderSectionKeyWord(FCursorPosition.Row);
    if (SectionKeyWord = '') then begin
      // Script scope. Only header section keywords are allowed.
      if (FCurrentLineWords.Count <= 1) then
        AddNotUsedHeaderSectionKeyWords();
    end
    else begin
      if (FCurrentLine = '') then begin
        // TODO search down if empty line is inside section, e.g.
        // IMAGES
        // <the empty line>
        // END IMAGES
        CacheOption(END_KEYWORD);
        exit;
      end;

      if (SectionKeyWord = DEFAULTS_KEYWORD) then
        AddDefaultsCompletionOptions()
      else begin
        if ((FCurrentLineWords.Count <= 2) and (SafeGet(FCurrentLineWords, 0) <> FCurrentWord)) then
          // Completion is for second word
          case (SectionKeyWord) of
            IMAGES_KEYWORD:
              AddImagePathCompletionOptions();
            DIRECTORIES_KEYWORD:
              AddFolderPathCompletionOptions();
            DELAYS_KEYWORD:
              AddDelayCompletionOptions();
          end;
      end;
    end;
  end;
end;

procedure TWpcAdvancedScriptAutocompletion.AddBetweenHeadersAndBranchesCompletionOptions();
begin
  CacheOption(BRANCH_KEYWORD);
  AddHeadersCompletionOptions();
end;

procedure TWpcAdvancedScriptAutocompletion.AddBranchesCompletionOptions();
begin
  // TODO
end;

{
  Finds section header and returns the keyword.
  If no header empty string will be returned.
  Works only for headers section (before first barnch occurs)
  Arguments:
    - Line: number of line to start
}
function TWpcAdvancedScriptAutocompletion.FindHeaderSectionKeyWord(LineNumber : Integer): String;
var
  i       : Integer;
  Line    : String;
  KeyWord : String;
begin
  for i := LineNumber - 1 downto 0 do begin
    Line := GetLine(i);
    for KeyWord in HEADER_SECTION_KEYWORDS do
      if (Line.StartsWith(KeyWord)) then begin
        Result := KeyWord;
        exit;
      end;
    if (Line.StartsWith(END_KEYWORD)) then begin
      // End of another section found. Scope is Script.
      Result := '';
      exit;
    end;
  end;
  // No section header found.
  Result := '';
end;

procedure TWpcAdvancedScriptAutocompletion.AddNotUsedHeaderSectionKeyWords();
const
  IMAGES_INDEX = 1;
  DIRECTORIES_INDEX = 2;
  DELAYS_INDEX = 3;
  DEFAULTS_INDEX = 4;
var
  i       : Integer;
  Line    : String;

  HeaderKeyWordsPresence : Array[1..4] of Boolean;
begin
  for i := 1 to 4 do
    HeaderKeyWordsPresence[i] := False;

  i := 0;
  repeat
    Line := GetLine(i);

    if (not HeaderKeyWordsPresence[IMAGES_INDEX] and Line.StartsWith(IMAGES_KEYWORD)) then
      HeaderKeyWordsPresence[IMAGES_INDEX] := True
    else if (not HeaderKeyWordsPresence[DIRECTORIES_INDEX] and Line.StartsWith(DIRECTORIES_KEYWORD)) then
      HeaderKeyWordsPresence[DIRECTORIES_INDEX] := True
    else if (not HeaderKeyWordsPresence[DELAYS_INDEX] and Line.StartsWith(DELAYS_KEYWORD)) then
      HeaderKeyWordsPresence[DELAYS_INDEX] := True
    else if (not HeaderKeyWordsPresence[DEFAULTS_INDEX] and Line.StartsWith(DEFAULTS_KEYWORD)) then
      HeaderKeyWordsPresence[DEFAULTS_INDEX] := True;

    Inc(i);
  until (i = FCursorPosition.Row);

  if (not HeaderKeyWordsPresence[IMAGES_INDEX]) then CacheOption(IMAGES_KEYWORD);
  if (not HeaderKeyWordsPresence[DIRECTORIES_INDEX]) then CacheOption(DIRECTORIES_KEYWORD);
  if (not HeaderKeyWordsPresence[DELAYS_INDEX]) then CacheOption(DELAYS_KEYWORD);
  if (not HeaderKeyWordsPresence[DEFAULTS_INDEX]) then CacheOption(DEFAULTS_KEYWORD);
end;

procedure TWpcAdvancedScriptAutocompletion.AddHeaderEndSectionOptions();
var
  KeyWord : String;
begin
  KeyWord := FindHeaderSectionKeyWord(FCursorPosition.Row);
  if (KeyWord = '') then
    // No section header found. Show all possible cases.
    AddNotUsedHeaderSectionKeyWords()
  else if (KeyWord <> END_KEYWORD) then
    CacheOption(KeyWord);
end;

procedure TWpcAdvancedScriptAutocompletion.AddImagePathCompletionOptions();
begin
  // TODO
end;

procedure TWpcAdvancedScriptAutocompletion.AddFolderPathCompletionOptions();
begin
  // TODO
end;

procedure TWpcAdvancedScriptAutocompletion.AddDelayCompletionOptions();
var
  Delay           : String;
  Unused          : Integer;
  MeasurementUnit : TWpcTimeMeasurementUnits;
begin
  Delay := SafeGet(FCurrentLineWords, 1);
  if (Delay <> '') then
    if (TryStrToInt(Delay, Unused)) then
      // Add measurement units
      for MeasurementUnit in TWpcTimeMeasurementUnits do
        CacheOption(TimeMeasurementUnitToStr(MeasurementUnit));
    // TODO handle ms completion if m is already typed
end;

procedure TWpcAdvancedScriptAutocompletion.AddDefaultsCompletionOptions();
begin
  // TODO
end;


end.

