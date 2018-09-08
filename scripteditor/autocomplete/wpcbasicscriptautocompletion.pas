unit WpcBasicScriptAutocompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractDynamicScriptAutocompletion,
  WpcScriptParser;

type

  { TWpcBasicScriptAutocompletion }
  // Provides completion based on simple analyzation of current position.
  // Do not handle statement position or validity of the options.
  TWpcBasicScriptAutocompletion = class(TWpcAbstractDynamicScriptAutocompletion)
  protected
    procedure DeterminePossibleOptions(); override;
  private
    function GetPreviousWord() : String;
    function WordInArray(Word : String; Words : Array of String) : Boolean;

    procedure SearchForAndAddToCacheBranchNames(); inline;
    procedure AddLineFirstWords(); inline;
  end;


implementation


{ TWpcBasicScriptAutocompletion }

procedure TWpcBasicScriptAutocompletion.DeterminePossibleOptions();
var
  PreviousWord : String;
begin
  if ((FCurrentLine = '') or (FCurrentLineWords.Count = 1)) then begin
    AddLineFirstWords();
    exit;
  end;

  PreviousWord := GetPreviousWord();
  case (PreviousWord) of
    END_KEYWORD: begin
      CacheOption(BRANCH_KEYWORD);
      CacheOption(CHOOSE_KEYWORD);

      CacheOption(IMAGES_KEYWORD);
      CacheOption(DIRECTORIES_KEYWORD);
      CacheOption(DELAYS_KEYWORD);
      CacheOption(DEFAULTS_KEYWORD);
    end;
    DELAY_KEYWORD: begin
      CacheOption(UNITS_KEYWORD); // defaults section
    end;
    UNITS_KEYWORD: begin
      AddToCacheAllMeasurementUnits(); // default measurement unit
    end;
    WALLPAPER_KEYWORD: begin
      CacheOption(TWpcAbstractDynamicScriptAutocompletion.FILE_PATH_INSERTION);
      CacheOption(FROM_KEYWORD);  // set directory, wallpaper chooser
      CacheOption(BY_KEYWORD);    // wallpaper chooser header
      CacheOption(STYLE_KEYWORD); // defaults section
    end;
    BRANCH_KEYWORD: begin
      if (SafeGet(FCurrentLineWords, 0) <> BRANCH_KEYWORD) then
        SearchForAndAddToCacheBranchNames();
    end;
    WAIT_KEYWORD: begin
      CacheOption(FOR_KEYWORD);
    end;
    SET_KEYWORD: begin
      CacheOption(WALLPAPER_KEYWORD);
    end;
    FROM_KEYWORD: begin
      CacheOption(DIRECTORY_KEYWORD);
    end;
    DIRECTORY_KEYWORD: begin
      CacheOption(TWpcAbstractDynamicScriptAutocompletion.DIR_PATH_INSERTION);
    end;
    STOP_KEYWORD: begin
      CacheOption(WITH_KEYWORD);
    end;
    SWITCH_KEYWORD: begin
      CacheOption(TO_KEYWORD);   // switch to branch
      CacheOption(BY_KEYWORD);   // branch to switch chooser header
      CacheOption(FROM_KEYWORD); // branch to switch chooser header (default selector)
    end;
    TO_KEYWORD: begin
      CacheOption(BRANCH_KEYWORD); // switch to branch
      CacheOption(USE_KEYWORD);    // branch to use chooser header
      CacheOption(SWITCH_KEYWORD); // branch to switch chooser header
    end;
    USE_KEYWORD: begin
      CacheOption(BRANCH_KEYWORD);
    end;
    CHOOSE_KEYWORD: begin
      CacheOption(WALLPAPER_KEYWORD); // wallpaper chooser header
      CacheOption(BRANCH_KEYWORD);    // branch to use / brnch to switch chooser header
    end;
    BY_KEYWORD: begin
      AddToCacheAllSelectors();
    end;
    WITH_KEYWORD: begin
      CacheOption(PROBABILITY_KEYWORD);
    end;
    STYLE_KEYWORD: begin
      AddToCacheAllWallpaperStyles();
    end;
    WEIGHT_KEYWORD,
    DATE_KEYWORD,
    TIME_KEYWORD,
    DATETIME_KEYWORD: begin
      if (SafeGet(FCurrentLineWords, 0) = CHOOSE_KEYWORD) then
        CacheOption(FROM_KEYWORD);
    end;
    SEASON_KEYWORD: begin
      if (SafeGet(FCurrentLineWords, 0) <> CHOOSE_KEYWORD) then
        AddToCacheSeasons();
    end;
    WEEKDAY_KEYWORD: begin
      if (SafeGet(FCurrentLineWords, 0) <> CHOOSE_KEYWORD) then
        AddToCacheWeekdays();
    end;
    MONTH_KEYWORD: begin
      if (SafeGet(FCurrentLineWords, 0) <> CHOOSE_KEYWORD) then
        AddToCacheMonths();
    end;
    IMAGES_KEYWORD,
    DIRECTORIES_KEYWORD,
    DELAYS_KEYWORD,
    DEFAULTS_KEYWORD,
    FOR_KEYWORD,
    PROBABILITY_KEYWORD,
    TIMES_KEYWORD:
      // no completion.
    else begin
      if (WordInArray(PreviousWord, SEASONS) or WordInArray(PreviousWord, MONTHS) or WordInArray(PreviousWord, DAYS_OF_WEEK)) then
        // no completion
        exit;

      FCachedOptions.Assign(FStaticOptions);
    end;
  end;
end;

{
  Returns word previous to word under completion.
  If no such word, empty string will be returned.
}
function TWpcBasicScriptAutocompletion.GetPreviousWord() : String;
begin
  if (FCurrentWordIndex > 0) then
    Result := SafeGet(FCurrentLineWords, FCurrentWordIndex - 1)
  else
    Result := '';
end;

function TWpcBasicScriptAutocompletion.WordInArray(Word : String; Words : Array of String) : Boolean;
var
  AWord : String;
begin
  for AWord in Words do begin
    if (AWord = Word) then begin
      Result := True;
      exit;
    end;
  end;

  Result := False;
end;

procedure TWpcBasicScriptAutocompletion.SearchForAndAddToCacheBranchNames();
var
  i         : Integer;
  Line      : String;
  LineWords : TStrings;
begin
  for i:=0 to (FScriptLines.Count - 1) do begin
    Line := GetLine(i);
    if (Line.StartsWith(BRANCH_KEYWORD)) then begin
      LineWords := GetLineWords(Line);
      try
        if (LineWords.Count = 2) then
          CacheOption(LineWords[1]); // cache branch name
      finally
        LineWords.Free();
      end;
    end;
  end;
end;

procedure TWpcBasicScriptAutocompletion.AddLineFirstWords();
begin
  CacheOption(SET_KEYWORD);    // set wallpaper / directory
  CacheOption(WAIT_KEYWORD);   // wait
  CacheOption(END_KEYWORD);    // end of branch / header section / chooser
  CacheOption(BRANCH_KEYWORD); // branch header
  CacheOption(USE_KEYWORD);    // use branch
  CacheOption(SWITCH_KEYWORD); // switch branch
  CacheOption(CHOOSE_KEYWORD); // a chooser
  CacheOption(STOP_KEYWORD);   // stop script

  CacheOption(DIRECTORIES_KEYWORD); // directories section header
  CacheOption(IMAGES_KEYWORD);      // images section header
  CacheOption(DELAYS_KEYWORD);      // delays section header
  CacheOption(DEFAULTS_KEYWORD);    // defaults section geader
  CacheOption(DELAY_KEYWORD);       // defaults section, delay / delay units
  CacheOption(WALLPAPER_KEYWORD);   // defaults section, wallpaper style
  CacheOption(DIRECTORY_KEYWORD);   // defaults section, base path
end;


end.

