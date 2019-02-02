unit WpcBasicScriptAutocompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractDynamicScriptAutocompletion,
  WpcScriptParser,
  WpcExceptions;

type

  { TWpcBasicScriptAutocompletion }
  // Provides completion based on simple analyzation of current position.
  // Do not handle statement position or validity of the options.
  TWpcBasicScriptAutocompletion = class(TWpcAbstractDynamicScriptAutocompletion)
  protected
    procedure DeterminePossibleOptions(); override;
  private
    function GetPreviousWord() : String;
    function GetFirstWord() : String; inline;
    function WordInArray(Word : String; Words : Array of String) : Boolean;

    procedure CacheVariablesFromSection(SectionKeyWord : String);

    procedure SearchForAndAddToCacheBranchNames(); inline;
    procedure AddLineFirstWords(); inline;
  end;


implementation


{ TWpcBasicScriptAutocompletion }

procedure TWpcBasicScriptAutocompletion.DeterminePossibleOptions();
var
  PreviousWord : String;
  FirstWord    : String;
begin
  if ((FCurrentLine = '') or (FCurrentLineWords.Count = 1)) then begin
    AddLineFirstWords();
    SearchForAndAddToCacheBranchNames();            // branch choosers item
    CacheVariablesFromSection(IMAGES_KEYWORD);      // wallpaper chooser item
    CacheVariablesFromSection(DIRECTORIES_KEYWORD); // wallpaper chooser item
    exit;
  end;


  PreviousWord := UpperCase(GetPreviousWord());
  FirstWord := UpperCase(GetFirstWord());
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
      case (FirstWord) of
        SET_KEYWORD: begin
          CacheOption(TWpcAbstractDynamicScriptAutocompletion.FILE_PATH_INSERTION);
          CacheOption(FROM_KEYWORD); // set directory
          CacheVariablesFromSection(IMAGES_KEYWORD);
          CacheVariablesFromSection(DIRECTORIES_KEYWORD);
        end;
        CHOOSE_KEYWORD: begin
          CacheOption(FROM_KEYWORD);  // wallpaper chooser
          CacheOption(BY_KEYWORD);    // wallpaper chooser header
        end;
        WALLPAPER_KEYWORD:
          CacheOption(STYLE_KEYWORD); // defaults section
      end;
    end;
    BRANCH_KEYWORD: begin
      if (FirstWord = CHOOSE_KEYWORD) then
        CacheOption(TO_KEYWORD)              // branch choosers header
      else if (FirstWord <> BRANCH_KEYWORD) then
        SearchForAndAddToCacheBranchNames(); // use or swith to a branch
    end;
    WAIT_KEYWORD: begin
      CacheOption(FOR_KEYWORD);
      CacheOption(TILL_KEYWORD);
      CacheVariablesFromSection(DELAYS_KEYWORD);
    end;
    SET_KEYWORD: begin
      CacheOption(WALLPAPER_KEYWORD);
    end;
    FROM_KEYWORD: begin
      if (FirstWord = SET_KEYWORD) then
        CacheOption(DIRECTORY_KEYWORD); // set directory
    end;
    DIRECTORY_KEYWORD: begin
      CacheOption(TWpcAbstractDynamicScriptAutocompletion.DIR_PATH_INSERTION);
      CacheVariablesFromSection(DIRECTORIES_KEYWORD);
    end;
    STOP_KEYWORD: begin
      CacheOption(WITH_KEYWORD);
    end;
    SWITCH_KEYWORD: begin
      case (FirstWord) of
        SWITCH_KEYWORD:
          CacheOption(TO_KEYWORD);   // switch to branch
        CHOOSE_KEYWORD: begin
          CacheOption(BY_KEYWORD);   // branch to switch chooser header
          CacheOption(FROM_KEYWORD); // branch to switch chooser header (default selector)
        end;
      end;
    end;
    TO_KEYWORD: begin
      case (FirstWord) of
        SWITCH_KEYWORD:
          CacheOption(BRANCH_KEYWORD); // switch to branch
        CHOOSE_KEYWORD: begin
          CacheOption(USE_KEYWORD);    // branch to use chooser header
          CacheOption(SWITCH_KEYWORD); // branch to switch chooser header
        end;
      end;
    end;
    USE_KEYWORD: begin
      case (FirstWord) of
        USE_KEYWORD:
          CacheOption(BRANCH_KEYWORD); // use branch
        CHOOSE_KEYWORD: begin
          CacheOption(BY_KEYWORD);     // branch to switch chooser header
          CacheOption(FROM_KEYWORD);   // branch to use chooser header
        end;
      end;
    end;
    FOR_KEYWORD: begin
      CacheVariablesFromSection(DELAYS_KEYWORD);
    end;
    CHOOSE_KEYWORD: begin
      if (FirstWord = CHOOSE_KEYWORD) then begin
        CacheOption(WALLPAPER_KEYWORD); // wallpaper chooser header
        CacheOption(BRANCH_KEYWORD);    // branch to use / brnch to switch chooser header
      end;
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
      if (FirstWord = CHOOSE_KEYWORD) then
        CacheOption(FROM_KEYWORD);
    end;
    SEASON_KEYWORD: begin
      if (FirstWord <> CHOOSE_KEYWORD) then
        AddToCacheSeasons()
      else
        CacheOption(FROM_KEYWORD); // chooser header
    end;
    WEEKDAY_KEYWORD: begin
      if (FirstWord <> CHOOSE_KEYWORD) then
        AddToCacheWeekdays()
      else
        CacheOption(FROM_KEYWORD); // chooser header
    end;
    MONTH_KEYWORD: begin
      if (FirstWord <> CHOOSE_KEYWORD) then
        AddToCacheMonths()
      else
        CacheOption(FROM_KEYWORD); // chooser header
    end;
    IMAGES_KEYWORD,
    DIRECTORIES_KEYWORD,
    DELAYS_KEYWORD,
    DEFAULTS_KEYWORD,
    TILL_KEYWORD,
    PROBABILITY_KEYWORD:
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

{
  Returns the first word in the current line.
}
function TWpcBasicScriptAutocompletion.GetFirstWord() : String;
begin
  Result := SafeGet(FCurrentLineWords, 0);
end;

function TWpcBasicScriptAutocompletion.WordInArray(Word: String;
  Words: array of String): Boolean;
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

{
  Adds variables from header section into autocompletion cache.
  The possible sections are: directories, images and delays.
}
procedure TWpcBasicScriptAutocompletion.CacheVariablesFromSection(SectionKeyWord : String);
var
  i         : Integer;
  LineWords : TStrings;
begin
  if (not WordInArray(SectionKeyWord, [DIRECTORIES_KEYWORD, IMAGES_KEYWORD, DELAYS_KEYWORD])) then
    raise TWpcIllegalArgumentException.Create('Unknown header section: ' + SectionKeyWord);

  i := 0;
  while (i < FScriptLines.Count) do begin
    if (GetLine(i).StartsWith(SectionKeyWord)) then begin
      Inc(i);
      while (i < FScriptLines.Count) do begin
        LineWords := GetLineWords(GetLine(i));
        try
          if (LineWords.Count = 2) then begin
            if ((LineWords[0] = END_KEYWORD) and (LineWords[1] = SectionKeyWord)) then
              // end of the section
              break;

            CacheOption(VARIABLE_START_SYMBOL + LineWords[0]); // cache variable name
          end
          else
            // syntax error, just skip rest
            break;
        finally
          LineWords.Free();
        end;
        Inc(i);
      end;
      // processing of given section is done
      exit;
    end;
    Inc(i);
  end;
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

