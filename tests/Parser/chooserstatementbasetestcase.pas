unit ChooserStatementBaseTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StrUtils,
  FpcUnit, TestRegistry,
  ParserBaseTestCase,
  WpcStatements,
  WpcScriptParser;

type

  { TAbstractChooserStatementParsingTest }

  TAbstractChooserStatementParsingTest = class abstract(TParserBaseTestCase)
  protected const
    SELECTORS : Array[TWpcSelector] of String = (
      WEIGHT_KEYWORD, SEASON_KEYWORD, WEEKDAY_KEYWORD, MONTH_KEYWORD, DATE_KEYWORD, TIME_KEYWORD, DATETIME_KEYWORD
    );
    SELECTORS_VALUES_STRING : Array[TWpcSelector] of Array[1..2] of String = (
      ('1', '2'),
      ('SPRING', 'SUMMER'),
      ('WEDNESDAY', 'THURSDAY'),
      ('MARCH', 'MAY'),
      ('25.01', '10.02'),
      ('10:05', '22:10'),
      ('25.01-10:05', '10.02-22:10')
    );
    SELECTORS_VALUES : Array[TWpcSelector] of Array[1..2] of LongWord = (
      (1, 2),
      (2, 3),
      (4, 5),
      (3, 5),
      (25, 41),
      (36300, 79800),
      (2109900, 3535800)
    );
    DEFAULT_WEIGHT_SELECTOR_VALUE = 1;
  protected
    ChooserItems : TListOfChooserItems;
  protected
    ChooserType  : String;
    ChooserItem1 : String;
    ChooserItem2 : String;
  protected
    Selector       : TWpcSelector;
    SelectorString : String;
  protected
    procedure AddRequiredObjectsIntoScript(); virtual; abstract;
  published
    procedure ShouldParseChooserByDefaultSelector();
    procedure ShouldParseChooserByWeightSelector();
    procedure ShouldParseChooserBySeasonSelector();
    procedure ShouldParseChooserByWeekdaySelector();
    procedure ShouldParseChooserByMonthSelector();
    procedure ShouldParseChooserByDateSelector();
    procedure ShouldParseChooserByTimeSelector();
    procedure ShouldParseChooserByDateTimeSelector();
    procedure ShouldParseChooserByDefaultSelectorWithDefaultSlectorValue();

    procedure SholudRaiseScriptParseExceptionWhenChooserStatementUncompleted();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfChooserHeader();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedIntoChooserHeader();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfChooserCloser();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedIntoChooserCloser();

    procedure SholudRaiseScriptParseExceptionWhenNoRequiredSelectorSpecified();
    procedure SholudRaiseScriptParseExceptionWhenNoRequiredSelectorValueSpecified();
    procedure SholudRaiseScriptParseExceptionWhenChooserContainsOnlyOneItem();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeSelector();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterSelector();
    procedure SholudRaiseScriptParseExceptionWhenDuplicateNonWeightSelectorValueFound();
    procedure SholudRaiseScriptParseExceptionWhenWrongSelectorKeywordSpecified();
  end;

implementation

{ TAbstractChooserStatementParsingTest }

// CHOOSE <STMT> FROM
//   Item1 WEIGHT 1
//   Item2 WEIGHT 2
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.ShouldParseChooserByDefaultSelector();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 1]);
  ScriptLines.Add(ChooserItem2 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_WEIGHT, 1], ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_WEIGHT, 2], ChooserItems[1].Weight);
end;

// CHOOSE <STMT> BY WEIGHT FROM
//   Item1 WEIGHT 1
//   Item2 WEIGHT 2
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.ShouldParseChooserByWeightSelector();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + WEIGHT_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 1]);
  ScriptLines.Add(ChooserItem2 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_WEIGHT, 1], ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_WEIGHT, 2], ChooserItems[1].Weight);
end;

// CHOOSE <STMT> BY SEASON FROM
//   Item1 SEASON SPRING
//   Item2 SEASON SUMMER
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.ShouldParseChooserBySeasonSelector();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + SEASON_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1 + ' ' + SEASON_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_SEASON, 1]);
  ScriptLines.Add(ChooserItem2 + ' ' + SEASON_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_SEASON, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_SEASON, 1], ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_SEASON, 2], ChooserItems[1].Weight);
end;

// CHOOSE <STMT> BY WEEKDAY FROM
//   Item1 WEEKDAY WEDNESDAY
//   Item2 WEEKDAY THURSDAY
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.ShouldParseChooserByWeekdaySelector();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + WEEKDAY_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1 + ' ' + WEEKDAY_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEEKDAY, 1]);
  ScriptLines.Add(ChooserItem2 + ' ' + WEEKDAY_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEEKDAY, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_WEEKDAY, 1], ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_WEEKDAY, 2], ChooserItems[1].Weight);
end;

// CHOOSE <STMT> BY MONTH FROM
//   Item1 MONTH MARCH
//   Item2 MONTH MAY
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.ShouldParseChooserByMonthSelector();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + MONTH_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1 + ' ' + MONTH_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_MONTH, 1]);
  ScriptLines.Add(ChooserItem2 + ' ' + MONTH_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_MONTH, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_MONTH, 1], ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_MONTH, 2], ChooserItems[1].Weight);
end;

// CHOOSE <STMT> BY DATE FROM
//   Item1 DATE 25.01
//   Item2 DATE 10.10
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.ShouldParseChooserByDateSelector();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + DATE_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1 + ' ' + DATE_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_DATE, 1]);
  ScriptLines.Add(ChooserItem2 + ' ' + DATE_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_DATE, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_DATE, 1], ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_DATE, 2], ChooserItems[1].Weight);
end;

// CHOOSE <STMT> BY TIME FROM
//   Item1 TIME 10:05
//   Item2 TIME 22:10
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.ShouldParseChooserByTimeSelector();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + TIME_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1 + ' ' + TIME_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_TIME, 1]);
  ScriptLines.Add(ChooserItem2 + ' ' + TIME_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_TIME, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_TIME, 1], ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_TIME, 2], ChooserItems[1].Weight);
end;

// CHOOSE <STMT> BY DATETIME FROM
//   Item1 DATETIME 25.01-10:05
//   Item2 DATETIME 10.10-22:10
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.ShouldParseChooserByDateTimeSelector();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + DATETIME_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1 + ' ' + DATETIME_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_DATETIME, 1]);
  ScriptLines.Add(ChooserItem2 + ' ' + DATETIME_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_DATETIME, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_DATETIME, 1], ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_DATETIME, 2], ChooserItems[1].Weight);
end;

// CHOOSE <STMT> FROM
//   Item1
//   Item2
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.ShouldParseChooserByDefaultSelectorWithDefaultSlectorValue();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + WEIGHT_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1);
  ScriptLines.Add(ChooserItem2 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, DEFAULT_WEIGHT_SELECTOR_VALUE, ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, SELECTORS_VALUES[S_WEIGHT, 2], ChooserItems[1].Weight);
end;

// CHOOSE <STMT> BY WEIGHT FROM
//   Item1 WEIGHT 1
//   Item2 WEIGHT 2
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenChooserStatementUncompleted();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + WEIGHT_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1);
  ScriptLines.Add(ChooserItem2 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 2]);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  AssertScriptParseExceptionOnParse(4, 1);
end;

// CHOOSE <STMT> BY WEIGHT FROM LIST
//   Item1 WEIGHT 1
//   Item2 WEIGHT 2
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfChooserHeader();
var
  WordsInHeader : Integer;
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + WEIGHT_KEYWORD + ' ' + FROM_KEYWORD + ' LIST');
  ScriptLines.Add(ChooserItem1);
  ScriptLines.Add(ChooserItem2 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  WordsInHeader := WordCount(ScriptLines[1], WHITESPACE_SET);
  AssertScriptParseExceptionOnParse(1, WordsInHeader - 1);
end;

// CHOOSE <STMT> BY WEIGHT ONLY FROM
//   Item1 WEIGHT 1
//   Item2 WEIGHT 2
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedIntoChooserHeader();
var
  WordsInHeader : Integer;
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + WEIGHT_KEYWORD + ' ONLY ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1);
  ScriptLines.Add(ChooserItem2 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  WordsInHeader := WordCount(ScriptLines[1], WHITESPACE_SET);
  AssertScriptParseExceptionOnParse(1, WordsInHeader - 1 - 1);
end;

// CHOOSE <STMT> BY WEIGHT FROM
//   Item1 WEIGHT 1
//   Item2 WEIGHT 2
// END CHOOSE BY WEIGHT
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfChooserCloser();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + WEIGHT_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1);
  ScriptLines.Add(ChooserItem2 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 2]);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD + ' ' + BY_KEYWORD + ' ' + WEIGHT_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  AssertScriptParseExceptionOnParse(4, 2);
end;

// CHOOSE <STMT> BY WEIGHT FROM
//   Item1 WEIGHT 1
//   Item2 WEIGHT 2
// END THIS CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedIntoChooserCloser();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + WEIGHT_KEYWORD + ' ' + FROM_KEYWORD);
  ScriptLines.Add(ChooserItem1);
  ScriptLines.Add(ChooserItem2 + ' ' + WEIGHT_KEYWORD + ' ' + SELECTORS_VALUES_STRING[S_WEIGHT, 2]);
  ScriptLines.Add(END_KEYWORD + ' THIS ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  AssertScriptParseExceptionOnParse(4, 1);
end;

// CHOOSE <STMT> BY <SLCTR> FROM
//   Item1 <SLCTR> <SL_VAL1>
//   Item2
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenNoRequiredSelectorSpecified();
var
  WordsInSecondOption : Integer;
begin
  for Selector in TWpcSelector do begin
    if (Selector = S_WEIGHT) then
      continue;

    SelectorString := SELECTORS[Selector];

    ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + SelectorString + ' ' + FROM_KEYWORD);
    ScriptLines.Add(ChooserItem1 + ' ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 1]);
    ScriptLines.Add(ChooserItem2);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);
    AddRequiredObjectsIntoScript();

    WordsInSecondOption := WordCount(ScriptLines[3], WHITESPACE_SET);
    AssertScriptParseExceptionOnParse(3, WordsInSecondOption - 1);

    ScriptLines.Clear();
  end;
end;

// CHOOSE <STMT> BY <SLCTR> FROM
//   Item1 <SLCTR> <SL_VAL1>
//   Item2 <SLCTR>
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenNoRequiredSelectorValueSpecified();
var
  WordsInSecondOption : Integer;
begin
  for Selector in TWpcSelector do begin
    SelectorString := SELECTORS[Selector];

    ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + SelectorString + ' ' + FROM_KEYWORD);
    ScriptLines.Add(ChooserItem1 + ' ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 1]);
    ScriptLines.Add(ChooserItem2 + ' ' + SelectorString);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);
    AddRequiredObjectsIntoScript();

    WordsInSecondOption := WordCount(ScriptLines[3], WHITESPACE_SET);
    AssertScriptParseExceptionOnParse(3, WordsInSecondOption - 1);

    ScriptLines.Clear();
  end;
end;

// CHOOSE <STMT> BY <SLCTR> FROM
//   Item1 <SLCTR> <SL_VAL1>
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenChooserContainsOnlyOneItem();
begin
  for Selector in TWpcSelector do begin
    SelectorString := SELECTORS[Selector];

    ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + SelectorString + ' ' + FROM_KEYWORD);
    ScriptLines.Add(ChooserItem1 + ' ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 1]);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);
    AddRequiredObjectsIntoScript();

    AssertScriptParseExceptionOnParse();

    ScriptLines.Clear();
  end;
end;

// CHOOSE <STMT> BY <SLCTR> FROM
//   Item1 <SLCTR> <SL_VAL1>
//   Item2 BY <SLCTR> <Sl_VAL2>
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeSelector();
var
  WordsInSecondOption : Integer;
begin
  for Selector in TWpcSelector do begin
    SelectorString := SELECTORS[Selector];

    ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + SelectorString + ' ' + FROM_KEYWORD);
    ScriptLines.Add(ChooserItem1 + ' ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 1]);
    ScriptLines.Add(ChooserItem2 + ' BY ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 2]);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);
    AddRequiredObjectsIntoScript();

    WordsInSecondOption := WordCount(ScriptLines[3], WHITESPACE_SET);
    AssertScriptParseExceptionOnParse(3, WordsInSecondOption - 1 - 2);

    ScriptLines.Clear();
  end;
end;

// CHOOSE <STMT> BY <SLCTR> FROM
//   Item1 <SLCTR> <SL_VAL1>
//   Item2 <SLCTR> HERE <Sl_VAL2>
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterSelector();
var
  WordsInSecondOption : Integer;
begin
  for Selector in TWpcSelector do begin
    SelectorString := SELECTORS[Selector];

    ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + SelectorString + ' ' + FROM_KEYWORD);
    ScriptLines.Add(ChooserItem1 + ' ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 1]);
    ScriptLines.Add(ChooserItem2 + ' ' + SelectorString + ' HERE ' + SELECTORS_VALUES_STRING[Selector, 2]);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);
    AddRequiredObjectsIntoScript();

    WordsInSecondOption := WordCount(ScriptLines[3], WHITESPACE_SET);
    AssertScriptParseExceptionOnParse(3, WordsInSecondOption - 1 - 2);

    ScriptLines.Clear();
  end;
end;

// CHOOSE <STMT> BY <SLCTR> FROM
//   Item1 <SLCTR> <SL_VAL1>
//   Item2 <SLCTR> <Sl_VAL2>
//   Item1 <SLCTR> <Sl_VAL1>
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenDuplicateNonWeightSelectorValueFound();
begin
  for Selector in TWpcSelector do begin
    if (Selector = S_WEIGHT) then
      continue;

    SelectorString := SELECTORS[Selector];

    ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + SelectorString + ' ' + FROM_KEYWORD);
    ScriptLines.Add(ChooserItem1 + ' ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 1]);
    ScriptLines.Add(ChooserItem2 + ' ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 2]);
    ScriptLines.Add(ChooserItem1 + ' ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 1]);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);
    AddRequiredObjectsIntoScript();

    AssertScriptParseExceptionOnParse(4);

    ScriptLines.Clear();
  end;
end;

// CHOOSE <STMT> BY <SLCTR> FROM
//   Item1 <SLCTR> <SL_VAL1>
//   Item2 <WRONG-SLCTR> <Sl_VAL2>
// END CHOOSE
procedure TAbstractChooserStatementParsingTest.SholudRaiseScriptParseExceptionWhenWrongSelectorKeywordSpecified();
const
  WRONG_SELECTORS : Array[TWpcSelector] of String = (
    SEASON_KEYWORD, WEEKDAY_KEYWORD, MONTH_KEYWORD, DATE_KEYWORD, TIME_KEYWORD, DATETIME_KEYWORD, WEIGHT_KEYWORD
  );
var
  WordsInSecondOption : Integer;
begin
  for Selector in TWpcSelector do begin
    SelectorString := SELECTORS[Selector];

    ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + BY_KEYWORD + ' ' + SelectorString + ' ' + FROM_KEYWORD);
    ScriptLines.Add(ChooserItem1 + ' ' + SelectorString + ' ' + SELECTORS_VALUES_STRING[Selector, 1]);
    ScriptLines.Add(ChooserItem2 + ' ' + WRONG_SELECTORS[Selector] + ' ' + SELECTORS_VALUES_STRING[Selector, 2]);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);
    AddRequiredObjectsIntoScript();

    WordsInSecondOption := WordCount(ScriptLines[3], WHITESPACE_SET);
    if (Selector = S_WEIGHT) then
      AssertScriptParseExceptionOnParse(3, WordsInSecondOption - 1 - 1)
    else
      AssertScriptParseExceptionOnParse(3, WordsInSecondOption - 1);

    ScriptLines.Clear();
  end;
end;


end.

