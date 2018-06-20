unit SetDirectoryStatementParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  OSUtils,
  WpcScriptCommons,
  WpcDirectoryStatement,
  WpcScriptParser,
  WpcWallpaperStyles;

const
  SET_DIRECTORY = SET_KEYWORD + ' ' + DIRECTORY_KEYWORD + ' ';
  DIRECTORY_WITH_WALLPAPERS = 'path' + PATH_SEPARATOR + 'wallpapers';
type

  { TSetDirectoryStatementParsingTest }

  TSetDirectoryStatementParsingTest = class(TParserBaseTestCase)
  protected
    DirectoryStatement : TWpcDirectoryStatement;
  published
    procedure ShouldParseBaseDirectoryStatement();
    procedure ShouldParseDirectoryStatementWithRecursiveFlag();
    procedure ShouldParseDirectoryStatementWithOrderedFlag();
    procedure ShouldParseDirectoryStatementWithAllFlags();
    procedure ShouldParseDirectoryStatementWithStyleProperty();
    procedure ShouldParseDirectoryStatementWithDelayProperty();
    procedure ShouldParseDirectoryStatementWithProbabilityProperty();
    procedure ShouldParseDirectoryStatementWithTimesProperty();
    procedure ShouldParseDirectoryStatementWithAllProperties();
    procedure ShouldParseDirectoryStatementWithAllFlagsAndAllProperties();

    procedure SholudRaiseScriptParseExceptionWhenNoDirectorySpecified();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeFlags();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBetweenFlags();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterFlags();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBetweenProperties();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
    procedure SholudRaiseScriptParseExceptionWhenRecursiveFlagIsAddedBetweenProperties();
    procedure SholudRaiseScriptParseExceptionWhenOrderedFlagIsAddedBetweenProperties();
    procedure SholudRaiseScriptParseExceptionWhenRecursiveFlagIsAddedAtTheEndOfProperties();
    procedure SholudRaiseScriptParseExceptionWhenOrderedFlagIsAddedAtTheEndOfProperties();
  end;

implementation

{ TSetDirectoryStatementParsingTest }

// SET DIRECTORY path/wallpaper
procedure TSetDirectoryStatementParsingTest.ShouldParseBaseDirectoryStatement();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertFalse(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsOrdered()); // false should be default value
  AssertFalse(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsRecurcive()); // false should be default value
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, CENTER = DirectoryStatement.GetStyle()); // CENTER should be default value
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, 0, DirectoryStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_PROBABILITY, DirectoryStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_TIMES, DirectoryStatement.GetTimes());
end;

// SET DIRECTORY path/wallpaper RECURSIVE
procedure TSetDirectoryStatementParsingTest.ShouldParseDirectoryStatementWithRecursiveFlag();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + ' ' + RECURSIVE_KEYWORD);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsRecurcive());
  AssertFalse(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsOrdered());
end;

// SET DIRECTORY path/wallpaper ORDERED
procedure TSetDirectoryStatementParsingTest.ShouldParseDirectoryStatementWithOrderedFlag();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + ' ' + ORDERED_KEYWORD);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsOrdered());
  AssertFalse(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsRecurcive());
end;

// SET DIRECTORY path/wallpaper ORDERED RECURSIVE
procedure TSetDirectoryStatementParsingTest.ShouldParseDirectoryStatementWithAllFlags();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + ' ' + ORDERED_KEYWORD + ' ' + RECURSIVE_KEYWORD);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsRecurcive());
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsOrdered());
end;

// SET DIRECTORY path/wallpaper STYLE STRETCH
procedure TSetDirectoryStatementParsingTest.ShouldParseDirectoryStatementWithStyleProperty();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + STYLE_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetStyle() = TEST_DEFAULT_STYLE_VALUE);
end;

// SET DIRECTORY path/wallpaper FOR 5m
procedure TSetDirectoryStatementParsingTest.ShouldParseDirectoryStatementWithDelayProperty();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + DELAY_FOR_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, DirectoryStatement.GetDelay());
end;

// SET DIRECTORY path/wallpaper WITH PROBABILITY 50
procedure TSetDirectoryStatementParsingTest.ShouldParseDirectoryStatementWithProbabilityProperty();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, DirectoryStatement.GetProbability());
end;

// SET DIRECTORY path/wallpaper 4 TIMES
procedure TSetDirectoryStatementParsingTest.ShouldParseDirectoryStatementWithTimesProperty();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_TIMES_VALUE, DirectoryStatement.GetTimes());
end;

// SET DIRECTORY path/wallpaper STYLE STRETCH FOR 5m WITH PROBABILITY 50 4 TIMES
procedure TSetDirectoryStatementParsingTest.ShouldParseDirectoryStatementWithAllProperties();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + STYLE_PROPERTY + DELAY_FOR_PROPERTY + WITH_PROBABILITY_PROPERTY + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetStyle() = TEST_DEFAULT_STYLE_VALUE);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, DirectoryStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, DirectoryStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_TIMES_VALUE, DirectoryStatement.GetTimes());
end;

// SET DIRECTORY path/wallpaper ORDERED RECURCIVE STYLE STRETCH FOR 5m WITH PROBABILITY 50 4 TIMES
procedure TSetDirectoryStatementParsingTest.ShouldParseDirectoryStatementWithAllFlagsAndAllProperties();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + ' ' + ORDERED_KEYWORD + ' ' + RECURSIVE_KEYWORD + STYLE_PROPERTY + DELAY_FOR_PROPERTY + WITH_PROBABILITY_PROPERTY + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_DIRECTORY_STATEMENT_ID = MainBranchStatements[0].GetId());
  DirectoryStatement := TWpcDirectoryStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetDirectory().GetPath().EndsWith(DIRECTORY_WITH_WALLPAPERS));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsRecurcive());
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.IsOrdered());
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, DirectoryStatement.GetStyle() = TEST_DEFAULT_STYLE_VALUE);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, DirectoryStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, DirectoryStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_TIMES_VALUE, DirectoryStatement.GetTimes());
end;

// SET DIRECTORY
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenNoDirectorySpecified();
begin
  ScriptLines.Add(SET_DIRECTORY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 2);
end;

// SET DIRECTORY path/wallpaper TODAY
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + ' TODAY');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 3);
end;

// SET DIRECTORY path/wallpaper TODAY ORDERED
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeFlags();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + ' TODAY ' + ORDERED_KEYWORD);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 3);
end;

// SET DIRECTORY path/wallpaper ORDERED TODAY RECURSIVE
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBetweenFlags();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + ' ' + ORDERED_KEYWORD + ' TODAY ' + RECURSIVE_KEYWORD);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 4);
end;

// SET DIRECTORY path/wallpaper ORDERED RECURSIVE TODAY
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterFlags();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + ' ' + ORDERED_KEYWORD + ' ' + RECURSIVE_KEYWORD + ' TODAY');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 5);
end;

// SET DIRECTORY path/wallpaper SMOOTHLY STYLE CENTER
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + ' SMOOTHLY ' + STYLE_PROPERTY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 3);
end;

// SET DIRECTORY path/wallpaper STYLE CENTER SMOOTHLY 4 TIMES
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBetweenProperties();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + STYLE_PROPERTY + ' SMOOTHLY ' + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 5);
end;

// SET DIRECTORY path/wallpaper STYLE CENTER FOR 5m ONCE
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS +  STYLE_PROPERTY + DELAY_FOR_PROPERTY + ' ONCE ');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 7);
end;

// SET DIRECTORY path/wallpaper STYLE CENTER RECURSIVE 4 TIMES
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenRecursiveFlagIsAddedBetweenProperties();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + STYLE_PROPERTY + ' ' + RECURSIVE_KEYWORD + ' ' + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 5);
end;

// SET DIRECTORY path/wallpaper STYLE CENTER ORDERED 4 TIMES
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenOrderedFlagIsAddedBetweenProperties();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + STYLE_PROPERTY + ' ' + ORDERED_KEYWORD + ' ' + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 5);
end;

// SET DIRECTORY path/wallpaper STYLE CENTER WITH PROBABILITY 50 RECURSIVE
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenRecursiveFlagIsAddedAtTheEndOfProperties();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + STYLE_PROPERTY + WITH_PROBABILITY_PROPERTY + RECURSIVE_KEYWORD);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 8);
end;

// SET DIRECTORY path/wallpaper STYLE CENTER WITH PROBABILITY 50 ORDERED
procedure TSetDirectoryStatementParsingTest.SholudRaiseScriptParseExceptionWhenOrderedFlagIsAddedAtTheEndOfProperties();
begin
  ScriptLines.Add(SET_DIRECTORY + DIRECTORY_WITH_WALLPAPERS + STYLE_PROPERTY + WITH_PROBABILITY_PROPERTY + RECURSIVE_KEYWORD);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 8);
end;


initialization
  RegisterTest(PARSER_TEST_SUITE_NAME, TSetDirectoryStatementParsingTest);


end.

