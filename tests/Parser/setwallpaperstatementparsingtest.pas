unit SetWallpaperStatementParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  WpcScriptCommons,
  WpcWallpaperStatement,
  WpcScriptParser,
  WpcWallpaperStyles;

const
  SET_WALLPAPER = SET_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ';
  WALLPAPER_IMAGE_FILE = 'File.ext';

type

  { TSetWallpaperStatementParsingTest }

  TSetWallpaperStatementParsingTest = class(TParserBaseTestCase)
  protected
    WallpaperStatement : TWpcWallpaperStatement;
  published
    procedure ShouldParseBaseWallpaperStatement();
    procedure ShouldParseWallpaperStatementWithStyleProperty();
    procedure ShouldParseWallpaperStatementWithProbabilityProperty();
    procedure ShouldParseWallpaperStatementWithDelayProperty();
    procedure ShouldParseWallpaperStatementWithTillProperty();
    procedure ShouldParseWallpaperStatementWithDelayPropertyUsingDelayVariable();
    procedure ShouldParseWallpaperStatementWithAllProperties();

    procedure SholudRaiseScriptParseExceptionWhenNoWallpaperImageSpecified();
    procedure SholudRaiseScriptParseExceptionIfBothDelayAndTillPropertiesSpecified();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
  end;

implementation

{ TSetWallpaperStatementParsingTest }

// SET WALLPAPER File.ext
procedure TSetWallpaperStatementParsingTest.ShouldParseBaseWallpaperStatement();
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_STATEMENT_ID = MainBranchStatements[0].GetId());
  WallpaperStatement := TWpcWallpaperStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.GetImage().GetPath().EndsWith(WALLPAPER_IMAGE_FILE));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, CENTERED = WallpaperStatement.GetStyle()); // CENTER should be default value
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, 0, WallpaperStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_PROBABILITY, WallpaperStatement.GetProbability());
end;

// SET WALLPAER File.ext STYLE STRETCH
procedure TSetWallpaperStatementParsingTest.ShouldParseWallpaperStatementWithStyleProperty();
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + STYLE_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_STATEMENT_ID = MainBranchStatements[0].GetId());
  WallpaperStatement := TWpcWallpaperStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.GetImage().GetPath().EndsWith(WALLPAPER_IMAGE_FILE));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.GetStyle() = TEST_DEFAULT_STYLE_VALUE);
end;

// SET WALLPAER File.ext WITH PROBABILITY 50
procedure TSetWallpaperStatementParsingTest.ShouldParseWallpaperStatementWithProbabilityProperty();
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_STATEMENT_ID = MainBranchStatements[0].GetId());
  WallpaperStatement := TWpcWallpaperStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.GetImage().GetPath().EndsWith(WALLPAPER_IMAGE_FILE));
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, WallpaperStatement.GetProbability());
end;

// SET WALLPAER File.ext FOR 5m
procedure TSetWallpaperStatementParsingTest.ShouldParseWallpaperStatementWithDelayProperty();
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + DELAY_FOR_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_STATEMENT_ID = MainBranchStatements[0].GetId());
  WallpaperStatement := TWpcWallpaperStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.GetImage().GetPath().EndsWith(WALLPAPER_IMAGE_FILE));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.IsDelayStatic());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WallpaperStatement.GetDelay());
end;

// SET WALLPAPER File.ext TILL 11:37
procedure TSetWallpaperStatementParsingTest.ShouldParseWallpaperStatementWithTillProperty();
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + TILL_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_STATEMENT_ID = MainBranchStatements[0].GetId());
  WallpaperStatement := TWpcWallpaperStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.GetImage().GetPath().EndsWith(WALLPAPER_IMAGE_FILE));
  AssertFalse(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.IsDelayStatic());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_TILL_VALUE, WallpaperStatement.GetOriginalDelayValue());
end;

// SET WALLPAER File.ext FOR $fiveMinutes
procedure TSetWallpaperStatementParsingTest.ShouldParseWallpaperStatementWithDelayPropertyUsingDelayVariable();
const
  DelayVariableName = 'fiveMinutes';

  Delays : Array[1..1] of VariableDefinition = (
    (DelayVariableName, TEST_DEFAULT_DELAY_STRING)
  );
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + ' ' + FOR_KEYWORD + ' ' + VARIABLE_START_SYMBOL + DelayVariableName);
  WrapInMainBranch(ScriptLines);
  AddVariables(ScriptLines, DELAYS_KEYWORD, Delays);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_STATEMENT_ID = MainBranchStatements[0].GetId());
  WallpaperStatement := TWpcWallpaperStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.GetImage().GetPath().EndsWith(WALLPAPER_IMAGE_FILE));
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WallpaperStatement.GetDelay());
end;

// SET WALLPAER File.ext STYLE STRETCH FOR 5m WITH PROBABILITY 50
procedure TSetWallpaperStatementParsingTest.ShouldParseWallpaperStatementWithAllProperties();
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + STYLE_PROPERTY + DELAY_FOR_PROPERTY + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_STATEMENT_ID = MainBranchStatements[0].GetId());
  WallpaperStatement := TWpcWallpaperStatement(MainBranchStatements[0]);
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.GetImage().GetPath().EndsWith(WALLPAPER_IMAGE_FILE));
  AssertTrue(WRONG_STATEMENT_PROPRTY_VALUE, WallpaperStatement.GetStyle() = TEST_DEFAULT_STYLE_VALUE);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WallpaperStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, WallpaperStatement.GetProbability());
end;

// SET WALLPAPER
procedure TSetWallpaperStatementParsingTest.SholudRaiseScriptParseExceptionWhenNoWallpaperImageSpecified();
begin
  ScriptLines.Add(SET_WALLPAPER);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 2);
end;

// SET WALLPAPER File.ext FOR 5m TILL 11:37
procedure TSetWallpaperStatementParsingTest.SholudRaiseScriptParseExceptionIfBothDelayAndTillPropertiesSpecified;
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + DELAY_FOR_PROPERTY + TILL_PROPERTY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1);
end;

// SET WALLPAER File.ext CENTER
procedure TSetWallpaperStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + ' CENTER ');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 3);
end;

// SET WALLPAER File.ext SMOOTHLY STYLE CENTER
procedure TSetWallpaperStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + ' SMOOTHLY ' + STYLE_PROPERTY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 3);
end;

// SET WALLPAER File.ext STYLE CENTER FOR 5m ONCE
procedure TSetWallpaperStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
begin
  ScriptLines.Add(SET_WALLPAPER + WALLPAPER_IMAGE_FILE + STYLE_PROPERTY + DELAY_FOR_PROPERTY + ' ONCE ');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 7);
end;


initialization
  RegisterTest(PARSER_TEST_SUITE_NAME, TSetWallpaperStatementParsingTest);


end.

