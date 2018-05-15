unit UseBranchStatementParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  WpcScriptCommons,
  WpcBranchActionsStatements,
  WpcScriptParser;

type

  { TUseBranchStatementParsingTest }

  TUseBranchStatementParsingTest = class(TParserBaseTestCase)
  public const
    USE_BRANCH = USE_KEYWORD + ' ' + BRANCH_KEYWORD + ' ';
    ADDITIONAL_BRANCH_NAME = 'SomeBranch';
  protected
    UseBranchStatement : TWpcUseBranchStatement;
  published
    procedure ShouldParseBaseUseBranchStatement();
    procedure ShouldParseUsehBranchStatementWithProbabilityProperty();
    procedure ShouldParseUseBranchStatementWithTimesProperty();
    procedure ShouldParseUseBranchStatementWithAllProperties();

    procedure SholudRaiseScriptParseExceptionWhenStatementKeywordsUncompleted();
    procedure SholudRaiseScriptParseExceptionWhenNoBranchSpecified();
    procedure SholudRaiseScriptParseExceptionWhenReferencedBranchDoesntExist();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
  end;

implementation

{ TUseBranchStatementParsingTest }

// USE BRANCH SomeBranch
procedure TUseBranchStatementParsingTest.ShouldParseBaseUseBranchStatement();
begin
  ScriptLines.Add(USE_BRANCH + ADDITIONAL_BRANCH_NAME);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_USE_BRANCH_STATEMENT_ID = MainBranchStatements[0].GetId());
  UseBranchStatement := TWpcUseBranchStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, ADDITIONAL_BRANCH_NAME, UseBranchStatement.GetBranchName());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_PROBABILITY, UseBranchStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_TIMES, UseBranchStatement.GetTimes());
end;

// USE BRANCH SomeBranch WITH PROBABILY 50
procedure TUseBranchStatementParsingTest.ShouldParseUsehBranchStatementWithProbabilityProperty();
begin
  ScriptLines.Add(USE_BRANCH + ADDITIONAL_BRANCH_NAME + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_USE_BRANCH_STATEMENT_ID = MainBranchStatements[0].GetId());
  UseBranchStatement := TWpcUseBranchStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, ADDITIONAL_BRANCH_NAME, UseBranchStatement.GetBranchName());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, UseBranchStatement.GetProbability());
end;

// USE BRANCH SomeBranch 4 TIMES
procedure TUseBranchStatementParsingTest.ShouldParseUseBranchStatementWithTimesProperty();
begin
  ScriptLines.Add(USE_BRANCH + ADDITIONAL_BRANCH_NAME + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_USE_BRANCH_STATEMENT_ID = MainBranchStatements[0].GetId());
  UseBranchStatement := TWpcUseBranchStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, ADDITIONAL_BRANCH_NAME, UseBranchStatement.GetBranchName());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_TIMES_VALUE, UseBranchStatement.GetTimes());
end;

// USE BRANCH SomeBranch 4 TIMES WITH PROBABILY 50
procedure TUseBranchStatementParsingTest.ShouldParseUseBranchStatementWithAllProperties();
begin
  ScriptLines.Add(USE_BRANCH + ADDITIONAL_BRANCH_NAME + X_TIMES_PROPERTY + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_USE_BRANCH_STATEMENT_ID = MainBranchStatements[0].GetId());
  UseBranchStatement := TWpcUseBranchStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, ADDITIONAL_BRANCH_NAME, UseBranchStatement.GetBranchName());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, UseBranchStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_TIMES_VALUE, UseBranchStatement.GetTimes());
end;

// USE SomeBranch
procedure TUseBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenStatementKeywordsUncompleted();
begin
  ScriptLines.Add(USE_KEYWORD + ' ' + ADDITIONAL_BRANCH_NAME);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 1);
end;

// USE BRANCH
procedure TUseBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenNoBranchSpecified();
begin
  ScriptLines.Add(USE_BRANCH);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 2);
end;

// USE BRANCH SomeNonexistentBranch
procedure TUseBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenReferencedBranchDoesntExist();
begin
  ScriptLines.Add(USE_BRANCH + ' SomeNonexistentBranch ');
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse();
end;

// USE BRANCH SomeBranch ONCE
procedure TUseBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
begin
  ScriptLines.Add(USE_BRANCH + ADDITIONAL_BRANCH_NAME + ' ONCE ');
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 3);
end;

// USE BRANCH SomeBranch ONCE WITH PROBABILITY 50
procedure TUseBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
begin
  ScriptLines.Add(USE_BRANCH + ADDITIONAL_BRANCH_NAME + ' ONCE ' + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 3);
end;

// USE BRANCH SomeBranch 4 TIMES ALWAYS
procedure TUseBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
begin
  ScriptLines.Add(USE_BRANCH + ADDITIONAL_BRANCH_NAME + X_TIMES_PROPERTY + ' ALWAYS ');
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 5);
end;


initialization
  RegisterTest(PARSER_TEST_SUITE_NAME, TUseBranchStatementParsingTest);


end.

