unit WaitStatementParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  WpcScriptCommons,
  WpcStatements,
  WpcScriptParser,
  WpcExceptions;

type

  { TWaitStatementParsingTestCase }

  TWaitStatementParsingTestCase = class(TParserBaseTestCase)
  protected
    WaitStatement : TWpcWaitStatement;
  published
    procedure ShouldParseWaitStatementWithDefaults();
    procedure ShouldParseWaitStatementWithDefaultTimeUnit();
    procedure ShouldParseBaseWaitStatement();
    procedure ShouldParseWaitStatementWithProbabilityProperty();
    procedure ShouldParseWaitStatementWithTimesProperty();
    procedure ShouldParseWaitStatementWithDelayProperty();
    procedure ShouldParseWaitStatementWithAllProperties();
    procedure ShouldParseWaitStatementWithAllPropertiesExceptDelayPropertyWhichIsSimplified();

    procedure SholudRaiseScriptParseExceptionWhenSetDelayPropertyAfterDelayValue();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
  end;


implementation

{ TWaitStatementParsingTestCase }

// WAIT
procedure TWaitStatementParsingTestCase.ShouldParseWaitStatementWithDefaults();
begin
  ScriptLines.Add(WAIT_KEYWORD);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
  WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WaitStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_PROBABILITY, WaitStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, WaitStatement.GetTimes(), DEFAULT_TIMES);
end;

// WAIT 5
procedure TWaitStatementParsingTestCase.ShouldParseWaitStatementWithDefaultTimeUnit();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' 5 '); // minutes should be default unit
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
  WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WaitStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_PROBABILITY, WaitStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_TIMES, WaitStatement.GetTimes());
end;

// WAIT 5m
procedure TWaitStatementParsingTestCase.ShouldParseBaseWaitStatement();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' ' + TEST_DEFAULT_DELAY_STRING);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
  WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WaitStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_PROBABILITY, WaitStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_TIMES, WaitStatement.GetTimes());
end;

// WAIT 5m WITH PROBABILITY 50
procedure TWaitStatementParsingTestCase.ShouldParseWaitStatementWithProbabilityProperty();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' ' + TEST_DEFAULT_DELAY_STRING + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
  WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WaitStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, WaitStatement.GetProbability());
end;

// WAIT 5m 4 TIMES
procedure TWaitStatementParsingTestCase.ShouldParseWaitStatementWithTimesProperty();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' ' + TEST_DEFAULT_DELAY_STRING + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
  WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WaitStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_TIMES_VALUE, WaitStatement.GetTimes());
end;

// WAIT FOR 5m
procedure TWaitStatementParsingTestCase.ShouldParseWaitStatementWithDelayProperty();
begin
  ScriptLines.Add(WAIT_KEYWORD + DELAY_FOR_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
  WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WaitStatement.GetDelay());
end;

// WAIT FOR 5m WITH PROBABILITY 50 4 TIMES
procedure TWaitStatementParsingTestCase.ShouldParseWaitStatementWithAllProperties();
begin
  ScriptLines.Add(WAIT_KEYWORD + DELAY_FOR_PROPERTY + WITH_PROBABILITY_PROPERTY + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
  WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WaitStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, WaitStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_TIMES_VALUE, WaitStatement.GetTimes());
end;

// WAIT 5m WITH PROBABILITY 50 4 TIMES
procedure TWaitStatementParsingTestCase.ShouldParseWaitStatementWithAllPropertiesExceptDelayPropertyWhichIsSimplified();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' ' + TEST_DEFAULT_DELAY_STRING + WITH_PROBABILITY_PROPERTY + X_TIMES_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
  WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_DELAY_VALUE, WaitStatement.GetDelay());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, WaitStatement.GetProbability());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_TIMES_VALUE, WaitStatement.GetTimes());
end;

// WAIT 5m FOR 5m
procedure TWaitStatementParsingTestCase.SholudRaiseScriptParseExceptionWhenSetDelayPropertyAfterDelayValue();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' ' + TEST_DEFAULT_DELAY_STRING + DELAY_FOR_PROPERTY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 2);
end;

// WAIT 5m QUICK
procedure TWaitStatementParsingTestCase.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' ' + TEST_DEFAULT_DELAY_STRING + ' QUICK ');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 2);
end;

// WAIT 5m TWICE WITH PROBABILITY 50
procedure TWaitStatementParsingTestCase.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' ' + TEST_DEFAULT_DELAY_STRING + ' TWICE ' + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 2);
end;

// WAIT 5m 5 TIMES WITH PROBABILITY 50 BEFORE TOMORROW
procedure TWaitStatementParsingTestCase.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' ' + TEST_DEFAULT_DELAY_STRING + X_TIMES_PROPERTY + WITH_PROBABILITY_PROPERTY + ' BEFORE TOMORROW ');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 7);
end;


initialization
  RegisterTest(PARSER_TEST_SUITE_NAME, TWaitStatementParsingTestCase);


end.

