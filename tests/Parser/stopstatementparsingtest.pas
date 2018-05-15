unit StopStatementParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  WpcScriptCommons,
  WpcStopStatement,
  WpcScriptParser;

type

  { TStopStatementParsingTestCase }

  TStopStatementParsingTestCase = class(TParserBaseTestCase)
  protected
    StopStatement : TWpcStopStatement;
  published
    procedure ShouldParseBaseStopStatement();
    procedure ShouldParseStopStatementWithProbabilityProperty();

    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
  end;

implementation

{ TStopStatementParsingTestCase }

// STOP
procedure TStopStatementParsingTestCase.ShouldParseBaseStopStatement();
begin
  ScriptLines.Add(STOP_KEYWORD);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_STOP_STATEMENT_ID = MainBranchStatements[0].GetId());
  StopStatement := TWpcStopStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_PROBABILITY, StopStatement.GetProbability());
end;

// STOP WITH PROBABILITY 50
procedure TStopStatementParsingTestCase.ShouldParseStopStatementWithProbabilityProperty();
begin
  ScriptLines.Add(STOP_KEYWORD + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_STOP_STATEMENT_ID = MainBranchStatements[0].GetId());
  StopStatement := TWpcStopStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, StopStatement.GetProbability());
end;

// STOP AT
procedure TStopStatementParsingTestCase.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
begin
  ScriptLines.Add(STOP_KEYWORD + ' AT');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 1);
end;

// STOP ALWAYS WITH PROBABILITY 50
procedure TStopStatementParsingTestCase.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
begin
  ScriptLines.Add(STOP_KEYWORD + ' ALWAYS ' + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 1);
end;

// STOP WITH PROBABILITY 50 ANYTIME
procedure TStopStatementParsingTestCase.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties;
begin
  ScriptLines.Add(STOP_KEYWORD + WITH_PROBABILITY_PROPERTY + ' ANYTIME ');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 4);
end;


initialization
  RegisterTest(PARSER_TEST_SUITE_NAME, TStopStatementParsingTestCase);

end.

