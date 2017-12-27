unit SwitchBranchStatementParsingTest;

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

  { TSwitchBranchStatementParsingTest }

  TSwitchBranchStatementParsingTest = class(TParserBaseTestCase)
  public const
    SWITCH_TO_BRANCH = SWITCH_KEYWORD + ' ' + TO_KEYWORD + ' ' + BRANCH_KEYWORD + ' ';
    ADDITIONAL_BRANCH_NAME = 'SomeBranch';
  protected
    SwitchBranchStatement : TWpcSwitchBranchStatement;
  published
    procedure ShouldParseBaseSwitchBranchStatement();
    procedure ShouldParseSwitchBranchStatementWithProbabilityProperty();

    procedure SholudRaiseScriptParseExceptionWhenStatementKeywordsUncompleted();
    procedure SholudRaiseScriptParseExceptionWhenNoBranchSpecified();
    procedure SholudRaiseScriptParseExceptionWhenReferencedBranchDoesntExist();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
    procedure SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
  end;

implementation

{ TSwitchBranchStatementParsingTest }

// SWITCH TO BRANCH SomeBranch
procedure TSwitchBranchStatementParsingTest.ShouldParseBaseSwitchBranchStatement();
begin
  ScriptLines.Add(SWITCH_TO_BRANCH + ADDITIONAL_BRANCH_NAME);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_SWITCH_BRANCH_STATEMENT_ID = MainBranchStatements[0].GetId());
  SwitchBranchStatement := TWpcSwitchBranchStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, ADDITIONAL_BRANCH_NAME, SwitchBranchStatement.GetBranchName());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, DEFAULT_PROBABILITY, SwitchBranchStatement.GetProbability());
end;

// SWITCH TO BRANCH SomeBranch WITH PROBABILITY 50
procedure TSwitchBranchStatementParsingTest.ShouldParseSwitchBranchStatementWithProbabilityProperty();
begin
   ScriptLines.Add(SWITCH_TO_BRANCH + ADDITIONAL_BRANCH_NAME + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_SWITCH_BRANCH_STATEMENT_ID = MainBranchStatements[0].GetId());
  SwitchBranchStatement := TWpcSwitchBranchStatement(MainBranchStatements[0]);
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, ADDITIONAL_BRANCH_NAME, SwitchBranchStatement.GetBranchName());
  AssertEquals(WRONG_STATEMENT_PROPRTY_VALUE, TEST_DEFAULT_PROBABILITY_VALUE, SwitchBranchStatement.GetProbability());
end;

// SWITCH TO SomeBranch
procedure TSwitchBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenStatementKeywordsUncompleted;
begin
  ScriptLines.Add(SWITCH_KEYWORD + ' ' + TO_KEYWORD + ' ' + ADDITIONAL_BRANCH_NAME);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 2);
end;

// SWITCH TO RANCH
procedure TSwitchBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenNoBranchSpecified();
begin
  ScriptLines.Add(SWITCH_TO_BRANCH);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 3);
end;

// SWITCH TO BRANCH SomeNonexistentBranch
procedure TSwitchBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenReferencedBranchDoesntExist();
begin
  ScriptLines.Add(SWITCH_TO_BRANCH + ' SomeNonexistentBranch ');
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse();
end;

// SWITCH TO BRANCH SomeBranch ONCE
procedure TSwitchBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAtTheEndOfBase();
begin
  ScriptLines.Add(SWITCH_TO_BRANCH + ADDITIONAL_BRANCH_NAME + ' ONCE ');
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 4);
end;

// SWITCH TO BRANCH SomeBranch ONCE WITH PROBABILITY 50
procedure TSwitchBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedBeforeProperties();
begin
  ScriptLines.Add(SWITCH_TO_BRANCH + ADDITIONAL_BRANCH_NAME + ' ONCE ' + WITH_PROBABILITY_PROPERTY);
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 4);
end;

// SWITCH TO BRANCH SomeBranch WITH PROBABILITY 50 ONCE
procedure TSwitchBranchStatementParsingTest.SholudRaiseScriptParseExceptionWhenUnknownWordAddedAfterProperties();
begin
  ScriptLines.Add(SWITCH_TO_BRANCH + ADDITIONAL_BRANCH_NAME + WITH_PROBABILITY_PROPERTY + ' ONCE ');
  WrapInMainBranch(ScriptLines);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH_NAME);

  AssertScriptParseExceptionOnParse(1, 7);
end;


initialization
  RegisterTest(PARSER_TEST_SUITE_NAME, TSwitchBranchStatementParsingTest);


end.

