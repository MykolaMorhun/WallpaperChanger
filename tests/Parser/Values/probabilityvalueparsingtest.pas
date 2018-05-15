unit ProbabilityValueParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  ValuesParsingBaseTest,
  WpcScriptCommons,
  WpcStopStatement,
  WpcScriptParser;

type

  { TProbabilityValueParsingTest }

  TProbabilityValueParsingTest = class(TValuesParsingBaseTest)
  private const
    ZERO_PROBABILITY = 0;
    MINIMAL_PROBABILITY = 1;
    MAXIMAL_PROBABILITY = 100;
  protected
    function ParseAndGetProbabilityValue(ProbabilityString : String) : Byte;
    procedure ParseAndEnsureScriptParseException(ProbabilityString : String);
  published
    procedure ShouldBeAbleToParseProbabilityValue();
    procedure ShouldParseZeroProbabilityValue();
    procedure ShouldParseMinimalProbabilityValue();
    procedure ShouldParseMaximalProbabilityValue();

    procedure ShouldRaiseScriptParseExceptionIfNoProbabilityValueGiven();
    procedure ShouldRaiseScriptParseExceptionIfNegativeProbabilityGiven();
    procedure ShouldRaiseScriptParseExceptionIfOverflowedProbabilityGiven();
    procedure ShouldRaiseScriptParseExceptionIfFractionProbabilityGiven();
    procedure ShouldRaiseScriptParseExceptionIfInvalidProbabilityGiven();
  end;


implementation

{ TProbabilityValueParsingTest }

function TProbabilityValueParsingTest.ParseAndGetProbabilityValue(ProbabilityString : String) : Byte;
var
  StopStatement : TWpcStopStatement;
begin
  ScriptLines.Add(STOP_KEYWORD + ' ' + WITH_KEYWORD + ' ' + PROBABILITY_KEYWORD + ' ' + ProbabilityString);
  WrapInMainBranch(ScriptLines);

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  AssertTrue(WRONG_STATEMENT, WPC_STOP_STATEMENT_ID = MainBranchStatements[0].GetId());
  StopStatement := TWpcStopStatement(MainBranchStatements[0]);

  Result := StopStatement.GetProbability();
end;

procedure TProbabilityValueParsingTest.ParseAndEnsureScriptParseException(ProbabilityString : String);
begin
    ScriptLines.Add(STOP_KEYWORD + ' ' + WITH_KEYWORD + ' ' + PROBABILITY_KEYWORD + ' ' + ProbabilityString);
    WrapInMainBranch(ScriptLines);

    AssertScriptParseExceptionOnParse(1);
end;

procedure TProbabilityValueParsingTest.ShouldBeAbleToParseProbabilityValue();
const
  A_PROBABILITY = 50;
begin
  AssertEquals(FAILED_TO_PARSE + IntToStr(A_PROBABILITY) + ' ' + PROBABILITY_KEYWORD,
               A_PROBABILITY,
               ParseAndGetProbabilityValue(IntToStr(A_PROBABILITY)));
end;

procedure TProbabilityValueParsingTest.ShouldParseZeroProbabilityValue();
begin
   AssertEquals(FAILED_TO_PARSE + IntToStr(ZERO_PROBABILITY) + ' ' + PROBABILITY_KEYWORD,
                ZERO_PROBABILITY,
                ParseAndGetProbabilityValue(IntToStr(ZERO_PROBABILITY)));
end;

procedure TProbabilityValueParsingTest.ShouldParseMinimalProbabilityValue();
begin
   AssertEquals(FAILED_TO_PARSE + IntToStr(MINIMAL_PROBABILITY) + ' ' + PROBABILITY_KEYWORD,
                MINIMAL_PROBABILITY,
                ParseAndGetProbabilityValue(IntToStr(MINIMAL_PROBABILITY)));
end;

procedure TProbabilityValueParsingTest.ShouldParseMaximalProbabilityValue();
begin
  AssertEquals(FAILED_TO_PARSE + IntToStr(MAXIMAL_PROBABILITY) + ' ' + PROBABILITY_KEYWORD,
               MAXIMAL_PROBABILITY,
               ParseAndGetProbabilityValue(IntToStr(MAXIMAL_PROBABILITY)));
end;

procedure TProbabilityValueParsingTest.ShouldRaiseScriptParseExceptionIfNoProbabilityValueGiven();
begin
  ParseAndEnsureScriptParseException('');
end;

procedure TProbabilityValueParsingTest.ShouldRaiseScriptParseExceptionIfNegativeProbabilityGiven();
begin
  ParseAndEnsureScriptParseException('-1');
end;

procedure TProbabilityValueParsingTest.ShouldRaiseScriptParseExceptionIfOverflowedProbabilityGiven();
begin
  ParseAndEnsureScriptParseException('101');
end;

procedure TProbabilityValueParsingTest.ShouldRaiseScriptParseExceptionIfFractionProbabilityGiven();
begin
  ParseAndEnsureScriptParseException('50.5');
end;

procedure TProbabilityValueParsingTest.ShouldRaiseScriptParseExceptionIfInvalidProbabilityGiven();
begin
  ParseAndEnsureScriptParseException('5O');
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TProbabilityValueParsingTest);


end.

