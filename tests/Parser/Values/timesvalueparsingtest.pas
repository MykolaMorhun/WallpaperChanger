unit TimesValueParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  ValuesParsingBaseTest,
  WpcScriptCommons,
  WpcStatements,
  WpcScriptParser;

type

  { TTimesValueParsingTest }

  TTimesValueParsingTest = class(TValuesParsingBaseTest)
  protected
    function ParseAndGetTimesValue(TimesString : String) : LongWord;
    procedure ParseAndEnsureScriptParseException(TimesString : String);
  published
    procedure ShouldParseTimesValue();

    procedure ShouldRaiseScriptParseExceptionIfNoTimesValueGiven();
    procedure ShouldRaiseScriptParseExceptionIfInvalidTimesValueGiven();
  end;


implementation

{ TTimesValueParsingTest }

function TTimesValueParsingTest.ParseAndGetTimesValue(TimesString : String): LongWord;
var
  WaitStatement : TWpcWaitStatement;
begin
  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  ScriptLines := TStringList.Create();
  try
    ScriptLines.Add(WAIT_KEYWORD + ' 5s ' + TimesString + ' ' + TIMES_KEYWORD);
    WrapInMainBranch(ScriptLines);

    ParseScriptLines();
    ReadMainBranchStatementsList();

    AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

    AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
    WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);

    Result := WaitStatement.GetTimes();
  finally
    FreeAndNil(ScriptLines);
    FreeAndNil(Script);
  end;
end;

procedure TTimesValueParsingTest.ParseAndEnsureScriptParseException(TimesString : String);
begin
  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  ScriptLines := TStringList.Create();
  try
    ScriptLines.Add(WAIT_KEYWORD + ' 5s ' + TimesString + ' ' + TIMES_KEYWORD);
    WrapInMainBranch(ScriptLines);

    AssertScriptParseExceptionOnParse(1);
  finally
    FreeAndNil(ScriptLines);
  end;
end;

procedure TTimesValueParsingTest.ShouldParseTimesValue();
const
  VALID_TIMES_VALUES : Array[1..4] of LongWord = (1, 5, 100, 12345);
var
  i : Integer;
begin
  for i:=1 to Length(VALID_TIMES_VALUES) do begin
    AssertEquals(FAILED_TO_PARSE + IntToStr(VALID_TIMES_VALUES[i]) + ' ' + TIMES_KEYWORD,
                 VALID_TIMES_VALUES[i],
                 ParseAndGetTimesValue(IntToStr(VALID_TIMES_VALUES[i])));
  end;
end;

procedure TTimesValueParsingTest.ShouldRaiseScriptParseExceptionIfNoTimesValueGiven();
begin
  ParseAndEnsureScriptParseException('');
end;

procedure TTimesValueParsingTest.ShouldRaiseScriptParseExceptionIfInvalidTimesValueGiven;
const
  INVALID_TIMES_VALUES : Array[1..6] of String = ('0', '-1', '5.5', '5t', 'x', 'v5');
var
  i : Integer;
begin
  for i:=1 to Length(INVALID_TIMES_VALUES) do begin
    ParseAndEnsureScriptParseException(INVALID_TIMES_VALUES[i]);
  end;
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TTimesValueParsingTest);


end.

