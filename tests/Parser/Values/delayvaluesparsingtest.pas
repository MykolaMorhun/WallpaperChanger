unit DelayValuesParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  ValuesParsingBaseTest,
  WpcScriptCommons,
  WpcWaitStatement,
  WpcTimeMeasurementUnits,
  WpcTimeUtils,
  WpcScriptParser;

type

  { TDelayValuesParsingTest }

  TDelayValuesParsingTest = class(TValuesParsingBaseTest)
  private const
    DELAY_MEASUREMENT_UNITS_STRINGS : Array[1..5] of String = ('s', 'm', 'h', 'd', 'ms');
    DELAY_MEASUREMENT_UNITS : Array[1..5] of TWpcTimeMeasurementUnits = (SECONDS, MINUTES, HOURS, DAYS, MILLISECONDS);
    ZERO_DELAY = 0;
    MINIMAL_DELAY = 1;
    REGULAR_DELAY = 4;
  protected
    function ParseAndGetWaitValue(DelayString : String) : LongWord;
    procedure ParseAndEnsureScriptParseException(DelayString : String);
  published
    procedure ShouldParseDelayValueWithDefaultTimeunit();

    procedure ShoudParseZeroDelayValueWithEachMeasurementUnit();
    procedure ShoudParseMinimalDelayValueWithEachMeasurementUnit();
    procedure ShoudParseRegularDelayValueWithEachMeasurementUnit();
    procedure ShoudParseBigDelayValueWithEachMeasurementUnit();

    procedure ShouldRaiseScriptParseExceptionIfNoDelayValueGiven();
    procedure ShouldRaiseScriptParseExceptionIfInvalidMeasurementUnitGiven();
    procedure ShouldRaiseScriptParseExceptionIfInvalidDelayValueGiven();
    procedure ShouldRaiseScriptParseExceptionIfNegativeValueGivenForEachMeasurementUnit();
    procedure ShouldRaiseScriptParseExceptionIfFractionValueGivenForEachMeasurementUnit();
    procedure ShouldRaiseScriptParseExceptionIfMaximumValueExceededForEachMeasurementUnit();
  end;


implementation

{ TDelayValuesParsingTest }

// WAIT <DelayString>
function TDelayValuesParsingTest.ParseAndGetWaitValue(DelayString : String) : LongWord;
var
  WaitStatement : TWpcWaitStatement;
begin
  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  ScriptLines := TStringList.Create();
  try
    ScriptLines.Add(WAIT_KEYWORD + ' ' + DelayString);
    WrapInMainBranch(ScriptLines);

    ParseScriptLines();
    ReadMainBranchStatementsList();

    AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

    AssertTrue(WRONG_STATEMENT, WPC_WAIT_STATEMENT_ID = MainBranchStatements[0].GetId());
    WaitStatement := TWpcWaitStatement(MainBranchStatements[0]);

    Result := WaitStatement.GetDelay();
  finally
    FreeAndNil(ScriptLines);
    FreeAndNil(Script);
  end;
end;

// WAIT <DelayString>
procedure TDelayValuesParsingTest.ParseAndEnsureScriptParseException(DelayString : String);
begin
  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  ScriptLines := TStringList.Create();
  try
    ScriptLines.Add(WAIT_KEYWORD + ' ' + DelayString);
    WrapInMainBranch(ScriptLines);

    // TODO improve parser to give information about word number
    AssertScriptParseExceptionOnParse(1);
  finally
    FreeAndNil(ScriptLines);
  end;
end;

procedure TDelayValuesParsingTest.ShouldParseDelayValueWithDefaultTimeunit();
begin
  AssertEquals(FAILED_TO_PARSE + IntToStr(REGULAR_DELAY) + ' with default measurement units.',
               ConvertToMilliseconds(REGULAR_DELAY, MINUTES),
               ParseAndGetWaitValue(IntToStr(REGULAR_DELAY))); // minutes is default units
end;

procedure TDelayValuesParsingTest.ShoudParseZeroDelayValueWithEachMeasurementUnit();
var
  i : Integer;
begin
  for i:=1 to Length(DELAY_MEASUREMENT_UNITS_STRINGS) do begin
     AssertEquals(FAILED_TO_PARSE + IntToStr(ZERO_DELAY) + DELAY_MEASUREMENT_UNITS_STRINGS[i],
                  ZERO_DELAY,
                  ParseAndGetWaitValue(IntToStr(ZERO_DELAY) + DELAY_MEASUREMENT_UNITS_STRINGS[i]));
  end;
end;

procedure TDelayValuesParsingTest.ShoudParseMinimalDelayValueWithEachMeasurementUnit();
var
  i : Integer;
begin
  for i:=1 to Length(DELAY_MEASUREMENT_UNITS_STRINGS) do begin
     AssertEquals(FAILED_TO_PARSE + IntToStr(MINIMAL_DELAY) + DELAY_MEASUREMENT_UNITS_STRINGS[i],
                  ConvertToMilliseconds(MINIMAL_DELAY, DELAY_MEASUREMENT_UNITS[i]),
                  ParseAndGetWaitValue(IntToStr(MINIMAL_DELAY) + DELAY_MEASUREMENT_UNITS_STRINGS[i]));
  end;
end;

procedure TDelayValuesParsingTest.ShoudParseRegularDelayValueWithEachMeasurementUnit();
var
  i : Integer;
begin
  for i:=1 to Length(DELAY_MEASUREMENT_UNITS_STRINGS) do begin
     AssertEquals(FAILED_TO_PARSE + IntToStr(REGULAR_DELAY) + DELAY_MEASUREMENT_UNITS_STRINGS[i],
                  ConvertToMilliseconds(REGULAR_DELAY, DELAY_MEASUREMENT_UNITS[i]),
                  ParseAndGetWaitValue(IntToStr(REGULAR_DELAY) + DELAY_MEASUREMENT_UNITS_STRINGS[i]));
  end;
end;

procedure TDelayValuesParsingTest.ShoudParseBigDelayValueWithEachMeasurementUnit();
const
  BIG_DELAY_VALUES : Array[1..5] of LongWord = (2592000, 43200, 720, 30, 2592000000);
var
  i : Integer;
begin
  for i:=1 to Length(DELAY_MEASUREMENT_UNITS_STRINGS) do begin
     AssertEquals(FAILED_TO_PARSE + IntToStr(BIG_DELAY_VALUES[i]) + DELAY_MEASUREMENT_UNITS_STRINGS[i],
                  ConvertToMilliseconds(BIG_DELAY_VALUES[i], DELAY_MEASUREMENT_UNITS[i]),
                  ParseAndGetWaitValue(IntToStr(BIG_DELAY_VALUES[i]) + DELAY_MEASUREMENT_UNITS_STRINGS[i]));
  end;
end;

procedure TDelayValuesParsingTest.ShouldRaiseScriptParseExceptionIfNoDelayValueGiven();
begin
  ScriptLines.Add(WAIT_KEYWORD + ' ' + FOR_KEYWORD + ' ');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 2);
end;

procedure TDelayValuesParsingTest.ShouldRaiseScriptParseExceptionIfInvalidMeasurementUnitGiven();
const
  WRONG_MEASUREMENT_UNITS : Array[1..5] of String = ('us', 'ps', 'Mm', 'ns', 'ds');
var
  i : Integer;
begin
  for i:=1 to Length(WRONG_MEASUREMENT_UNITS) do begin
    ParseAndEnsureScriptParseException(IntToStr(REGULAR_DELAY) + WRONG_MEASUREMENT_UNITS[i]);
  end;
end;

procedure TDelayValuesParsingTest.ShouldRaiseScriptParseExceptionIfInvalidDelayValueGiven();
const
  WRONG_DELAY_VALUES : Array[1..5] of String = ('x', 's5', '-', '0x5', '10b');
var
  i : Integer;
begin
  for i:=1 to Length(WRONG_DELAY_VALUES) do begin
    ParseAndEnsureScriptParseException(WRONG_DELAY_VALUES[i] + DELAY_MEASUREMENT_UNITS_STRINGS[2]);
  end;
end;

procedure TDelayValuesParsingTest.ShouldRaiseScriptParseExceptionIfNegativeValueGivenForEachMeasurementUnit();
const
  NEGATIVE_DELAY = -2;
var
  i : Integer;
begin
  for i:=1 to Length(DELAY_MEASUREMENT_UNITS_STRINGS) do begin
    ParseAndEnsureScriptParseException(IntToStr(NEGATIVE_DELAY) + DELAY_MEASUREMENT_UNITS_STRINGS[i]);
  end;
end;

procedure TDelayValuesParsingTest.ShouldRaiseScriptParseExceptionIfFractionValueGivenForEachMeasurementUnit();
const
  FRACTION_DELAY_STRING = '2.5';
var
  i : Integer;
begin
  for i:=1 to Length(DELAY_MEASUREMENT_UNITS_STRINGS) do begin
    ParseAndEnsureScriptParseException(FRACTION_DELAY_STRING + DELAY_MEASUREMENT_UNITS_STRINGS[i]);
  end;
end;

procedure TDelayValuesParsingTest.ShouldRaiseScriptParseExceptionIfMaximumValueExceededForEachMeasurementUnit();
const
  EXCEEDED_DELAY_VALUES : Array[1..5] of String = ('2764801', '46081', '769', '33', '2764800001');
var
  i : Integer;
begin
  for i:=1 to Length(DELAY_MEASUREMENT_UNITS_STRINGS) do begin
    ParseAndEnsureScriptParseException(EXCEEDED_DELAY_VALUES[i] + DELAY_MEASUREMENT_UNITS_STRINGS[i]);
  end;
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TDelayValuesParsingTest);


end.

