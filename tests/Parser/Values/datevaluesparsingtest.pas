unit DateValuesParsingTest;
{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FpcUnit, TestRegistry,
   ValuesParsingBaseTest,
   SelectorValuesBaseTest,
   WpcScriptParser,
   DateTimeTestUtils;

type

  { TDateValuesParsingTest }

  TDateValuesParsingTest = class(TSelectorValuesBaseTest)
  public
    constructor Create(); override;
  private
    procedure InitTestData();
  published
    procedure ShouldParseAllSelectorValuesTogether();
  end;


implementation


{ TDateValuesParsingTest }

constructor TDateValuesParsingTest.Create();
begin
  SELECTOR_STRING := DATE_KEYWORD;

  A_SELECTOR_VALUE_STRING := '01.01';
  A_SELECTOR_VALUE := DateStringToDayOfYear(A_SELECTOR_VALUE_STRING);

  InitTestData();
end;

procedure TDateValuesParsingTest.InitTestData();
const
  n = 22;
  DATES_SELECTORS_STRINGS : Array[1..n] of String = (
    '02.01', '02.02',
    '31.01', '28.02', '31.03', '30.04', '31.05', '30.06', '31.07', '31.08', '30.09', '31.10', '30.11', '31.12',
    '2.2', '1.2', '2.1', '3.3', '9.9', '9.10', '10.9', '1.12'
  );
var
  i : Integer;

  DATES_SELECTORS_VALUES : Array[1..n] of LongWord;
begin
  for i:=1 to n do
    DATES_SELECTORS_VALUES[i] := DateStringToDayOfYear(DATES_SELECTORS_STRINGS[i]);

  SetSelectorValuesTestData(
    DATES_SELECTORS_STRINGS,
    DATES_SELECTORS_VALUES
  );
  SetInvalidSelectorValuesStrings([
    'Test', '-02.02', '.02', '02.', '.2', '2.',
    '00.00', '02.00', '00.02', '30.02', '32.05', '31.06', '13.13',
    '0.02', '02.0', '0.0', '0.12', '12.0',
    'first.May', '01.May', 'fifth.06', 'tenth of october',
    '02:02', '02,02', '02-02', '02_02', '02 02', '0202',
    '02.02.12', '02.02.2017', '02.02.', '.02.02', '02..02',
    '02.02-04:04:04'
  ]);
end;

procedure TDateValuesParsingTest.ShouldParseAllSelectorValuesTogether();
const
  n = 4;
  m = 12;
  DATES_SELECTORS_STRINGS : Array[1..n] of Array[1..m] of String = (
    ('01.01', '01.02', '01.03', '01.04', '01.05', '01.06', '01.07', '01.08', '01.09', '01.10', '01.11', '01.12'),
    ('14.01', '14.02', '14.03', '14.04', '14.05', '14.06', '14.07', '14.08', '14.09', '14.10', '14.11', '14.12'),
    ('31.01', '28.02', '31.03', '30.04', '31.05', '30.06', '31.07', '31.08', '30.09', '31.10', '30.11', '31.12'),
    ('01.01', '02.02', '03.03', '04.04', '05.05', '06.06', '07.07', '08.08', '09.09', '10.10', '11.11', '12.12')
  );
var
  i : Integer;
  j : Integer;

  DATES_SELECTORS_VALUES : Array[1..n] of Array[1..m] of LongWord;
begin
  for i:=1 to n do
    for j:=1 to m do
      DATES_SELECTORS_VALUES[i,j] := DateStringToDayOfYear(DATES_SELECTORS_STRINGS[i,j]);

  for i:=1 to n do
    ParseAndCheckSelectorValues(DATES_SELECTORS_STRINGS[i], DATES_SELECTORS_VALUES[i]);
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TDateValuesParsingTest);


end.

