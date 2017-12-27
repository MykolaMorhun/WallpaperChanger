unit TimeValuesParsingTest;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FpcUnit, TestRegistry,
   ValuesParsingBaseTest,
   SelectorValuesBaseTest,
   WpcScriptParser,
   DateTimeTestUtils;

type

  { TTimeValuesParsingTest }

  TTimeValuesParsingTest = class(TSelectorValuesBaseTest)
  public
    constructor Create(); override;
  private
    procedure InitTestData();
  published
    procedure ShouldParseAllSelectorValuesTogether();
  end;


implementation


{ TTimeValuesParsingTest }

constructor TTimeValuesParsingTest.Create();
begin
  SELECTOR_STRING := TIME_KEYWORD;

  A_SELECTOR_VALUE_STRING := '00:00';
  A_SELECTOR_VALUE := TimeStringToSecondOfDay(A_SELECTOR_VALUE_STRING);

  InitTestData();
end;

procedure TTimeValuesParsingTest.InitTestData();
const
  n = 31;
  TIMES_SELECTORS_STRINGS : Array[1..n] of String = (
    '00:01', '01:00',
    '00:59', '13:00', '13:59', '23:00', '23:59',
    '08:37', '22:18',
    '00:00:01', '00:01:00', '01:00:00',
    '00:00:59', '00:59:59', '13:00:59', '13:59:59', '23:00:59', '23:59:59',
    '01:01:01', '10:10:10', '12:12:12', '09:13:47', '21:04:54',
    '0:1', '1:0', '0:59', '13:0',
    '0:0:1', '0:1:0', '1:0:0','0:0:59'
  );
var
  i : Integer;

  TIMES_SELECTORS_VALUES : Array[1..n] of LongWord;
begin
  for i:=1 to n do
    TIMES_SELECTORS_VALUES[i] := TimeStringToSecondOfDay(TIMES_SELECTORS_STRINGS[i]);

  SetSelectorValuesTestData(
    TIMES_SELECTORS_STRINGS,
    TIMES_SELECTORS_VALUES
  );
  SetInvalidSelectorValuesStrings([
    'Test', 'two:four',
    '-01:01', '01:-01', '01:01:-01',
    '01::01', ':01:01', '01:10:', ':01:01:01', '01:01:01:', '01:01::01', '01::01:01',
    '24:00', '24:01', '25:01', '01:60', '01:61', '01:01:60', '01:01:61',
    '04.04', '04,04', '04-04', '04_04', '04 04', '0404',
    '04.04.04', '04,04,04', '04-04-04', '04_04_04', '04 04 04', '040404',
    '04:04.04', '04.04:04', '04:04,04', '04,04:04', '04:04-04', '04-04:04', '04:04_04', '04_04:04', '04:04 04', '04 04:04', '04:0404', '0404:04',
    '02.02-04:04:04'
  ]);
end;

procedure TTimeValuesParsingTest.ShouldParseAllSelectorValuesTogether();
const
  n = 5;
  m = 12;
  TIMES_SELECTORS_STRINGS : Array[1..n] of Array[1..m] of String = (
    ('01:01', '03:03', '05:05', '07:07', '09:09', '11:11', '13:13', '15:15', '17:17', '19:19', '21:21', '23:23'),
    ('06:00', '07:00', '08:00', '09:00', '10:00', '11:00', '12:00', '13:00', '14:00', '15:00', '16:00', '17:00'),
    ('17:15', '17:16', '17:17', '17:18', '17:19', '17:20', '17:21', '17:22', '17:23', '17:24', '17:25', '17:26'),
    ('09:45:08', '09:45:09', '09:45:10', '09:45:11', '09:45:12', '09:45:13', '09:45:14', '09:45:15', '09:45:16', '09:45:17', '09:45:18', '09:45:19'),
    ('00:15', '2:16', '5:5:5', '6:08:9', '10:11:12', '12:3', '14:00:5', '14:16', '17:23', '17:23:1', '17:23:02', '17:23:23')
  );
var
  i : Integer;
  j : Integer;

  TIMES_SELECTORS_VALUES : Array[1..n] of Array[1..m] of LongWord;
begin
  for i:=1 to n do
    for j:=1 to m do
      TIMES_SELECTORS_VALUES[i,j] := TimeStringToSecondOfDay(TIMES_SELECTORS_STRINGS[i,j]);

  for i:=1 to n do
    ParseAndCheckSelectorValues(TIMES_SELECTORS_STRINGS[i], TIMES_SELECTORS_VALUES[i]);
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TTimeValuesParsingTest);


end.

