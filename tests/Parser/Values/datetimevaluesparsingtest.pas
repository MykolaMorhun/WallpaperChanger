unit DateTimeValuesParsingTest;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FpcUnit, TestRegistry,
   ValuesParsingBaseTest,
   SelectorValuesBaseTest,
   WpcScriptParser,
   DateTimeTestUtils;

type

  { TDateTimeValuesParsingTest }

  TDateTimeValuesParsingTest = class(TSelectorValuesBaseTest)
  public
    constructor Create(); override;
  private
    procedure InitTestData();
  published
    procedure ShouldParseAllSelectorValuesTogether();
  end;


implementation


{ TDateTimeValuesParsingTest }

constructor TDateTimeValuesParsingTest.Create();
begin
  SELECTOR_STRING := DATETIME_KEYWORD;

  A_SELECTOR_VALUE_STRING := '01.01-00:00:00';
  A_SELECTOR_VALUE := DateTimeStringToSecondOfYear(A_SELECTOR_VALUE_STRING);

  InitTestData();
end;

procedure TDateTimeValuesParsingTest.InitTestData();
const
  n = 40;
  TIMES_SELECTORS_STRINGS : Array[1..n] of String = (
    '01.01-00:01',
    '01.01-00:59', '01.01-13:00', '01.01-23:59',
    '30.06-00:59', '30.06-13:00', '30.06-23:59',
    '31.12-00:59', '31.12-13:00', '31.12-23:59',
    '14.07-13:08', '02.11-18:56',
    '2.10-11:11', '15.5-23:01', '2.4-17:15',
    '18.07-5:45', '06.08-20:5', '10.10-4:8',
    '01.01-00:00:01',
    '01.01-00:00:59', '01.01-13:00:00', '01.01-23:59:59',
    '30.06-00:00:59', '30.06-13:00:00', '30.06-23:59:59',
    '31.12-00:59:59', '31.12-13:00:00', '31.12-23:59:59',
    '12.12-18:46:17', '29.03-09:53:18',
    '8.11-15:14:19', '23.6-19:00:18', '4.4-12:04:16',
    '12.11-5:15:15', '02.05-11:8:19', '09.10-10:54:8', '17.01-6:18:4', '17.03-6:4:18', '17.05-18:6:4', '17.10-5:6:7'
  );
var
  i : Integer;

  TIMES_SELECTORS_VALUES : Array[1..n] of LongWord;
begin
  for i:=1 to n do
    TIMES_SELECTORS_VALUES[i] := DateTimeStringToSecondOfYear(TIMES_SELECTORS_STRINGS[i]);

  SetSelectorValuesTestData(
    TIMES_SELECTORS_STRINGS,
    TIMES_SELECTORS_VALUES
  );
  SetInvalidSelectorValuesStrings([
    'Test', 'June:15:06', 'Second.May-19:34', '01.01-four:fifthteen', '10', '10-10',
    '-14.12-10:30', '12.-10-12:45', '01.01-11:-11',
    '32.01-15:16', '21.13-12:15',
    '01.01-24:00', '01.01-24:01', '01.01-11:60', '01.01-11:61',
    '.01-10:48', '10.-10:12', '10.10-',
    '01.01-11', '01.01-11:', '01.01-:11', '01.01-:11:11', '01.01-11:11:',
    '01..01-10:23', '01.01-10::14', '01.01--12:15',
    '01.01.10:14', '01.01/10:14', '01.01,10:14', '01.01;10:14', '01.01_10:14', '01.01 10:14', '01.0110:14', '01011014',
    '-14.12-10:30:30', '12.-10-12:45:01', '01.01-11:-11:11', '01.01-11:11:-11',
    '32.01-15:16:19', '21.13-12:15:19',
    '01.01-24:00:00', '01.01-24:00:01', '01.01-11:00:60', '01.01-11:00:61',
    '.01-10:48:36', '10.-10:12:53',
    '01.01-11:11:11:', '01.01-:11:11:11', '01.01-:11:',
    '01..01-10:23:48', '01.01-10::14:19', '01.01-10:14::19', '01.01--12:15:58',
    '01.01.10:14:19', '01.01/10:14:19', '01.01,10:14:19', '01.01;10:14:19', '01.01_10:14:19', '01.01 10:14:19', '01.0110:14:19', '0101101419',
    '01.02', '10:15', '10:15:45'
  ]);
end;

procedure TDateTimeValuesParsingTest.ShouldParseAllSelectorValuesTogether();
const
  n = 1;
  m = 32;
  DATETIMES_SELECTORS_STRINGS : Array[1..n] of Array[1..m] of String = (
    ('01.02-01:02:03', '2.02-01:02:03', '03.2-01:02:03', '4.2-01:02:03',
     '01.05-1:02:03', '02.05-01:2:03', '03.05-01:02:3', '04.05-1:2:03', '05.05-1:02:3', '06.05-01:2:3', '07.05-1:2:3',
     '1.06-1:02:03', '2.06-01:2:03', '3.06-01:02:3', '4.06-1:2:03', '5.06-1:02:3', '6.06-01:2:3', '7.06-1:2:3',
     '01.7-1:02:03', '02.7-01:2:03', '03.7-01:02:3', '04.7-1:2:03', '05.7-1:02:3', '06.7-01:2:3', '07.7-1:2:3',
     '1.8-1:02:03', '2.8-01:2:03', '3.8-01:02:3', '4.8-1:2:03', '5.8-1:02:3', '6.8-01:2:3', '7.8-1:2:3')
  );
var
  i : Integer;
  j : Integer;

  DATETIMES_SELECTORS_VALUES : Array[1..n] of Array[1..m] of LongWord;
begin
  for i:=1 to n do
    for j:=1 to m do
      DATETIMES_SELECTORS_VALUES[i,j] := DateTimeStringToSecondOfYear(DATETIMES_SELECTORS_STRINGS[i,j]);

  for i:=1 to n do
    ParseAndCheckSelectorValues(DATETIMES_SELECTORS_STRINGS[i], DATETIMES_SELECTORS_VALUES[i]);
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TDateTimeValuesParsingTest);


end.
