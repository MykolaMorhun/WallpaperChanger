unit WeekdayValueParsingTest;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FpcUnit, TestRegistry,
   ValuesParsingBaseTest,
   SelectorValuesBaseTest,
   WpcScriptParser;

type

  { TWeekdayValueParsingTest }

  TWeekdayValueParsingTest = class(TSelectorValuesBaseTest)
  public
    constructor Create(); override;
  private
    procedure InitTestData();
  published
    procedure ShouldParseAllSelectorValuesTogether();
  end;

implementation

{ TWeekdayValueParsingTest }

constructor TWeekdayValueParsingTest.Create();
begin
  SELECTOR_STRING := WEEKDAY_KEYWORD;

  A_SELECTOR_VALUE_STRING := 'SUNDAY';
  A_SELECTOR_VALUE := 1;

  InitTestData();
end;

procedure TWeekdayValueParsingTest.InitTestData();
begin
  SetSelectorValuesTestData(
    ['2', '3', '4', '5', '6', '7'],
    [2  , 3  , 4  , 5  , 6  , 7]
  );
  SetSelectorValuesAliasesTestData(
    ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'],
    [2       , 3        , 4          , 5         , 6       , 7         ]
  );
  SetInvalidSelectorValuesStrings(
    ['Sunnyday', 'Test', '0', '-1', '8']
  );
end;

procedure TWeekdayValueParsingTest.ShouldParseAllSelectorValuesTogether();
const
  n = 4;
  WEEKDAYS_SELECTORS_STRINGS : Array[1..n] of Array[1..7] of String = (
    ('SUNDAY', 'MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY', 'FRIDAY', 'SATURDAY'),
    ('sunday', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday'),
    ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'),
    ('1', '2', '3', '4', '5', '6', '7')
  );
  WEEKDAYS_SELECTOR_VALUES : Array[1..7] of LongWord = (1, 2, 3, 4, 5, 6, 7);
var
  i : Integer;
begin
  for i:=1 to n do
    ParseAndCheckSelectorValues(WEEKDAYS_SELECTORS_STRINGS[i], WEEKDAYS_SELECTOR_VALUES);
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TWeekdayValueParsingTest);


end.

