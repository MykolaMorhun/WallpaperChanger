unit MonthValueParsingTest;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FpcUnit, TestRegistry,
   ValuesParsingBaseTest,
   SelectorValuesBaseTest,
   WpcScriptParser;

type

  { TMonthValueParsingTest }

  TMonthValueParsingTest = class(TSelectorValuesBaseTest)
  public
    constructor Create(); override;
  private
    procedure InitTestData();
  published
    procedure ShouldParseAllSelectorValuesTogether();
  end;

implementation

{ TMonthValueParsingTest }

constructor TMonthValueParsingTest.Create();
begin
  SELECTOR_STRING := MONTH_KEYWORD;

  A_SELECTOR_VALUE_STRING := 'JANUARY';
  A_SELECTOR_VALUE := 1;

  InitTestData();
end;

procedure TMonthValueParsingTest.InitTestData();
begin
  SetSelectorValuesTestData(
    ['2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'],
    [2  , 3  , 4  , 5  , 6  , 7  , 8  , 9  , 10  , 11  , 12]
  );
  SetSelectorValuesAliasesTestData(
    ['February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'],
    [2         , 3      , 4      , 5    , 6     , 7     , 8       , 9          , 10       , 11        , 12    ]
  );
  SetInvalidSelectorValuesStrings(
    ['Maybe', 'Test', '0', '-1', '13']
  );
end;

procedure TMonthValueParsingTest.ShouldParseAllSelectorValuesTogether();
const
  n = 4;
  MONTHS_SELECTORS_STRINGS : Array[1..n] of Array[1..12] of String = (
    ('JANUARY', 'FEBRUARY', 'MARCH', 'APRIL', 'MAY', 'JUNE', 'JULY', 'AUGUST', 'SEPTEMBER', 'OCTOBER', 'NOVEMBER', 'DECEMBER'),
    ('january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december'),
    ('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'),
    ('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')
  );
  MONTHS_SELECTOR_VALUES : Array[1..12] of LongWord = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
var
  i : Integer;
begin
  for i:=1 to n do
    ParseAndCheckSelectorValues(MONTHS_SELECTORS_STRINGS[i], MONTHS_SELECTOR_VALUES);
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TMonthValueParsingTest);


end.

