unit WeightValueParsingTest;
 {$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FpcUnit, TestRegistry,
   ValuesParsingBaseTest,
   SelectorValuesBaseTest,
   WpcScriptParser;

type

  { TWeightValueParsingTest }

  TWeightValueParsingTest = class(TSelectorValuesBaseTest)
  public
    constructor Create(); override;
  private
    procedure InitTestData();
  published
    procedure ShouldParseAllSelectorValuesTogether();
  end;

implementation

{ TWeightValueParsingTest }

constructor TWeightValueParsingTest.Create();
begin
  SELECTOR_STRING := WEIGHT_KEYWORD;

  A_SELECTOR_VALUE_STRING := '1';
  A_SELECTOR_VALUE := 1;

  InitTestData();
end;

procedure TWeightValueParsingTest.InitTestData();
begin
  SetSelectorValuesTestData(
    ['1', '2', '10', '100', '1000', '10000', '100000', '1000000'],
    [1  , 2  , 10  , 100  , 1000  , 10000  , 100000  , 1000000  ]
  );
  SetInvalidSelectorValuesStrings(
    ['Test', '0', '-1', 'one', 'TWO']
  );
end;

procedure TWeightValueParsingTest.ShouldParseAllSelectorValuesTogether();
const
  n = 2;
  WEIGHT_SELECTORS_STRINGS : Array[1..n] of Array[1..8] of String = (
    ('1', '2', '10', '100', '1000', '10000', '100000', '1000000'),
    ('1', '2', '3', '4', '5', '6', '7', '8')
  );
  WEIGHT_SELECTOR_VALUES : Array[1..n] of Array[1..8] of LongWord = (
    (1, 2, 10, 100, 1000, 10000, 100000, 1000000),
    (1, 2, 3, 4, 5, 6, 7, 8)
  );
var
  i : Integer;
begin
  for i:=1 to n do
    ParseAndCheckSelectorValues(WEIGHT_SELECTORS_STRINGS[i], WEIGHT_SELECTOR_VALUES[i]);
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TWeightValueParsingTest);


end.

