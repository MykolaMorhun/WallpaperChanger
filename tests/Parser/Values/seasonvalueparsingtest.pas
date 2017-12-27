unit SeasonValueParsingTest;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, FpcUnit, TestRegistry,
   ValuesParsingBaseTest,
   SelectorValuesBaseTest,
   WpcScriptParser;

type

  { TSeasonValueParsingTest }

  TSeasonValueParsingTest = class(TSelectorValuesBaseTest)
  public
    constructor Create(); override;
    destructor Destroy(); override;
  private
    procedure InitTestData();
  published
    procedure ShouldParseAllSelectorValuesTogether();
  end;

implementation

{ TSeasonValueParsingTest }

constructor TSeasonValueParsingTest.Create();
begin
  SELECTOR_STRING := SEASON_KEYWORD;

  A_SELECTOR_VALUE_STRING := 'WINTER';
  A_SELECTOR_VALUE := 1;

  InitTestData();
end;

destructor TSeasonValueParsingTest.Destroy();
begin
  inherited Destroy();
end;

procedure TSeasonValueParsingTest.InitTestData();
begin
  SetSelectorValuesTestData(
    ['2', '3', '4'],
    [2  , 3  , 4  ]
  );
  SetSelectorValuesAliasesTestData(
    ['Spring', 'Summer', 'Autumn'],
    [2       , 3       , 4       ]
  );
  SetInvalidSelectorValuesStrings(
    ['Sprong', 'Test', '5', '0', '-1']
  );
end;

procedure TSeasonValueParsingTest.ShouldParseAllSelectorValuesTogether();
const
  n = 4;
  SEASONS_SELECTORS_STRINGS : Array[1..n] of Array[1..4] of String = (
    ('WINTER', 'SPRING', 'SUMMER', 'AUTUMN'),
    ('winter', 'spring', 'summer', 'autumn'),
    ('Winter', 'Spring', 'Summer', 'Autumn'),
    ('1', '2', '3', '4')
  );
  SEASONS_SELECTOR_VALUES : Array[1..4] of LongWord = (1, 2, 3, 4);
var
  i : Integer;
begin
  for i:=1 to n do
    ParseAndCheckSelectorValues(SEASONS_SELECTORS_STRINGS[i], SEASONS_SELECTOR_VALUES);
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TSeasonValueParsingTest);


end.

