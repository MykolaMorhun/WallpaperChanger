unit WallpaperChooserStatementParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  ChooserStatementBaseTestCase,
  WpcChooserStatements,
  WpcScriptParser;

type

  { TWallpaperChooserStatementParsingTest }

  TWallpaperChooserStatementParsingTest = class(TAbstractChooserStatementParsingTest)
  public const
    TEST_FILE1 = 'File1.ext';
    TEST_FILE2 = 'File2.ext';
  protected
    procedure SetUp(); override;
  protected
    procedure AddRequiredObjectsIntoScript(); override;
  published
    procedure ShouldParseItemWithDefaultWeightWhichConsistsFromOneWord();
  end;

implementation

{ TWallpaperChooserStatementParsingTest }

procedure TWallpaperChooserStatementParsingTest.SetUp();
begin
  inherited SetUp();

  ChooserType := ' ' + WALLPAPER_KEYWORD + ' ';
  ChooserItem1 := '  ' + TEST_FILE1 + STYLE_PROPERTY;
  ChooserItem2 := '  ' + TEST_FILE2 + ' ' + STYLE_KEYWORD + ' TILE ';
end;

procedure TWallpaperChooserStatementParsingTest.AddRequiredObjectsIntoScript();
begin
  // Does nothing
end;

// CHOOSE WALLAPER FROM
//   Item1
//   Item2
// END CHOOSE
procedure TWallpaperChooserStatementParsingTest.ShouldParseItemWithDefaultWeightWhichConsistsFromOneWord();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + FROM_KEYWORD);
  ScriptLines.Add('  ' + TEST_FILE1);
  ScriptLines.Add('  ' + TEST_FILE2);
  ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
  WrapInMainBranch(ScriptLines);
  AddRequiredObjectsIntoScript();

  ParseScriptLines();
  ReadMainBranchStatementsList();

  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

  ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
  AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

  AssertEquals(WRONG_SELECTOR_VALUE, DEFAULT_WEIGHT_SELECTOR_VALUE, ChooserItems[0].Weight);
  AssertEquals(WRONG_SELECTOR_VALUE, DEFAULT_WEIGHT_SELECTOR_VALUE, ChooserItems[1].Weight);
end;


initialization
  RegisterTest(PARSER_TEST_SUITE_NAME, TWallpaperChooserStatementParsingTest);


end.

