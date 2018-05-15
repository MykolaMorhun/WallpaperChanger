unit UseBranchChooserStatementParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  ChooserStatementBaseTestCase,
  WpcChooserStatements,
  WpcScriptParser;

type

  { TUseBranchChooserStatementParsingTest }

  TUseBranchChooserStatementParsingTest = class(TAbstractChooserStatementParsingTest)
  public const
    TEST_BRANCH1 = 'BranchA';
    TEST_BRANCH2 = 'BranchB';
  protected
    procedure SetUp(); override;
  protected
    procedure AddRequiredObjectsIntoScript(); override;
  published
    procedure ShouldParseItemWithDefaultWeightWhichConsistsFromOneWord();
  end;

implementation

{ TUseBranchChooserStatementParsingTest }

procedure TUseBranchChooserStatementParsingTest.SetUp();
begin
  inherited SetUp();

  ChooserType := ' ' + BRANCH_KEYWORD + ' ' + TO_KEYWORD + ' ' + USE_KEYWORD + ' ';
  ChooserItem1 := '  ' + TEST_BRANCH1 + X_TIMES_PROPERTY;
  ChooserItem2 := '  ' + TEST_BRANCH2 + ' ';
end;

procedure TUseBranchChooserStatementParsingTest.AddRequiredObjectsIntoScript();
begin
  ScriptLines.Add(BRANCH_KEYWORD + ' ' + TEST_BRANCH1);
  ScriptLines.Add(END_KEYWORD + ' ' + BRANCH_KEYWORD);
  ScriptLines.Add(BRANCH_KEYWORD + ' ' + TEST_BRANCH2);
  ScriptLines.Add(END_KEYWORD + ' ' + BRANCH_KEYWORD);
end;

// CHOOSE BRANCH TO SWITCH FROM
//   BranchA
//   BranchB
// END CHOOSE
procedure TUseBranchChooserStatementParsingTest.ShouldParseItemWithDefaultWeightWhichConsistsFromOneWord();
begin
  ScriptLines.Add(CHOOSE_KEYWORD + ChooserType + FROM_KEYWORD);
  ScriptLines.Add('  ' + TEST_BRANCH1);
  ScriptLines.Add('  ' + TEST_BRANCH2);
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
  RegisterTest(PARSER_TEST_SUITE_NAME, TUseBranchChooserStatementParsingTest);


end.

