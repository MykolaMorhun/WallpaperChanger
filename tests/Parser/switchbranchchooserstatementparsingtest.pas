unit SwitchBranchChooserStatementParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  ChooserStatementBaseTestCase,
  WpcChooserStatements,
  WpcScriptParser;

type

  { TSwitchBranchChooserStatementParsingTest }

  TSwitchBranchChooserStatementParsingTest = class(TAbstractChooserStatementParsingTest)
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

{ TSwitchBranchChooserStatementParsingTest }

procedure TSwitchBranchChooserStatementParsingTest.SetUp();
begin
  inherited SetUp();

  ChooserType := ' ' + BRANCH_KEYWORD + ' ' + TO_KEYWORD + ' ' + SWITCH_KEYWORD + ' ';
  ChooserItem1 := '  ' + TEST_BRANCH1 + WITH_PROBABILITY_PROPERTY;
  ChooserItem2 := '  ' + TEST_BRANCH2 + ' ';
end;

procedure TSwitchBranchChooserStatementParsingTest.AddRequiredObjectsIntoScript();
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
procedure TSwitchBranchChooserStatementParsingTest.ShouldParseItemWithDefaultWeightWhichConsistsFromOneWord();
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
  RegisterTest(PARSER_TEST_SUITE_NAME, TSwitchBranchChooserStatementParsingTest);


end.

