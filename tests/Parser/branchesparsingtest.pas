unit BranchesParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  WpcScript,
  WpcScriptParser;

type

  { TBranchesParsingTest }

  TBranchesParsingTest = class(TParserBaseTestCase)
  public const
    WALLPAPER_IMAGE_FILE = 'wp.jpg';
    SET_WALLPAPER_STATEMENT = SET_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + WALLPAPER_IMAGE_FILE;
    WAIT_STSTEMENT = WAIT_KEYWORD + ' 5m';

    ADDITIONAL_BRANCH1_NAME = 'SomeBranch';
    ADDITIONAL_BRANCH2_NAME = 'Branch2';
  protected
    procedure AddSimpleBranch(BranchName : String);
  published
    procedure ShouldParseSingleMainBranch();
    procedure ShouldParseAFewBranchesMainFirst();
    procedure ShouldParseAFewBranchesMainInTheMiddle();
    procedure ShouldParseAFewBranchesMainLast();
    procedure ShouldParseEmptyBranch();

    procedure SholudRaiseScriptParseExceptionWhenNoMainBranch();
    procedure SholudRaiseScriptParseExceptionWhenMainBranchIsEmpty();
  end;


implementation

{ TBranchesParsingTest }

procedure TBranchesParsingTest.AddSimpleBranch(BranchName : String);
begin
  ScriptLines.Add(BRANCH_KEYWORD + ' ' + BranchName);
  ScriptLines.Add(SET_WALLPAPER_STATEMENT);
  ScriptLines.Add(WAIT_STSTEMENT);
  ScriptLines.Add(END_KEYWORD + ' ' + BRANCH_KEYWORD);
end;


procedure TBranchesParsingTest.ShouldParseSingleMainBranch();
begin
  AddSimpleBranch(MAIN_BARNCH);

  ParseScriptLines();

  AssertEquals(1, Script.GetBranchNames().Count);
end;

procedure TBranchesParsingTest.ShouldParseAFewBranchesMainFirst();
begin
  AddSimpleBranch(MAIN_BARNCH);
  AddSimpleBranch(ADDITIONAL_BRANCH1_NAME);
  AddSimpleBranch(ADDITIONAL_BRANCH2_NAME);

  ParseScriptLines();

  AssertEquals(3, Script.GetBranchNames().Count);
end;

procedure TBranchesParsingTest.ShouldParseAFewBranchesMainInTheMiddle();
begin
  AddSimpleBranch(ADDITIONAL_BRANCH1_NAME);
  AddSimpleBranch(MAIN_BARNCH);
  AddSimpleBranch(ADDITIONAL_BRANCH2_NAME);

  ParseScriptLines();

  AssertEquals(3, Script.GetBranchNames().Count);
end;

procedure TBranchesParsingTest.ShouldParseAFewBranchesMainLast();
begin
  AddSimpleBranch(ADDITIONAL_BRANCH1_NAME);
  AddSimpleBranch(MAIN_BARNCH);
  AddSimpleBranch(ADDITIONAL_BRANCH2_NAME);

  ParseScriptLines();

  AssertEquals(3, Script.GetBranchNames().Count);
end;

procedure TBranchesParsingTest.ShouldParseEmptyBranch();
begin
  AddSimpleBranch(MAIN_BARNCH);
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH1_NAME);

  ParseScriptLines();

  AssertEquals(2, Script.GetBranchNames().Count);
end;

procedure TBranchesParsingTest.SholudRaiseScriptParseExceptionWhenNoMainBranch();
begin
  AddSimpleBranch(ADDITIONAL_BRANCH1_NAME);
  AddSimpleBranch(ADDITIONAL_BRANCH2_NAME);

  AssertScriptParseExceptionOnParse();
end;

procedure TBranchesParsingTest.SholudRaiseScriptParseExceptionWhenMainBranchIsEmpty();
begin
  AddEmptyBranch(ScriptLines, ADDITIONAL_BRANCH1_NAME);

  AssertScriptParseExceptionOnParse();
end;


initialization
  RegisterTest(PARSER_TEST_SUITE_NAME, TBranchesParsingTest);


end.

