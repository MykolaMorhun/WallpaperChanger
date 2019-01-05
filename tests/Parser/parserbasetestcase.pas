unit ParserBaseTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FpcUnit, TestRegistry,
  WpcScriptParser,
  WpcScript,
  WpcBranchStatement,
  WpcExceptions,
  WpcWallpaperStyles;

const
  PARSER_TEST_SUITE_NAME = 'Parser';

  DEFAULT_PROBABILITY = 100;
  DEFAULT_TIMES = 1;

  TEST_DEFAULT_DELAY_STRING = '5m';
  TEST_DEFAULT_DELAY_VALUE = 5 * 60 * 1000;
  TEST_DEFAULT_PROBABILITY_STRING = '50';
  TEST_DEFAULT_PROBABILITY_VALUE = 50;
  TEST_DEFAULT_TIMES_STRING = '4';
  TEST_DEFAULT_TIMES_VALUE = 4;
  TEST_DEFAULT_STYLE_STRING = 'STRETCHED';
  TEST_DEFAULT_STYLE_VALUE = STRETCHED;

  // Spaces are added for future concatanation of the items.
  DELAY_FOR_PROPERTY = ' ' + FOR_KEYWORD + ' ' + TEST_DEFAULT_DELAY_STRING + ' ';
  X_TIMES_PROPERTY = ' ' + TEST_DEFAULT_TIMES_STRING + ' ' + TIMES_KEYWORD + ' ';
  WITH_PROBABILITY_PROPERTY = ' ' + WITH_KEYWORD + ' ' + PROBABILITY_KEYWORD + ' ' + TEST_DEFAULT_PROBABILITY_STRING + ' ';
  STYLE_PROPERTY = ' ' + STYLE_KEYWORD + ' ' + TEST_DEFAULT_STYLE_STRING + ' ';

  // Fail test error messages
  WRONG_NUMBER_OF_BRANCHES = 'Script has wrong number of branches';
  WRONG_NUMBER_OF_SATEMENTS = 'Branch has wrong number of statements';
  WRONG_STATEMENT = 'Wrong statement type';
  WRONG_STATEMENT_PROPRTY_VALUE = 'Wrong statement property value';
  WRONG_SELECTOR_VALUE = 'Wrong selector value';

type
  // variable name , variable value
  VariableDefinition = Array[1..2] of String;

  { TParserBaseTestCase }

  TParserBaseTestCase = class(TTestCase)
  protected
    ScriptLines          : TStringList;
    Script               : TWpcScript;
    Parser               : TWpcScriptParser;
    MainBranchStatements : TListOfBranchStatements;
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
  protected
    procedure WrapInMainBranch(Statements : TStringList);
    procedure AddEmptyBranch(Statements : TStringList; BranchName : String);
    procedure AddVariables(Statements : TStringList; HeaderSectionKeyWord: String; Variables : Array of VariableDefinition);
    procedure ParseScriptLines();
    function GetBranchStatemntsList(BranchName : String) : TListOfBranchStatements;
    procedure ReadMainBranchStatementsList();
  public
    procedure AssertScriptParseExceptionOnParse();
    procedure AssertScriptParseExceptionOnParse(Line : Integer);
    procedure AssertScriptParseExceptionOnParse(Line : Integer; WordNumber : Integer);
  end;


implementation

{ TParserBaseTestCase }

procedure TParserBaseTestCase.SetUp();
begin
  ScriptLines := TStringList.Create();
  Script := nil;
  Parser := nil;
  // Shouldn't be freed because it is part of main branch. Will be freed with its script.
  MainBranchStatements := nil;
end;

procedure TParserBaseTestCase.TearDown();
begin
  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  if (Script <> nil) then FreeAndNil(Script);
  if (Parser <> nil) then FreeAndNil(Parser);
end;

procedure TParserBaseTestCase.WrapInMainBranch(Statements : TStringList);
begin
  Statements.Insert(0, BRANCH_KEYWORD + ' ' + MAIN_BARNCH);
  Statements.Add(END_KEYWORD + ' ' + BRANCH_KEYWORD);
end;

procedure TParserBaseTestCase.AddEmptyBranch(Statements: TStringList; BranchName: String);
begin
  Statements.Add(BRANCH_KEYWORD + ' ' + BranchName);
  Statements.Add(END_KEYWORD + ' ' + BRANCH_KEYWORD);
end;

{
  Adds header section at the beginning of the given script.
  Parameters:
   - Statements: the script into which section should be added
   - HeaderSectionKeyWord: defines which section should be added
   - Variables: pairs of variable name and variable value
}
procedure TParserBaseTestCase.AddVariables(Statements : TStringList; HeaderSectionKeyWord : String; Variables : Array of VariableDefinition);
var
  i : Integer;
begin
  Statements.Insert(0, END_KEYWORD + ' ' + HeaderSectionKeyWord);
  for i := (Length(Variables) - 1) downto 0 do begin
    Statements.Insert(0, Variables[i, 1] + ' ' + Variables[i, 2]);
  end;
  Statements.Insert(0, HeaderSectionKeyWord);
end;

procedure TParserBaseTestCase.ParseScriptLines();
begin
  Parser := TWpcScriptParser.Create(ScriptLines);
  Parser.CheckScriptResources := False;
  Script := Parser.Parse();
end;

{
  Returns list of the given branch statements.
  Returned list shouldn't be modified or freed by invoker.
}
function TParserBaseTestCase.GetBranchStatemntsList(BranchName : String) : TListOfBranchStatements;
var
  Branch : TWpcBranchStatement;
begin
  if (Script = nil) then
    Fail('Parse script before getting a branch statements.');
  Branch := Script.GetBranch(BranchName);
  if (Branch = nil) then
    Fail('Script does not contain branch: ' + BranchName);
  Result := Branch.GetBranchStatements();
end;

{
  Sets MainBranchStatements field. It shouldn't be modified or freed by invoker.
}
procedure TParserBaseTestCase.ReadMainBranchStatementsList();
begin
  MainBranchStatements := GetBranchStatemntsList(MAIN_BARNCH);
end;

procedure TParserBaseTestCase.AssertScriptParseExceptionOnParse();
begin
  AssertScriptParseExceptionOnParse(TWpcScriptParseException.UNKNOWN_LINE, TWpcScriptParseException.UNKNOWN_WORD_NUMBER);
end;

procedure TParserBaseTestCase.AssertScriptParseExceptionOnParse(Line: Integer);
begin
  AssertScriptParseExceptionOnParse(Line, TWpcScriptParseException.UNKNOWN_WORD_NUMBER);
end;

{
  Performs script parsing and checks that TWpcScriptParseException was thrown at given line and word numbers.
  Note, line and word numbers is counted from 0
}
procedure TParserBaseTestCase.AssertScriptParseExceptionOnParse(Line: Integer; WordNumber: Integer);
begin
  try
    ParseScriptLines();
    Fail('An TWpcScriptParseException is expected but nothing was thrown.');
  except
    on ScriptParsingException : TWpcScriptParseException do begin
      if ((Line <> TWpcScriptParseException.UNKNOWN_LINE) and (Line <> ScriptParsingException.Line)) then begin
        Fail('The Script parse exception is expected at line ' + IntToStr(Line) + ' of script, ' +
             'but occured at line ' + IntToStr(ScriptParsingException.Line) + '. Message: ' + ScriptParsingException.Message);
      end;
      if ((WordNumber <> TWpcScriptParseException.UNKNOWN_WORD_NUMBER) and (WordNumber <> ScriptParsingException.WordNumer)) then begin
        Fail('The Script parse exception is expected at line ' + IntToStr(Line) + ' word ' + IntToStr(WordNumber) + ', ' +
             'but occured at line ' + IntToStr(ScriptParsingException.Line) + ' word ' + IntToStr(ScriptParsingException.WordNumer) + '. Message: ' + ScriptParsingException.Message);
      end;
    end;
    on UnexpectedException : Exception do begin
      Fail('Unexpected exception: ' + UnexpectedException.ToString());
    end;
  end;
end;


end.

