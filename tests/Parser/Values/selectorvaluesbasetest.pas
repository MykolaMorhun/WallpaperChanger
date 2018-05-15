unit SelectorValuesBaseTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ParserBaseTestCase,
  ValuesParsingBaseTest,
  WpcScriptCommons,
  WpcChooserStatements,
  WpcScriptParser,
  WpcExceptions;

type

  { TSelectorValuesBaseTest }

  TSelectorValuesBaseTest = class abstract(TValuesParsingBaseTest)
  protected const
    CHOOSER_ITEM_1 = 'wallpaper.jpg';
    CHOOSER_ITEM_2 = 'background.png';
  protected
    SELECTOR_STRING : String;
    A_SELECTOR_VALUE_STRING : String;
    A_SELECTOR_VALUE : LongWord;
  protected
    SELECTOR_VALUES_STRING : Array of String;
    SELECTOR_VALUES : Array of LongWord;
    SELECTOR_VALUES_ALIASES_STRING : Array of String;
    SELECTOR_VALUES_ALIASES : Array of LongWord;
    INVALID_SELECTOR_VALUES_STRING : Array of String;
  public
    constructor Create(); override;
    destructor Destroy(); override;
  protected
    procedure SetSelectorValuesTestData(SelectorsStrings : Array of String; SelectorValues : Array of LongWord);
    procedure SetSelectorValuesAliasesTestData(AliasesStrings : Array of String; AliasesValues : Array of LongWord);
    procedure SetInvalidSelectorValuesStrings(InvalidSelectorsStrings : Array of String);
  protected
    function ParseAndGetSelectorValue(ValueString : String) : LongWord;
    procedure ParseAndEnsureScriptParseException(ValueString : String);

    procedure ParseAndCheckSelectorValues(SelectorsStrings: Array of String; SelectorsValues: Array of LongWord);
  published
    procedure ShouldParseSelectorValue();
    procedure ShouldParseSelectorValueByAlias();

    procedure ShouldRaiseScriptParseExceptionIfInvalidSelectorValueGiven();
  end;

implementation

{ TSelectorValuesBaseTest }

constructor TSelectorValuesBaseTest.Create();
begin
  SELECTOR_VALUES_STRING := nil;
  SELECTOR_VALUES := nil;
  SELECTOR_VALUES_ALIASES_STRING := nil;
  SELECTOR_VALUES_ALIASES := nil;
  INVALID_SELECTOR_VALUES_STRING := nil;
end;

destructor TSelectorValuesBaseTest.Destroy();
begin
  SetLength(SELECTOR_VALUES_STRING, 0);
  SetLength(SELECTOR_VALUES, 0);
  SetLength(SELECTOR_VALUES_ALIASES_STRING, 0);
  SetLength(SELECTOR_VALUES_ALIASES, 0);
  SetLength(INVALID_SELECTOR_VALUES_STRING, 0);
end;

procedure TSelectorValuesBaseTest.SetSelectorValuesTestData(SelectorsStrings : Array of String; SelectorValues: Array of LongWord);
var
  i : Integer;
  Len : Integer;
begin
  if (Length(SelectorsStrings) <> Length(SelectorValues)) then
    raise TWpcIllegalArgumentException.Create('Test data arrays should have the same length.');

  Len := Length(SelectorsStrings);
  SetLength(SELECTOR_VALUES_STRING, Len);
  SetLength(SELECTOR_VALUES, Len);
  for i:=0 to (Len - 1) do begin
    SELECTOR_VALUES_STRING[i] := SelectorsStrings[i];
    SELECTOR_VALUES[i] := SelectorValues[i];
  end;
end;

procedure TSelectorValuesBaseTest.SetSelectorValuesAliasesTestData(AliasesStrings : Array of String; AliasesValues : Array of LongWord);
var
  i : Integer;
  Len : Integer;
begin
  if (Length(AliasesStrings) <> Length(AliasesValues)) then
    raise TWpcIllegalArgumentException.Create('Test data arrays should have the same length.');

  Len := Length(AliasesStrings);
  SetLength(SELECTOR_VALUES_ALIASES_STRING, Len);
  SetLength(SELECTOR_VALUES_ALIASES, Len);
  for i:=0 to (Len - 1) do begin
    SELECTOR_VALUES_ALIASES_STRING[i] := AliasesStrings[i];
    SELECTOR_VALUES_ALIASES[i] := AliasesValues[i];
  end;
end;

procedure TSelectorValuesBaseTest.SetInvalidSelectorValuesStrings(InvalidSelectorsStrings : Array of String);
var
  i : Integer;
  Len : Integer;
begin
  Len := Length(InvalidSelectorsStrings);
  SetLength(INVALID_SELECTOR_VALUES_STRING, Len);
  for i:=0 to (Len - 1) do
    INVALID_SELECTOR_VALUES_STRING[i] := InvalidSelectorsStrings[i];
end;

function TSelectorValuesBaseTest.ParseAndGetSelectorValue(ValueString : String) : LongWord;
var
  ChooserItems : TListOfChooserItems;
begin
  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  ScriptLines := TStringList.Create();
  try
    ScriptLines.Add(CHOOSE_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + BY_KEYWORD + ' ' + SELECTOR_STRING + ' ' + FROM_KEYWORD);
    ScriptLines.Add(CHOOSER_ITEM_1 + ' ' + SELECTOR_STRING + ' ' + A_SELECTOR_VALUE_STRING);
    ScriptLines.Add(CHOOSER_ITEM_2 + ' ' + SELECTOR_STRING + ' ' + ValueString);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);

    ParseScriptLines();
    ReadMainBranchStatementsList();

    AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);
    AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_CHOOSER_STATEMENT_ID = MainBranchStatements[0].GetId());

    ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
    AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 2, ChooserItems.Count);

    AssertEquals(WRONG_SELECTOR_VALUE, A_SELECTOR_VALUE, ChooserItems[0].Weight);
    Result := ChooserItems[1].Weight;

  finally
    FreeAndNil(ScriptLines);
    FreeAndNil(Script);
  end;
end;

procedure TSelectorValuesBaseTest.ParseAndCheckSelectorValues(SelectorsStrings: Array of String; SelectorsValues: Array of LongWord);
var
  i            : Integer;
  Len          : Integer;
  ChooserItems : TListOfChooserItems;
begin
  if (Length(SelectorsStrings) <> Length(SelectorsValues)) then
    raise TWpcIllegalArgumentException.Create('Test data arrays should have the same length.');

  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  ScriptLines := TStringList.Create();
  try
    Len := Length(SelectorsValues);

    ScriptLines.Add(CHOOSE_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + BY_KEYWORD + ' ' + SELECTOR_STRING + ' ' + FROM_KEYWORD);
    for i:=0 to (Len - 1) do
      ScriptLines.Add(CHOOSER_ITEM_1 + ' ' + SELECTOR_STRING + ' ' + SelectorsStrings[i]);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);

    ParseScriptLines();
    ReadMainBranchStatementsList();

    AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);
    AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_CHOOSER_STATEMENT_ID = MainBranchStatements[0].GetId());

    ChooserItems := IWpcChooserItems(MainBranchStatements[0]).GetItems();
    AssertEquals(WRONG_NUMBER_OF_SATEMENTS, Len, ChooserItems.Count);

    for i:=0 to (Len - 1) do
      AssertEquals(WRONG_SELECTOR_VALUE, SelectorsValues[i], ChooserItems[i].Weight);
  finally
    FreeAndNil(ScriptLines);
    FreeAndNil(Script);
  end;
end;

procedure TSelectorValuesBaseTest.ParseAndEnsureScriptParseException(ValueString : String);
begin
  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  ScriptLines := TStringList.Create();
  try
    ScriptLines.Add(CHOOSE_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + BY_KEYWORD + ' ' + SELECTOR_STRING + ' ' + FROM_KEYWORD);
    ScriptLines.Add(CHOOSER_ITEM_1 + ' ' + SELECTOR_STRING + ' ' + A_SELECTOR_VALUE_STRING);
    ScriptLines.Add(CHOOSER_ITEM_2 + ' ' + SELECTOR_STRING + ' ' + ValueString);
    ScriptLines.Add(END_KEYWORD + ' ' + CHOOSE_KEYWORD);
    WrapInMainBranch(ScriptLines);

    AssertScriptParseExceptionOnParse(3);
  finally
    FreeAndNil(ScriptLines);
  end;
end;

procedure TSelectorValuesBaseTest.ShouldParseSelectorValue();
var
  i : Integer;
begin
  for i:=0 to (Length(SELECTOR_VALUES_STRING) - 1) do begin
    AssertEquals(FAILED_TO_PARSE + SELECTOR_VALUES_STRING[i],
                 SELECTOR_VALUES[i],
                 ParseAndGetSelectorValue(SELECTOR_VALUES_STRING[i]));
  end;
end;

procedure TSelectorValuesBaseTest.ShouldParseSelectorValueByAlias();
var
  i : Integer;
begin
  for i:=0 to (Length(SELECTOR_VALUES_ALIASES) - 1) do begin
    AssertEquals(FAILED_TO_PARSE + SELECTOR_VALUES_ALIASES_STRING[i],
                 SELECTOR_VALUES_ALIASES[i],
                 ParseAndGetSelectorValue(SELECTOR_VALUES_ALIASES_STRING[i]));
  end;
end;

procedure TSelectorValuesBaseTest.ShouldRaiseScriptParseExceptionIfInvalidSelectorValueGiven;
var
  i : Integer;
begin
  for i:=0 to (Length(INVALID_SELECTOR_VALUES_STRING) - 1) do begin
    ParseAndEnsureScriptParseException(INVALID_SELECTOR_VALUES_STRING[i]);
  end;
end;


end.

