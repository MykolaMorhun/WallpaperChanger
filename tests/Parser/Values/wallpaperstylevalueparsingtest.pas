unit WallpaperStyleValueParsingTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpcUnit, TestRegistry,
  ParserBaseTestCase,
  ValuesParsingBaseTest,
  WpcScriptCommons,
  WpcStatements,
  WpcWallpaperStyles,
  WpcScriptParser;

type

  { TWallpaperStyleValueParsingTest }

  TWallpaperStyleValueParsingTest = class(TValuesParsingBaseTest)
  private const
    n = 3;
    WALLPAPER_STYLES_STRING : Array[1..n] of String = (
      'CENTER', 'TILE', 'STRETCH'
    );
    WALLPAPER_STYLES_VALUES : Array[1..n] of TWallpaperStyle = (
      CENTER, TILE, STRETCH
    );
    WRONG_WALLPAPER_STYLES_STRING : Array[1..4] of String = (
      'UNKNOWN', 'TI4E', 'TI LE', 'TI_LE'
    );

    WALLPAPER_FILE = 'wallpaper.jpg';
  protected
    function ParseAndGetWallpaperStyleValue(WalpaperStyleString : String) : TWallpaperStyle;
    procedure ParseAndEnsureScriptParseException(WallpaperStyleString : String);
  published
    procedure ShouldParseAllWallpaperStylesValues();

    procedure ShouldRaiseScriptParseExceptionIfNoWallpaperStyleValueGiven();
    procedure ShouldRaiseScriptParseExceptionIfInvalidWallpaperStyleValueGiven();
  end;


implementation


{ TWallpaperStyleValueParsingTest }

function TWallpaperStyleValueParsingTest.ParseAndGetWallpaperStyleValue(WalpaperStyleString : String): TWallpaperStyle;
var
  WallpaperStatement : TWpcWallpaperStatement;
begin
  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  ScriptLines := TStringList.Create();
  try
    ScriptLines.Add(SET_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + WALLPAPER_FILE + ' ' + STYLE_KEYWORD + ' ' + WalpaperStyleString);
    WrapInMainBranch(ScriptLines);

    ParseScriptLines();
    ReadMainBranchStatementsList();

    AssertEquals(WRONG_NUMBER_OF_SATEMENTS, 1, MainBranchStatements.Count);

    AssertTrue(WRONG_STATEMENT, WPC_WALLPAPER_STATEMENT_ID = MainBranchStatements[0].GetId());
    WallpaperStatement := TWpcWallpaperStatement(MainBranchStatements[0]);

    Result := WallpaperStatement.GetStyle();
  finally
    FreeAndNil(ScriptLines);
    FreeAndNil(Script);
  end;
end;

procedure TWallpaperStyleValueParsingTest.ParseAndEnsureScriptParseException(WallpaperStyleString : String);
begin
  if (ScriptLines <> nil) then FreeAndNil(ScriptLines);
  ScriptLines := TStringList.Create();
  try
    ScriptLines.Add(SET_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + WALLPAPER_FILE + ' ' + STYLE_KEYWORD + ' ' + WallpaperStyleString);
    WrapInMainBranch(ScriptLines);

    // TODO improve parser to give information about word number
    AssertScriptParseExceptionOnParse(1);
  finally
    FreeAndNil(ScriptLines);
  end;
end;

procedure TWallpaperStyleValueParsingTest.ShouldParseAllWallpaperStylesValues();
var
  i : Integer;
begin
  for i:=1 to Length(WALLPAPER_STYLES_STRING) do begin
    AssertTrue(FAILED_TO_PARSE + WALLPAPER_STYLES_STRING[i],
               WALLPAPER_STYLES_VALUES[i] = ParseAndGetWallpaperStyleValue(WALLPAPER_STYLES_STRING[i]));
  end;
end;

procedure TWallpaperStyleValueParsingTest.ShouldRaiseScriptParseExceptionIfNoWallpaperStyleValueGiven();
begin
  ScriptLines.Add(SET_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + WALLPAPER_FILE + ' ' + STYLE_KEYWORD + ' ');
  WrapInMainBranch(ScriptLines);

  AssertScriptParseExceptionOnParse(1, 4);
end;

procedure TWallpaperStyleValueParsingTest.ShouldRaiseScriptParseExceptionIfInvalidWallpaperStyleValueGiven();
var
  i : Integer;
begin
  for i:=1 to Length(WRONG_WALLPAPER_STYLES_STRING) do begin
    ParseAndEnsureScriptParseException(WRONG_WALLPAPER_STYLES_STRING[i]);
  end;
end;


initialization
  RegisterTest(VALUES_PARSER_TEST_SUITE_NAME, TWallpaperStyleValueParsingTest);


end.

