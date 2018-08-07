unit WpcAbstractScriptAutocompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcScriptAutocompletion,
  WpcScriptEditorConstantsList,
  WpcWallpaperStyles,
  WpcScriptParser;

type

  { TWpcAbstractScriptAutocompletion }

  TWpcAbstractScriptAutocompletion = class abstract(IWpcScriptAutocompletion)
  protected
    // Keeps autocomplete options when filter changes.
    FCachedOptions : TStrings;
    // Contains all keywords and constants.
    FStaticOptions : TStrings;
  public
    constructor Create();
    destructor Destroy(); override;
  private
    procedure InitStaticPart(); inline;
  public
    function UpdateAutocompleteList(CurrentWord : String) : TStrings; override;
  end;


implementation

{ TWpcAbstractScriptAutocompletion }

constructor TWpcAbstractScriptAutocompletion.Create();
begin
  FStaticOptions := TStringList.Create();
  FCachedOptions := TStringList.Create();

  InitStaticPart();
end;

destructor TWpcAbstractScriptAutocompletion.Destroy();
begin
  FStaticOptions.Free();
  FCachedOptions.Free();
end;

procedure TWpcAbstractScriptAutocompletion.InitStaticPart();
var
  WallpaperStyle : TWallpaperStyle;
begin
  FStaticOptions.AddStrings(KEYWORDS);
  FStaticOptions.AddStrings(SELECTORS);
  FStaticOptions.AddStrings(SEASONS);
  FStaticOptions.AddStrings(MONTHS);
  FStaticOptions.AddStrings(DAYS_OF_WEEK);

  // Put all wallpaper styles as static here
  // because one could write script for another desktop environment.
  for WallpaperStyle in TWallpaperStyle do
    FStaticOptions.Add(WallpaperStyleToStr(WallpaperStyle));
end;

function TWpcAbstractScriptAutocompletion.UpdateAutocompleteList(CurrentWord : String) : TStrings;
var
  Options : TStrings;
  Word    : String;

  // Filter strings with already types beginning.
  procedure AddIfMatches(Option : String); inline;
  begin
    if (Option.StartsWith(CurrentWord)) then
      Options.Add(Option);
  end;

begin
  CurrentWord := UpperCase(CurrentWord);

  Options := TStringList.Create();

  if (CurrentWord = '') then begin
    // Autocomplete was invoked on white space
    Options.Assign(FCachedOptions);
  end
  else begin
    for Word in FCachedOptions do
      AddIfMatches(Word);
  end;

  Result := Options;
end;


end.

