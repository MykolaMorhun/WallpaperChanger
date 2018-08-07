unit WpcStaticScriptAutocompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcAbstractScriptAutocompletion;

type

  { TWpcStaticScriptAutocompletion }
  // Provides completion based on static list of all keywords and constants.
  // It doesn't apply any logic to filter the list by position or other signs.
  TWpcStaticScriptAutocompletion = class(TWpcAbstractScriptAutocompletion)
  public
    constructor Create();
  public
    function CreateAutocompleteList(FullScript : TStrings;
                                    CursorPosition : TPoint;
                                    CurrentWord : String) : TStrings; override;
  end;

implementation

{ TWpcStaticScriptAutocompletion }

constructor TWpcStaticScriptAutocompletion.Create();
begin
  inherited Create();

  FCachedOptions.Assign(FStaticOptions);
end;

function TWpcStaticScriptAutocompletion.CreateAutocompleteList(FullScript : TStrings;
                                                               CursorPosition : TPoint;
                                                               CurrentWord : String) : TStrings;
begin
  // Static options list has already assigned to cached options. Just filter by word beginning.
  Result := UpdateAutocompleteList(CurrentWord);
end;


end.

