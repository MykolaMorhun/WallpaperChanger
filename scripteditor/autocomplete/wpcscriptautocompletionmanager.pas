unit WpcScriptAutocompletionManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcScriptAutocompletion,
  WpcStaticScriptAutocompletion,
  WpcBasicScriptAutocompletion,
  WpcAdvancedScriptAutocompletion;

type

  TWpcScriptAutocompletionMode = (SACM_STATIC, SACM_BASIC, SACM_ADVANCED);

  { TWpcScriptAutocompletionManager }

  TWpcScriptAutocompletionManager = class(IWpcScriptAutocompletion)
  private
    type
      TWpcCreateAutocompleteListFunction = function(FullScript : TStrings;
                                                    CursorPosition : TPoint;
                                                    CurrentWord : String) : TStrings of object;

      TWpcUpdateAutocompleteListFunction = function(CurrentWord : String) : TStrings of object;
  private
    FAutocompletion : IWpcScriptAutocompletion;
    FCurrentMode    : TWpcScriptAutocompletionMode;

    FCreateAutoCompleteListFunction : TWpcCreateAutocompleteListFunction;
    FUpdateAutocompleteListFunction : TWpcUpdateAutocompleteListFunction;
  public
    constructor Create(Mode : TWpcScriptAutocompletionMode);
    destructor Destroy(); override;
  public
    function CreateAutocompleteList(FullScript : TStrings;
                                    CursorPosition : TPoint;
                                    CurrentWord : String) : TStrings; override;

    function UpdateAutocompleteList(CurrentWord : String) : TStrings; override;
  private
    procedure SetAutocompletionMode(Mode : TWpcScriptAutocompletionMode);
    function GetAutocompletionmode() : TWpcScriptAutocompletionMode;
  public
    property Mode : TWpcScriptAutocompletionMode read GetAutocompletionmode write SetAutocompletionMode;
  end;

implementation


{ TWpcScriptAutocompletionManager }

constructor TWpcScriptAutocompletionManager.Create(Mode : TWpcScriptAutocompletionMode);
begin
  FAutocompletion := nil;

  SetAutocompletionMode(Mode);
end;

destructor TWpcScriptAutocompletionManager.Destroy();
begin
  if (FAutocompletion <> nil) then FAutocompletion.Free();
end;

function TWpcScriptAutocompletionManager.CreateAutocompleteList(FullScript : TStrings;
                                                                CursorPosition : TPoint;
                                                                CurrentWord : String) : TStrings;
begin
  Result := FCreateAutoCompleteListFunction(FullScript, CursorPosition, CurrentWord);
end;

function TWpcScriptAutocompletionManager.UpdateAutocompleteList(CurrentWord : String) : TStrings;
begin
  Result := FUpdateAutocompleteListFunction(CurrentWord);
end;

procedure TWpcScriptAutocompletionManager.SetAutocompletionMode(Mode : TWpcScriptAutocompletionMode);
begin
  if (FAutocompletion <> nil) then FAutocompletion.Free();
  FCurrentMode := Mode;

  case (Mode) of
    SACM_STATIC:
      FAutocompletion := TWpcStaticScriptAutocompletion.Create();
    SACM_BASIC:
      FAutocompletion := TWpcBasicScriptAutocompletion.Create();
    SACM_ADVANCED:
      FAutocompletion := TWpcAdvancedScriptAutocompletion.Create();
  end;

  FCreateAutoCompleteListFunction := @FAutocompletion.CreateAutocompleteList;
  FUpdateAutocompleteListFunction := @FAutocompletion.UpdateAutocompleteList;
end;

function TWpcScriptAutocompletionManager.GetAutocompletionmode(): TWpcScriptAutocompletionMode;
begin
  Result := FCurrentMode;
end;


end.

