unit WpcScriptAutocompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  IWpcScriptAutocompletion = class abstract(TObject)
  public
    {
      Creates list of words which is sutable to complete current word from scratch.
      Should be used on new autocomplete invokation.
      Invoker must free returned list.
      Parameters:
        - FillScript: all script lines
        - CursorPosition: current position of caret (col,row). Starts from 1.
          Might be out of script (right or down)
        - CurrentWord: word under completion
    }
    function CreateAutocompleteList(FullScript : TStrings;
                                    CursorPosition : TPoint;
                                    CurrentWord : String) : TStrings; virtual; abstract;
    {
      Creates list of words which is sutable to complete current word reusing previously parsed data.
      Should be used on current autocomplete filter changes.
      Invoker must free returned list.
      Parameters:
        - CurrentWord: word under completion
    }
    function UpdateAutocompleteList(CurrentWord : String) : TStrings; virtual; abstract;
  end;


implementation

end.

