unit WpcMainManuActions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MM_NO_ACTION_STRING = 'Do nothing';
  MM_SHOW_MAIN_MENU_STRING = 'Show Main Menu';
  MM_EXIT_ACTION_STRING = 'Exit program';
  MM_OPEN_DOCUMENTATION_ACTION_STRING = 'Open documentation';
  MM_OPEN_SETTINGS_ACTION_STRING = 'Open settings';
  MM_SET_SINGLE_WALLPAPER_ACTION_STRING = 'Set wallpaper';
  MM_SET_DIRECTORY_ACTION_STRING = 'Set directory';
  MM_RUN_SCRIPT_ACTION_STRING = 'Run script';
  MM_OPEN_SCRIPT_EDITOR_ACTION_STRING = 'Open Script Editor';
  MM_NEXT_WALLPAPER_STRING = 'Set next wallpaper';
  MM_STOP_OR_RERUN_ACTION_STRING = 'Stop or Rerun script';
  MM_STOP_ONLY_ACTION_STRING = 'Stop script if any';

type

  EMainManuActions = (
    MM_NO_ACTION,
    MM_SHOW_MAIN_MENU,
    MM_EXIT_ACTION,
    MM_OPEN_DOCUMENTATION_ACTION,
    MM_OPEN_SETTINGS_ACTION,
    MM_SET_SINGLE_WALLPAPER_ACTION,
    MM_SET_DIRECTORY_ACTION,
    MM_RUN_SCRIPT_ACTION,
    MM_OPEN_SCRIPT_EDITOR_ACTION,
    MM_NEXT_WALLPAPER,
    MM_STOP_OR_RERUN_ACTION,
    MM_STOP_ONLY_ACTION
  );

function MainManuActionToStr(MMAction : EMainManuActions) : String;
function StrToMainMenuAction(MMActionString : String) : EMainManuActions;

implementation

function MainManuActionToStr(MMAction : EMainManuActions) : String;
begin
  case (MMAction) of
    MM_NO_ACTION:                   Result := MM_NO_ACTION_STRING;
    MM_SHOW_MAIN_MENU:              Result := MM_SHOW_MAIN_MENU_STRING;
    MM_EXIT_ACTION:                 Result := MM_EXIT_ACTION_STRING;
    MM_OPEN_DOCUMENTATION_ACTION:   Result := MM_OPEN_SCRIPT_EDITOR_ACTION_STRING;
    MM_OPEN_SETTINGS_ACTION:        Result := MM_OPEN_SETTINGS_ACTION_STRING;
    MM_SET_SINGLE_WALLPAPER_ACTION: Result := MM_SET_SINGLE_WALLPAPER_ACTION_STRING;
    MM_SET_DIRECTORY_ACTION:        Result := MM_SET_DIRECTORY_ACTION_STRING;
    MM_RUN_SCRIPT_ACTION:           Result := MM_RUN_SCRIPT_ACTION_STRING;
    MM_OPEN_SCRIPT_EDITOR_ACTION:   Result := MM_OPEN_SCRIPT_EDITOR_ACTION_STRING;
    MM_NEXT_WALLPAPER:              Result := MM_NEXT_WALLPAPER_STRING;
    MM_STOP_OR_RERUN_ACTION:        Result := MM_STOP_OR_RERUN_ACTION_STRING;
    MM_STOP_ONLY_ACTION:            Result := MM_STOP_ONLY_ACTION_STRING;
  end;
end;

function StrToMainMenuAction(MMActionString : String) : EMainManuActions;
begin
  MMActionString := UpperCase(MMActionString);
  if (UpperCase(MM_NO_ACTION_STRING) = MMActionString) then
    Result := MM_NO_ACTION
  else if (UpperCase(MM_SHOW_MAIN_MENU_STRING) = MMActionString) then
    Result := MM_SHOW_MAIN_MENU
  else if (UpperCase(MM_EXIT_ACTION_STRING) = MMActionString) then
    Result := MM_EXIT_ACTION
  else if (UpperCase(MM_OPEN_DOCUMENTATION_ACTION_STRING) = MMActionString) then
    Result := MM_OPEN_DOCUMENTATION_ACTION
  else if (UpperCase(MM_OPEN_SETTINGS_ACTION_STRING) = MMActionString) then
    Result := MM_OPEN_SETTINGS_ACTION
  else if (UpperCase(MM_SET_SINGLE_WALLPAPER_ACTION_STRING) = MMActionString) then
    Result := MM_SET_SINGLE_WALLPAPER_ACTION
  else if (UpperCase(MM_SET_DIRECTORY_ACTION_STRING) = MMActionString) then
    Result := MM_SET_DIRECTORY_ACTION
  else if (UpperCase(MM_RUN_SCRIPT_ACTION_STRING) = MMActionString) then
    Result := MM_RUN_SCRIPT_ACTION
  else if (UpperCase(MM_OPEN_SCRIPT_EDITOR_ACTION_STRING) = MMActionString) then
    Result := MM_OPEN_SCRIPT_EDITOR_ACTION
  else if (UpperCase(MM_NEXT_WALLPAPER_STRING) = MMActionString) then
    Result := MM_NEXT_WALLPAPER
  else if (UpperCase(MM_STOP_OR_RERUN_ACTION_STRING) = MMActionString) then
    Result := MM_STOP_OR_RERUN_ACTION
  else if (UpperCase(MM_STOP_ONLY_ACTION_STRING) = MMActionString) then
    Result := MM_STOP_ONLY_ACTION
  else
    Result := MM_NO_ACTION;
end;


end.

