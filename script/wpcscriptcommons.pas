unit WpcScriptCommons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcExceptions;

type
  TWpcStatemetId = (
    WPC_WAIT_STATEMENT_ID,
    WPC_WALLPAPER_STATEMENT_ID,
    WPC_DIRECTORY_STATEMENT_ID,
    WPC_STOP_STATEMENT_ID,
    WPC_SWITCH_BRANCH_STATEMENT_ID,
    WPC_USE_BRANCH_STATEMENT_ID,
    WPC_WALLPAPER_CHOOSER_STATEMENT_ID,
    WPC_BRANCH_TO_USE_CHOOSER_STATEMENT_ID,
    WPC_BRANCH_TO_SWITCH_CHOOSER_STATEMENT_ID,
    WPC_BRANCH_STATEMENT_ID,

    WPC_END_OF_BLOCK_STATEMENT,

    WPC_UNKNOWN_STATEMENT
  );

  TWpcStatementPropertyId = (
    WPC_DELAY_STATEMENT_PROPERY_ID,
    WPC_TIMES_STATEMENT_PROPERY_ID,
    WPC_PROBABILITY_STATEMENT_PROPERY_ID,
    WPC_WALLPAPER_STYLE_PROPERTY_ID,

    WPC_UNKNOWN_STATEMENT_PROPERTY
  );


  function StatementIdToStr(StatementId : TWpcStatemetId) : String;
  function StatementPropertyIdToStr(StatementPropertyId : TWpcStatementPropertyId) : String;

implementation

function StatementIdToStr(StatementId: TWpcStatemetId): String;
begin
  case (StatementId) of
    WPC_WAIT_STATEMENT_ID:                     Result := 'WAIT';
    WPC_WALLPAPER_STATEMENT_ID:                Result := 'SET WALLPAPER';
    WPC_DIRECTORY_STATEMENT_ID:                Result := 'SET WALLPAPERS FROM DIRECTORY';
    WPC_STOP_STATEMENT_ID:                     Result := 'STOP';
    WPC_SWITCH_BRANCH_STATEMENT_ID:            Result := 'SWITCH BRANCH';
    WPC_USE_BRANCH_STATEMENT_ID:               Result := 'USE BRANCH';
    WPC_WALLPAPER_CHOOSER_STATEMENT_ID:        Result := 'WALLPAPER CHOOSER';
    WPC_BRANCH_TO_USE_CHOOSER_STATEMENT_ID:    Result := 'BRANCH TO USE CHOOSER';
    WPC_BRANCH_TO_SWITCH_CHOOSER_STATEMENT_ID: Result := 'BRANCH TO SWITCH CHOOSER';
    WPC_BRANCH_STATEMENT_ID:                   Result := 'BRANCH';
    WPC_END_OF_BLOCK_STATEMENT:                Result := 'END';
    WPC_UNKNOWN_STATEMENT:                     Result := '_UNKNOWN_'
    else
      // Should never happen
      raise TWpcException.Create('Unknown statement id.');
  end;
end;

function StatementPropertyIdToStr(StatementPropertyId: TWpcStatementPropertyId): String;
begin
  case (StatementPropertyId) of
    WPC_DELAY_STATEMENT_PROPERY_ID:       Result := 'DELAY PROPERTY';
    WPC_TIMES_STATEMENT_PROPERY_ID:       Result := 'TIMES PROPERTY';
    WPC_PROBABILITY_STATEMENT_PROPERY_ID: Result := 'PROBABILITY PROPRTY';
    WPC_WALLPAPER_STYLE_PROPERTY_ID:      Result := 'STYLE PROPERTY';
    WPC_UNKNOWN_STATEMENT_PROPERTY:       Result := 'UNKNOWN PROPERTY';
    else
      // Should never happen
      raise TWpcException.Create('Unknown statement id.');
  end;
end;

end.

