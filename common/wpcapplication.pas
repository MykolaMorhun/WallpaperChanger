unit WpcApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcOptions, WpcScriptParser;

const
  SETTINGS_FILE = 'WPCSettings.ini';
  STATE_FILE = 'WPCState.ini';

type

  { TWPCApplication }

  // Singleton. Holds application state and modules objects.
  TWpcApplication = class(TObject)
  private
    ApplicationSettings : TWpcPersistentSettings;
    ApplicationStateSettings : TWpcStateSettings;
  public
    constructor Create();
    destructor Destroy(); override;
  private
    procedure initialize();
  end;

implementation

{ TWpcApplication }

constructor TWpcApplication.Create();
begin
  initialize();
end;

destructor TWpcApplication.Destroy();
begin
  ApplicationSettings.Free();
  ApplicationStateSettings.Free();
end;

{
  Loads configs, creates required objects.
}
procedure TWpcApplication.initialize();
begin
  ApplicationSettings := TWpcPersistentSettings.Create(SETTINGS_FILE);
  ApplicationSettings.ReadFromFile();

  ApplicationStateSettings := TWpcStateSettings.Create(STATE_FILE);
  ApplicationStateSettings.ReadFromFile();
end;


end.

