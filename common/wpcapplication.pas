unit WPCApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  // Singleton. Holds application state and modules objects.

  { TWPCApplication }

  TWPCApplication = class(TObject)
  public
    constructor Create();
    destructor Destroy();
  private
    procedure initialize();
  end;

implementation

{ TWPCApplication }

constructor TWPCApplication.Create();
begin
  initialize();
end;

destructor TWPCApplication.Destroy();
begin

end;

{
  Loads configs, creates required objects.
}
procedure TWPCApplication.initialize();
begin

end;


end.

