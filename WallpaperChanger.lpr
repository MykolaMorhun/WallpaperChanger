program WallpaperChanger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Interfaces,
  Forms, MainForm;

{$R *.res}

begin
  // TODO check application uniqueness.
  RequireDerivedFormResource := True;
  Application.ShowMainForm := False;
  Application.Initialize;
  Application.CreateForm(TBannerForm, WPCBannerForm);
  Application.Run;
end.

