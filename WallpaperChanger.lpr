program WallpaperChanger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, WpcAboutForm, WpcKdePlasmaWallpaperSetter;

{$R *.res}

begin
  // TODO check application uniqueness.
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TBannerForm, BannerForm);
  Application.Run;
end.

