unit WpcAboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TWpcAboutForm }

  TWpcAboutForm = class(TForm)
    CloseButton: TButton;
    AuthorsLabel: TLabel;
    VersionLabel: TLabel;
    WallpaperChangerLabel: TLabel;

    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;


implementation

{$R *.lfm}

{ TWpcAboutForm }

procedure TWpcAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close();
end;

procedure TWpcAboutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;


end.

