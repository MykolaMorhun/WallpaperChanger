unit WpcShowApplicationStartedForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TWpcShowApplicationStartedForm }

  TWpcShowApplicationStartedForm = class(TForm)
    ControlsPanel: TPanel;
    DoNotShowAgainCheckBox: TCheckBox;
    MessageLabel: TLabel;
    MessagesPanel: TPanel;
    OkButton: TButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OkButtonClick(Sender: TObject);
  private

  end;

var
  ShowApplicationStartedForm: TWpcShowApplicationStartedForm;

implementation

uses
  WpcApplication;

{$R *.lfm}

{ TWpcShowApplicationStartedForm }

procedure TWpcShowApplicationStartedForm.OkButtonClick(Sender: TObject);
begin
  Close();
end;

procedure TWpcShowApplicationStartedForm.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  If (DoNotShowAgainCheckBox.Checked) then begin;
    ApplicationManager.CurrentSettings.ShowAppStartedWindow := False;
    ApplicationManager.CurrentSettings.SaveIntoFile();
  end;

  CloseAction := caFree;
end;


end.

