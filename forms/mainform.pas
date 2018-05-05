unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ExtDlgs,
  WpcApplication, WpcApplicationManager,
  WpcExceptions;

type

  { TBannerForm }

  TBannerForm = class(TForm)
    WPCTrayIcon: TTrayIcon;
    SelectScriptDialog: TOpenDialog;
    SelectWallpaperDirectoryDialog: TSelectDirectoryDialog;
    SelectWallpaperDialog: TOpenPictureDialog;
    MainContextMenuImageList: TImageList;
    WPCMainPopupMenu: TPopupMenu;
    StopMenuItem: TMenuItem;
    ScriptMenuItem: TMenuItem;
    RunScriptMenuItem: TMenuItem;
    ScriptEditorMenuItem: TMenuItem;
    WallpaperMenuItem: TMenuItem;
    SetWallpaperDirectoryMenuItem: TMenuItem;
    PreviousWallpaperMenuItem: TMenuItem;
    NextWallpaperMenuItem: TMenuItem;
    Separator2MenuItem: TMenuItem;
    SetWallpaperImageMenuItem: TMenuItem;
    Separator1MenuItem: TMenuItem;
    OptionsMenuItem: TMenuItem;
    InfoMenuItem: TMenuItem;
    DocsMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;

    procedure FormCreate(Sender: TObject);

    procedure StopMenuItemClick(Sender: TObject);
    procedure RunScriptMenuItemClick(Sender: TObject);
    procedure ScriptEditorMenuItemClick(Sender: TObject);
    procedure SetWallpaperDirectoryMenuItemClick(Sender: TObject);
    procedure NextWallpaperMenuItemClick(Sender: TObject);
    procedure PreviousWallpaperMenuItemClick(Sender: TObject);
    procedure SetWallpaperImageMenuItemClick(Sender: TObject);
    procedure OptionsMenuItemClick(Sender: TObject);
    procedure DocsMenuItemClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
  end;

var
  BannerForm: TBannerForm;

implementation

{$R *.lfm}

{ TBannerForm }

(* ApplicationManager initialization *)

procedure TBannerForm.FormCreate(Sender : TObject);
begin
  BannerForm.ShowInTaskBar := stNever;
  WPCTrayIcon.Show();

  ApplicationManager := TWpcApplicationManager.Create();
  ApplicationManager.ApplySettings(); // init app with settings from config file

  BannerForm.Hide();
end;

(* ApplicationManager menu handlers *)

procedure TBannerForm.StopMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.RunScriptMenuItemClick(Sender : TObject);
var
  ErrorMessage: String;
begin
  try
    if (SelectScriptDialog.Execute()) then begin
      ApplicationManager.RunScript(SelectScriptDialog.FileName);
    end;
  except
    on ParseExcepton : TWpcScriptParseException do begin
      ErrorMessage := Concat('Failed to parse script: ', ParseExcepton.Message);
      if (ParseExcepton.Line <> TWpcScriptParseException.UNKNOWN_LINE) then
        ErrorMessage := Concat(ErrorMessage, ' Line: ', IntToStr(ParseExcepton.Line + 1));
      if (ParseExcepton.WordNumer <> TWpcScriptParseException.UNKNOWN_WORD_NUMBER) then
        ErrorMessage := Concat(ErrorMessage, ' Word: ', IntToStr(ParseExcepton.WordNumer + 1));
      ShowMessage(ErrorMessage);
    end;
    on WpcException : TWpcException do begin
      ErrorMessage := Concat('Error: ', WpcException.Message);
      ShowMessage(ErrorMessage);
    end;
  end;
end;

procedure TBannerForm.ScriptEditorMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.SetWallpaperDirectoryMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.NextWallpaperMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.PreviousWallpaperMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.SetWallpaperImageMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.OptionsMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.DocsMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.AboutMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.ExitMenuItemClick(Sender : TObject);
begin
  // TODO stop all jobs
  BannerForm.Close();
end;


end.

