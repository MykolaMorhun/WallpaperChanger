unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ExtDlgs, LCLType,
  WpcApplication, WpcApplicationManager,
  WpcImage, WpcDirectory,
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
  BannerForm : TBannerForm;

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
  ApplicationManager.StopScript();
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
var
  Directory : TWpcDirectory;
begin
  if (SelectWallpaperDirectoryDialog.Execute()) then begin
    try
      Directory := TWpcDirectory.Create(SelectWallpaperDirectoryDialog.FileName);
      try
        ApplicationManager.SetWallpapersFromDirectory(Directory);
      except
        on E : TWpcUseErrorException do begin
          // A script is already running
          if (Application.MessageBox('Another script is already running. Would you like to replace it?',
                                     'Conflict',
                                     MB_ICONQUESTION + MB_YESNO) = IDYES) then begin
            ApplicationManager.StopScript();
            ApplicationManager.SetWallpapersFromDirectory(Directory);
          end;
          // else do nothing
        end;
      end;
    finally
      Directory.Free();
    end;
  end;
end;

procedure TBannerForm.NextWallpaperMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.PreviousWallpaperMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.SetWallpaperImageMenuItemClick(Sender : TObject);
var
  Image : TWpcImage;
begin
  if (SelectWallpaperDialog.Execute()) then begin
    try
      Image := TWpcImage.Create(SelectWallpaperDialog.FileName);
      try
        ApplicationManager.SetWallpaper(Image);
      except
        on E : TWpcUseErrorException do
          ShowMessage(E.Message);
      end;
    finally
      Image.Free();
    end;
  end;
end;

procedure TBannerForm.OptionsMenuItemClick(Sender : TObject);
begin
  ApplicationManager.OpenOptionsForm();
end;

procedure TBannerForm.DocsMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.AboutMenuItemClick(Sender : TObject);
begin

end;

procedure TBannerForm.ExitMenuItemClick(Sender : TObject);
begin
  if (ApplicationManager.IsScriptRunning()) then begin
    ApplicationManager.StopScript();
  end;

  BannerForm.Close();
end;


end.

