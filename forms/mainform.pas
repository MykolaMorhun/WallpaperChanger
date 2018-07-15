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
    Separator3MenuItem: TMenuItem;
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
  private
    function GetTargetFile(Dialog : TFileDialog) : String;
    function WarnScriptIsRunning() : Boolean; inline;
    procedure MenuOnScriptStop(); inline;
    procedure MenuOnScriptStart(); inline;
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

  MenuOnScriptStop();
  BannerForm.Hide();
end;

(* ApplicationManager menu handlers *)

procedure TBannerForm.StopMenuItemClick(Sender : TObject);
begin
  ApplicationManager.StopScript();
  MenuOnScriptStop();
end;

procedure TBannerForm.RunScriptMenuItemClick(Sender : TObject);
var
  ScriptPath : String;
begin
  ScriptPath := GetTargetFile(SelectScriptDialog);
  if (ScriptPath <> '') then begin
    try
      ApplicationManager.RunScript(SelectScriptDialog.FileName);
      MenuOnScriptStart();
    except
      on ParseExcepton : TWpcScriptParseException do
       ShowMessage(ParseExcepton.PrettyMessage);
      on WpcException : TWpcException do
        ShowMessage(Concat('Error: ', WpcException.Message));
    end;
  end;
end;

procedure TBannerForm.ScriptEditorMenuItemClick(Sender : TObject);
begin
  ApplicationManager.OpenScriptEditorForm();
end;

procedure TBannerForm.SetWallpaperDirectoryMenuItemClick(Sender : TObject);
var
  DirectoryPath : String;
  Directory : TWpcDirectory;
begin
  DirectoryPath := GetTargetFile(SelectWallpaperDirectoryDialog);
  if (DirectoryPath <> '') then begin
    Directory := TWpcDirectory.Create(SelectWallpaperDirectoryDialog.FileName);
    try
      ApplicationManager.SetWallpapersFromDirectory(Directory);
    except
      on E : TWpcRuntimeException do begin
        ShowMessage('Specified directory doesn''t contain images.');
        exit;
      end;
    end;

    MenuOnScriptStart();
  end;
end;

procedure TBannerForm.NextWallpaperMenuItemClick(Sender : TObject);
begin
  ApplicationManager.SetNextWallpaper();
end;

procedure TBannerForm.PreviousWallpaperMenuItemClick(Sender : TObject);
begin
  // TODO implement
end;

procedure TBannerForm.SetWallpaperImageMenuItemClick(Sender : TObject);
var
  Image : TWpcImage;
begin
  if (SelectWallpaperDialog.Execute()) then begin
    Image := TWpcImage.Create(SelectWallpaperDialog.FileName);
    try
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

(* Helpers *)

{
  Shows Select dialog to user to choose required item.
  If another script is running user will be asked about it termination or this action cancelation first.
  Returns:
    path to item (Dialog.FileName) - if new action should be started (and terminates current script if needed)
    empty string - if user canceled termination of current script or canceled choose dialog
}
function TBannerForm.GetTargetFile(Dialog: TFileDialog): String;
var
  ShouldTerminateCurrentScript : Boolean;
begin
  ShouldTerminateCurrentScript := False;
  if (ApplicationManager.IsScriptRunning()) then
    if (WarnScriptIsRunning()) then
      ShouldTerminateCurrentScript := True
    else begin
      Result := '';
      exit;
    end;

  if (Dialog.Execute()) then begin
    if (ShouldTerminateCurrentScript) then begin
      ApplicationManager.StopScript();
      MenuOnScriptStop();
    end;
    Result := Dialog.FileName;
  end
  else
    Result := '';
end;

{
  Shows warning to user that another script is already running and asks about replacing it.
  Returns true if user wants to replace current script and false otherwise.
}
function TBannerForm.WarnScriptIsRunning() : Boolean;
begin
  Result := IDYES =
    Application.MessageBox('Another script is already running. Would you like to replace it?',
                           'Conflict',
                           MB_ICONQUESTION + MB_YESNO);
end;

procedure TBannerForm.MenuOnScriptStop();
begin
  StopMenuItem.Enabled := False;
  NextWallpaperMenuItem.Enabled := False;
end;

procedure TBannerForm.MenuOnScriptStart();
begin
  StopMenuItem.Enabled := True;
  NextWallpaperMenuItem.Enabled := True;
end;


end.

