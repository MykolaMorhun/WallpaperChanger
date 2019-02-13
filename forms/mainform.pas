unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ExtDlgs, LCLType,
  WpcImage, WpcDirectory,
  WpcMainManuActions,
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
    StopOrRerunMenuItem: TMenuItem;
    ScriptMenuItem: TMenuItem;
    RunScriptMenuItem: TMenuItem;
    ScriptEditorMenuItem: TMenuItem;
    WallpaperMenuItem: TMenuItem;
    SetWallpaperDirectoryMenuItem: TMenuItem;
    PreviousWallpaperMenuItem: TMenuItem;
    NextWallpaperMenuItem: TMenuItem;
    SetWallpaperImageMenuItem: TMenuItem;
    Separator1MenuItem: TMenuItem;
    SettingsMenuItem: TMenuItem;
    InfoMenuItem: TMenuItem;
    DocsMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;

    procedure FormCreate(Sender: TObject);

    procedure StopOrRerunMenuItemClick(Sender: TObject);
    procedure RunScriptMenuItemClick(Sender: TObject);
    procedure ScriptEditorMenuItemClick(Sender: TObject);
    procedure SetWallpaperDirectoryMenuItemClick(Sender: TObject);
    procedure NextWallpaperMenuItemClick(Sender: TObject);
    procedure PreviousWallpaperMenuItemClick(Sender: TObject);
    procedure SetWallpaperImageMenuItemClick(Sender: TObject);
    procedure SettingsMenuItemClick(Sender: TObject);
    procedure DocsMenuItemClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure WPCTrayIconClick(Sender: TObject);
  private
    function GetTargetFile(Dialog : TFileDialog; DefaultPath : String = '') : String;
    function WarnScriptIsRunning() : Boolean; inline;
  public
    procedure ShutdownApplication();
    procedure ShowBalloonMessage(Message : String; Title : String = '');

    procedure UpdateUIOnScriptStop(); inline;
    procedure UpdateUIOnScriptStart(); inline;
  end;

var
  WPCBannerForm : TBannerForm;


implementation

uses
  WpcApplication, WpcApplicationManager;

{$R *.lfm}

{ TBannerForm }

(* ApplicationManager initialization *)

procedure TBannerForm.FormCreate(Sender : TObject);
begin
  WPCBannerForm.ShowInTaskBar := stNever;
  WPCTrayIcon.Hint := 'Wallpaper Changer';
  WPCTrayIcon.Show();
  UpdateUIOnScriptStop();

  ApplicationManager := TWpcApplicationManager.Create(Self);

  WPCBannerForm.Hide();
end;

(* Main menu handlers *)

procedure TBannerForm.StopOrRerunMenuItemClick(Sender : TObject);
begin
  if (ApplicationManager.IsScriptRunning()) then begin
    // Stop task
    ApplicationManager.StopScript();
  end
  else begin
    // Rerun last task if any
    ApplicationManager.ReRunLastTask();
  end;
end;

procedure TBannerForm.RunScriptMenuItemClick(Sender : TObject);
var
  ScriptPath : String;
begin
  ScriptPath := GetTargetFile(SelectScriptDialog, ApplicationManager.CurrentState.LastScript);
  if (ScriptPath <> '') then begin
    try
      ApplicationManager.RunScript(SelectScriptDialog.FileName);
    except
      on ParseExcepton : TWpcScriptParseException do
        Application.MessageBox(PChar(ParseExcepton.PrettyMessage),
                               'Error running script',
                               MB_ICONEXCLAMATION + MB_OK);
      on WpcException : TWpcException do
        Application.MessageBox(PChar(Concat('Error: ', WpcException.Message)),
                               'Unknown error',
                               MB_ICONERROR + MB_OK);
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
  DirectoryPath := GetTargetFile(SelectWallpaperDirectoryDialog, ApplicationManager.CurrentState.LastDirectory);
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
        on E : Exception do
          ShowMessage(E.Message);
      end;
    finally
      Image.Free();
    end;
  end;
end;

procedure TBannerForm.SettingsMenuItemClick(Sender : TObject);
begin
  ApplicationManager.OpenOptionsForm();
end;

procedure TBannerForm.DocsMenuItemClick(Sender : TObject);
begin
  if (not ApplicationManager.DocumentationOpener.OpenDocumentation()) then
    ShowMessage('Failed to open documentation.');
end;

procedure TBannerForm.AboutMenuItemClick(Sender : TObject);
begin
  ApplicationManager.OpenAboutForm();
end;

procedure TBannerForm.ExitMenuItemClick(Sender : TObject);
begin
  ShutdownApplication();
end;

(* Tray *)

procedure TBannerForm.WPCTrayIconClick(Sender : TObject);
begin
  case (ApplicationManager.CurrentSettings.TrayClickAction) of
    MM_NO_ACTION:
      exit;
    MM_SHOW_MAIN_MENU:
      WPCMainPopupMenu.PopUp();
    MM_EXIT_ACTION:
      ExitMenuItemClick(Sender);
    MM_OPEN_DOCUMENTATION_ACTION:
      DocsMenuItemClick(Sender);
    MM_OPEN_SETTINGS_ACTION:
      SettingsMenuItemClick(Sender);
    MM_SET_SINGLE_WALLPAPER_ACTION:
      SetWallpaperImageMenuItemClick(Sender);
    MM_SET_DIRECTORY_ACTION:
      SetWallpaperDirectoryMenuItemClick(Sender);
    MM_RUN_SCRIPT_ACTION:
      RunScriptMenuItemClick(Sender);
    MM_OPEN_SCRIPT_EDITOR_ACTION:
      ScriptEditorMenuItemClick(Sender);
    MM_NEXT_WALLPAPER:
      NextWallpaperMenuItemClick(Sender);
    MM_STOP_OR_RERUN_ACTION:
      StopOrRerunMenuItemClick(Sender);
    MM_STOP_ONLY_ACTION:
      if (ApplicationManager.IsScriptRunning()) then
        ApplicationManager.StopScript();
  end;
end;

(* Helpers *)

{
  Shows Select dialog to user to choose required item.
  If another script is running user will be asked about it termination or this action cancelation first.
  Returns:
    path to item (Dialog.FileName) - if new action should be started (and terminates current script if needed)
    empty string - if user canceled termination of current script or canceled choose dialog
}
function TBannerForm.GetTargetFile(Dialog: TFileDialog; DefaultPath : String = '') : String;
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

  if (DefaultPath <> '') then
    Dialog.FileName := DefaultPath;
  if (Dialog.Execute()) then begin
    if (ShouldTerminateCurrentScript) then
      ApplicationManager.StopScript();
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

(* Callbacks and UI updaters *)

procedure TBannerForm.ShutdownApplication();
begin
  ApplicationManager.Free();
  WPCBannerForm.Close();
end;

procedure TBannerForm.ShowBalloonMessage(Message : String; Title : String = '');
begin
  WPCTrayIcon.BalloonTitle := Title;
  WPCTrayIcon.BalloonHint := Message;
  WPCTrayIcon.ShowBalloonHint();
end;

procedure TBannerForm.UpdateUIOnScriptStop();
begin
  StopOrRerunMenuItem.Caption := 'Rerun';
  StopOrRerunMenuItem.ImageIndex := 0;
  NextWallpaperMenuItem.Enabled := False;
end;

procedure TBannerForm.UpdateUIOnScriptStart();
begin
  StopOrRerunMenuItem.Caption := 'Stop';
  StopOrRerunMenuItem.ImageIndex := 1;
  NextWallpaperMenuItem.Enabled := True;
end;


end.

