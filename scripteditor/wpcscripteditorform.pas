unit WpcScriptEditorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ActnList, LCLType,
  WpcScriptParser,
  WpcScript;

type

  { TScriptEditorForm }

  TScriptEditorForm = class(TForm)
    ScriptOpenDialog: TOpenDialog;
    ScriptSaveDialog: TSaveDialog;
    StatementInsertStopAction: TAction;
    StatementInsertSwitchBranchChooserAction: TAction;
    StatementInsertUseBranchChooserAction: TAction;
    StatementInsertWallpaperChooserAction: TAction;
    StatementInsertSwitchBranchAction: TAction;
    StatementInsertUseBranchAction: TAction;
    StatementInsertSetDirectoryAction: TAction;
    StatementInsertSetWallpaperAction: TAction;
    StatementInsertWaitAction: TAction;
    StatementInsertWallpaperStylePropertyAction: TAction;
    StatementInsertTimesPropertyAction: TAction;
    StatementInsertProbabilityPropertyAction: TAction;
    StatementInsertDelayPropertyAction: TAction;
    StatementToggleInsertInteractiveAction: TAction;
    CloseEditorWindowAction: TAction;
    StatementEditAction: TAction;
    HelpAboutAction: TAction;
    DialogEditAction: TAction;
    ResourceInsertDirectoryAction: TAction;
    ResourceInsertImageAction: TAction;
    HelpDocumentationAction: TAction;
    BranchAddAction: TAction;
    FileToggleReadOnlyAction: TAction;
    FileSaveScriptAsAction: TAction;
    FileSaveScriptAction: TAction;
    FileOpenScriptAction: TAction;
    FileNewFullScriptAction: TAction;
    FileNewBaseScriptAction: TAction;
    FileNewBlankScriptAction: TAction;
    ScriptStopAction: TAction;
    ScriptRunAction: TAction;
    ScriptRunLogAction: TAction;
    ScriptCheckResourcesAction: TAction;
    ScriptCheckSyntaxAction: TAction;

    FileMenuItem: TMenuItem;
    FileNewMenuItem: TMenuItem;
    FileOpenMenuItem: TMenuItem;
    FileSaveMenuItem: TMenuItem;
    FileSaveAsMenuItem: TMenuItem;
    FileCloseEditorMenuItem: TMenuItem;
    FileNewBlankMenuItem: TMenuItem;
    FileNewBaseMenuItem: TMenuItem;
    FileNewFullMenuItem: TMenuItem;
    FileSeparator1MenuItem: TMenuItem;
    FileReadOnlyMenuItem: TMenuItem;
    FileSeparator2MenuItem: TMenuItem;
    ScriptMenuItem: TMenuItem;
    ScriptStopMenuItem: TMenuItem;
    ScriptRunMenuItem: TMenuItem;
    ScriptRunLogMenuItem: TMenuItem;
    ScriptCheckResourcesMenuItem: TMenuItem;
    ScriptCheckSyntaxMenuItem: TMenuItem;
    BranchMenuItem: TMenuItem;
    BranchAddMenuItem: TMenuItem;
    StatementMenuItem: TMenuItem;
    StatementInsertChooserSwitchBranchMenuItem: TMenuItem;
    StatementInsertChooserUseBranchMenuItem: TMenuItem;
    StatementInsertChooserWallpaperMenuItem: TMenuItem;
    StatementInsertStopMenuItem: TMenuItem;
    StatementInsertChooserMenuItem: TMenuItem;
    StatementInsertSwithcBranchMenuItem: TMenuItem;
    StatementInsertUseBranchMenuItem: TMenuItem;
    StatementInsertDirectoryMenuItem: TMenuItem;
    StatementInsertWallpaperMenuItem: TMenuItem;
    StatementInsertWaitMenuItem: TMenuItem;
    StatementPropertyWallpaperStyleMenuItem: TMenuItem;
    StatementPropertyTimesMenuItem: TMenuItem;
    StatementPropertyProbabilityMenuItem: TMenuItem;
    StatementSeparator1: TMenuItem;
    StatementPropertyDelayMenuItem: TMenuItem;
    StatementEditMenuItem: TMenuItem;
    StatementInsertPropertyMenuItem: TMenuItem;
    StatementInsertDialogMenuItem: TMenuItem;
    StatementInsertMenuItem: TMenuItem;
    ResourceInsertDirectoryMenuItem: TMenuItem;
    ResourceInsertImageMenuItem: TMenuItem;
    ResourceMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    HelpDocsMenuItem: TMenuItem;
    HelpAboutMenuItem: TMenuItem;

    ScriptEditorActionList: TActionList;
    ScriptEditorImageList: TImageList;
    ScriptEditorMainMenu: TMainMenu;
    ScriptEditorStatusBar: TStatusBar;
    ScriptSynEdit: TSynEdit;
    procedure BranchAddActionExecute(Sender: TObject);
    procedure CloseEditorWindowActionExecute(Sender: TObject);
    procedure FileNewBaseScriptActionExecute(Sender: TObject);
    procedure FileNewBlankScriptActionExecute(Sender: TObject);
    procedure FileNewFullScriptActionExecute(Sender: TObject);
    procedure FileOpenScriptActionExecute(Sender: TObject);
    procedure FileSaveScriptActionExecute(Sender: TObject);
    procedure FileSaveScriptAsActionExecute(Sender: TObject);
    procedure FileToggleReadOnlyActionExecute(Sender: TObject);
    procedure HelpAboutActionExecute(Sender: TObject);
    procedure HelpDocumentationActionExecute(Sender: TObject);
    procedure ResourceInsertDirectoryActionExecute(Sender: TObject);
    procedure ResourceInsertImageActionExecute(Sender: TObject);
    procedure ScriptCheckResourcesActionExecute(Sender: TObject);
    procedure ScriptCheckSyntaxActionExecute(Sender: TObject);
    procedure ScriptRunActionExecute(Sender: TObject);
    procedure ScriptRunLogActionExecute(Sender: TObject);
    procedure ScriptStopActionExecute(Sender: TObject);
    procedure ScriptSynEditChange(Sender: TObject);
    procedure StatementEditActionExecute(Sender: TObject);
    procedure StatementInsertSetDirectoryActionExecute(Sender: TObject);
    procedure StatementInsertSetWallpaperActionExecute(Sender: TObject);
    procedure StatementInsertStopActionExecute(Sender: TObject);
    procedure StatementInsertSwitchBranchActionExecute(Sender: TObject);
    procedure StatementInsertSwitchBranchChooserActionExecute(Sender: TObject);
    procedure StatementInsertUseBranchActionExecute(Sender: TObject);
    procedure StatementInsertUseBranchChooserActionExecute(Sender: TObject);
    procedure StatementInsertWaitActionExecute(Sender: TObject);
    procedure StatementInsertWallpaperChooserActionExecute(Sender: TObject);
    procedure StatementToggleInsertInteractiveActionExecute(Sender: TObject);
  private
    // true if any changes were made in the current script
    FIsDirty : Boolean;
    // Path to the file in which the current script is saved. Empty string if none.
    FScriptPath : String;
    // Script under editing
    FCurrentScript : TSynEdit;
  public
    constructor Create(TheOwner : TComponent); override;
  public
    function AskSave(Sender : TObject = nil) : Boolean;
  end;

implementation

{$R *.lfm}

{ TScriptEditorForm }

constructor TScriptEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FCurrentScript := ScriptSynEdit;
  FIsDirty := False;
  FScriptPath := '';

  FileNewBaseScriptActionExecute(Self);
end;

(* Actions *)

// File

procedure TScriptEditorForm.CloseEditorWindowActionExecute(Sender : TObject);
begin
  if (AskSave(Sender)) then exit;

  Hide();
end;

procedure TScriptEditorForm.FileNewBlankScriptActionExecute(Sender : TObject);
begin
  if (AskSave(Sender)) then exit;

  FCurrentScript.ClearAll();
  FCurrentScript.Lines.Clear();

  FIsDirty := False;
  FScriptPath := '';
end;

procedure TScriptEditorForm.FileNewBaseScriptActionExecute(Sender : TObject);
begin
  if (AskSave(Sender)) then exit;

  with FCurrentScript do begin
    ClearAll();

    Lines.Clear();
    Lines.Add(BRANCH_KEYWORD + ' ' + MAIN_BARNCH);
    Lines.Add('  ');
    Lines.Add(END_KEYWORD + ' ' + BRANCH_KEYWORD);
    Lines.Add('');

    CaretX := 3;
    CaretY := 2;
  end;

  FIsDirty := False;
  FScriptPath := '';
end;

procedure TScriptEditorForm.FileNewFullScriptActionExecute(Sender : TObject);
begin
  if (AskSave(Sender)) then exit;

   with FCurrentScript do begin
    ClearAll();

    Lines.Clear();
    Lines.Add(COMMENTARY_SYMBOL + ' Script description');
    Lines.Add('');
    Lines.Add(DIRECTORIES_KEYWORD);
    Lines.Add('  ');
    Lines.Add(END_KEYWORD + ' ' + DIRECTORIES_KEYWORD);
    Lines.Add('');
    Lines.Add(IMAGES_KEYWORD);
    Lines.Add('  ');
    Lines.Add(END_KEYWORD + ' ' + IMAGES_KEYWORD);
    Lines.Add('  ');
    Lines.Add(DELAYS_KEYWORD);
    Lines.Add('  ');
    Lines.Add(END_KEYWORD + ' ' + DELAYS_KEYWORD);
    Lines.Add('');
    Lines.Add(DEFAULTS_KEYWORD);
    Lines.Add('  DELAY 5m');
    Lines.Add('  DELAY UNITS s');
    Lines.Add('  WALLPAPER STYLE CENTER');
    Lines.Add(END_KEYWORD + ' ' + DEFAULTS_KEYWORD);
    Lines.Add('');
    Lines.Add(BRANCH_KEYWORD + ' ' + MAIN_BARNCH);
    Lines.Add('  ');
    Lines.Add(END_KEYWORD + ' ' + BRANCH_KEYWORD);
    Lines.Add('');

    CaretX := 3;
    CaretY := 22;
  end;

  FIsDirty := False;
  FScriptPath := '';
end;

procedure TScriptEditorForm.FileOpenScriptActionExecute(Sender : TObject);
begin
  if (AskSave(Sender)) then exit;

  if (ScriptOpenDialog.Execute()) then begin
    FScriptPath := ScriptOpenDialog.FileName;
    FCurrentScript.Lines.LoadFromFile(FScriptPath);
    FIsDirty := False;
  end;
end;

procedure TScriptEditorForm.FileSaveScriptActionExecute(Sender : TObject);
begin
  if (FScriptPath <> '') then
    FCurrentScript.Lines.SaveToFile(FScriptPath)
  else
    FileSaveScriptAsActionExecute(Sender);
end;

procedure TScriptEditorForm.FileSaveScriptAsActionExecute(Sender : TObject);
begin
  if (ScriptSaveDialog.Execute()) then begin
    FScriptPath := ScriptSaveDialog.FileName;
    ScriptSynEdit.Lines.SaveToFile(FScriptPath);
  end
end;

procedure TScriptEditorForm.FileToggleReadOnlyActionExecute(Sender : TObject);
begin
  // TODO
end;

// Script

procedure TScriptEditorForm.ScriptCheckSyntaxActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.ScriptCheckResourcesActionExecute(Sender : TObject);
begin

end;


procedure TScriptEditorForm.ScriptRunLogActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.ScriptRunActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.ScriptStopActionExecute(Sender : TObject);
begin

end;

// Branch

procedure TScriptEditorForm.BranchAddActionExecute(Sender : TObject);
begin

end;

// Statement

procedure TScriptEditorForm.StatementInsertWaitActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementInsertSetDirectoryActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementInsertSetWallpaperActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementInsertUseBranchActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementInsertSwitchBranchActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementInsertWallpaperChooserActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementInsertUseBranchChooserActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementInsertSwitchBranchChooserActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementInsertStopActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementEditActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.StatementToggleInsertInteractiveActionExecute(Sender : TObject);
begin

end;

// Resource

procedure TScriptEditorForm.ResourceInsertImageActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.ResourceInsertDirectoryActionExecute(Sender : TObject);
begin

end;

// Help

procedure TScriptEditorForm.HelpAboutActionExecute(Sender : TObject);
begin

end;

procedure TScriptEditorForm.HelpDocumentationActionExecute(Sender : TObject);
begin

end;

(* UI handlers *)

procedure TScriptEditorForm.ScriptSynEditChange(Sender: TObject);
begin
  FIsDirty := True;
end;

(* Helpers *)

{
  Checks whether current script is saved and if no asks user and save it if needed.
  Returns true when user cancelled operation, false otherwise.
}
function TScriptEditorForm.AskSave(Sender : TObject = nil) : Boolean;
begin
  if (FIsDirty) then
    case (Application.MessageBox('Would you like to save changes?',
                                 'Unsaved changes',
                                 MB_ICONQUESTION + MB_YESNOCANCEL)) of
      IDYES:
        begin
          FileSaveScriptActionExecute(Sender);
          Result := False;
        end;
      IDNO:
        Result := False;
      IDCANCEL:
        Result := True;
    end
  else
    Result := False;
end;


end.

