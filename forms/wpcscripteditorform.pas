unit WpcScriptEditorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  SynEdit, SynEditTypes, SynHighlighterAny, SynCompletion,
  Forms, Controls, Graphics, Dialogs, Menus, ComCtrls, ActnList, ExtDlgs, ExtCtrls, StdCtrls,
  Types, LCLType,
  WpcScriptSyntaxHighlighterConfigurrer,
  WpcScriptAutocompletionManager,
  WpcAbstractDynamicScriptAutocompletion,
  WpcScriptParser,
  WpcScript,
  WpcWallpaperStyles,
  WpcScriptExecutor,
  WpcInThreadScriptExecutorLogger,
  WpcInThreadScriptFakeExecutionTracer,
  WpcLogger,
  OSUtils,
  WpcExceptions;

type

  { TWpcScriptEditorLogger }

  TWpcScriptEditorLogger = class(IWpcLogger)
  private
    FLogsConsumer : TMemo;
  public
    constructor Create(LogsConsumer : TMemo);

    procedure LogMessage(Message : String; AddLineBreak : Boolean = True); override;
  end;

  TScriptEditorForm = class;

  TWpcScriptEditorClosedCallback = procedure(ScriptEditorForm : TScriptEditorForm) of Object;

  { TScriptEditorForm }

  TScriptEditorForm = class(TForm)
    ScriptEditorActionList: TActionList;
    ScriptEditorImageList: TImageList;
    ScriptEditorMainMenu: TMainMenu;
    EditorPanel: TPanel;
    ScriptSynEdit: TSynEdit;
    ScriptSynCompletion: TSynCompletion;
    EditorBottomPanelSplitter: TSplitter;
    BottomPanel: TPanel;
    BottomPanelMemo: TMemo;

    ResourceOpenPictureDialog: TOpenPictureDialog;
    ScriptOpenDialog: TOpenDialog;
    ScriptSaveDialog: TSaveDialog;
    ResourceSelectDirectoryDialog: TSelectDirectoryDialog;
    SelectScriptEditorFontFontDialog: TFontDialog;
    FindInScriptFindDialog: TFindDialog;
    FindReplaceInScriptReplaceDialog: TReplaceDialog;

    StatementInsertPropertyInteractiveAction: TAction;
    StatementInsertInteractiveAction: TAction;
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
    StatementInsertTillPropertyAction: TAction;
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
    ToolButton1: TToolButton;
    ViewToggleScriptEditorToolbarAction: TAction;
    ViewFontSizeDecreaseAction: TAction;
    ViewFontSizeIncreaseAction: TAction;
    ViewFontSelectAction: TAction;
    ViewToggleBottomPanelAction: TAction;
    CleanBottomPanelOutputAction: TAction;
    ScriptStopAction: TAction;
    ScriptRunLogAction: TAction;
    ScriptCheckResourcesAction: TAction;
    ScriptCheckSyntaxAction: TAction;
    SearchReplaceInScriptAction: TAction;
    SearchInScriptAction: TAction;

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
    ViewMenuItem: TMenuItem;
    ViewScriptEditorToolBarMenuItem: TMenuItem;
    ViewSeparator1MenuItem: TMenuItem;
    ViewFontMenuItem: TMenuItem;
    ViewFontSizeDecreaseMenuItem: TMenuItem;
    ViewFontSizeIncreaseMenuItem: TMenuItem;
    ViewFontSeparator1: TMenuItem;
    ViewFontSelectScriptEditorFontMenuItem: TMenuItem;
    ViewToggleBottomPanelMenuItem: TMenuItem;
    ScriptMenuItem: TMenuItem;
    ScriptCheckSyntaxMenuItem: TMenuItem;
    ScriptCheckResourcesMenuItem: TMenuItem;
    ScriptRunLogMenuItem: TMenuItem;
    ScriptStopMenuItem: TMenuItem;
    ScriptSeparator1MenuItem: TMenuItem;
    SearchInScriptMenuItem: TMenuItem;
    SearchAndReplaceinScriptMenuItem: TMenuItem;
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
    StatementInsertPropertyMenuItem: TMenuItem;
    StatementPropertyTillMenuItem: TMenuItem;
    StatementPropertyWallpaperStyleMenuItem: TMenuItem;
    StatementPropertyTimesMenuItem: TMenuItem;
    StatementPropertyProbabilityMenuItem: TMenuItem;
    StatementSeparator1: TMenuItem;
    StatementPropertyDelayMenuItem: TMenuItem;
    StatementEditMenuItem: TMenuItem;
    StatementToggleInteractiveInsertionMenuItem: TMenuItem;
    StatementInsertInteractivePropertyMenuItem: TMenuItem;
    StatementInsertInteractiveMenuItem: TMenuItem;
    StatementInsertMenuItem: TMenuItem;
    ResourceInsertDirectoryMenuItem: TMenuItem;
    ResourceInsertImageMenuItem: TMenuItem;
    ResourceMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    HelpDocsMenuItem: TMenuItem;
    HelpAboutMenuItem: TMenuItem;

    ScriptEditorToolBar: TToolBar;
    NewScriptToolButton: TToolButton;
    OpenScriptToolButton: TToolButton;
    SaveToolButton: TToolButton;
    ToolButtonSeparator1: TToolButton;
    CheckScriptSyntaxToolButton: TToolButton;
    CheckScriptResourcesToolButton: TToolButton;
    RunScriptInLogModeToolButton: TToolButton;
    ToolButtonSeparator2: TToolButton;
    AddNewBranchToolButton: TToolButton;
    InsertWaitStatementToolButton: TToolButton;
    InsertWallpaperStatementToolButton: TToolButton;
    InsertDirectoryStatementToolButton: TToolButton;
    InsertUseBranchStatementToolButton: TToolButton;
    InsertSwitchBranchStatementToolButton: TToolButton;
    InsertChooseWallpaperStatementToolButton: TToolButton;
    InsertChooseBranchToUseStatementToolButton: TToolButton;
    InsertChooseBranchToSwitchStatementToolButton: TToolButton;
    InsertStopScriptStatementToolButton: TToolButton;
    ToolButtonSeparator3: TToolButton;
    InsertDelayStatementPropertyToolButton: TToolButton;
    InsertProbabilityStatementPropertyToolButton: TToolButton;
    InsertTimesStatementPropertyToolButton: TToolButton;
    InsertWallpaperStyleStatementPropertyToolButton: TToolButton;
    ToolButtonSeparator4: TToolButton;
    InsertImageResourceToolButton: TToolButton;
    InsertDirectoryResourceToolButton: TToolButton;
    ToolButtonSeparator5: TToolButton;
    OpenDocumentationToolButton: TToolButton;

    BottomPanelToolBar: TToolBar;
    HideBottomPanelToolButton: TToolButton;
    ClearBottomPanelToolButton: TToolButton;

    procedure BranchAddActionExecute(Sender: TObject);
    procedure CleanBottomPanelOutputActionExecute(Sender: TObject);
    procedure CloseEditorWindowActionExecute(Sender: TObject);
    procedure FileNewBaseScriptActionExecute(Sender: TObject);
    procedure FileNewBlankScriptActionExecute(Sender: TObject);
    procedure FileNewFullScriptActionExecute(Sender: TObject);
    procedure FileOpenScriptActionExecute(Sender: TObject);
    procedure FileSaveScriptActionExecute(Sender: TObject);
    procedure FileSaveScriptAsActionExecute(Sender: TObject);
    procedure FileToggleReadOnlyActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure HelpAboutActionExecute(Sender: TObject);
    procedure HelpDocumentationActionExecute(Sender: TObject);
    procedure HideBottomPanelToolButtonClick(Sender: TObject);
    procedure ResourceInsertDirectoryActionExecute(Sender: TObject);
    procedure ResourceInsertImageActionExecute(Sender: TObject);
    procedure ScriptCheckResourcesActionExecute(Sender: TObject);
    procedure ScriptCheckSyntaxActionExecute(Sender: TObject);
    procedure ScriptRunLogActionExecute(Sender: TObject);
    procedure ScriptStopActionExecute(Sender: TObject);
    procedure SearchInScriptActionExecute(Sender: TObject);
    procedure SearchReplaceInScriptActionExecute(Sender: TObject);
    procedure ScriptSynCompletionCodeCompletion(var Value: string;
      SourceValue: string; var SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    procedure ScriptSynCompletionExecute(Sender: TObject);
    procedure ScriptSynCompletionSearchPosition(var APosition: integer);
    procedure StatementEditActionExecute(Sender: TObject);
    procedure StatementInsertDelayPropertyActionExecute(Sender: TObject);
    procedure StatementInsertInteractiveActionExecute(Sender: TObject);
    procedure StatementInsertProbabilityPropertyActionExecute(Sender: TObject);
    procedure StatementInsertPropertyInteractiveActionExecute(Sender: TObject);
    procedure StatementInsertSetDirectoryActionExecute(Sender: TObject);
    procedure StatementInsertSetWallpaperActionExecute(Sender: TObject);
    procedure StatementInsertStopActionExecute(Sender: TObject);
    procedure StatementInsertSwitchBranchActionExecute(Sender: TObject);
    procedure StatementInsertSwitchBranchChooserActionExecute(Sender: TObject);
    procedure StatementInsertTillPropertyActionExecute(Sender: TObject);
    procedure StatementInsertTimesPropertyActionExecute(Sender: TObject);
    procedure StatementInsertUseBranchActionExecute(Sender: TObject);
    procedure StatementInsertUseBranchChooserActionExecute(Sender: TObject);
    procedure StatementInsertWaitActionExecute(Sender: TObject);
    procedure StatementInsertWallpaperChooserActionExecute(Sender: TObject);
    procedure StatementInsertWallpaperStylePropertyActionExecute(Sender: TObject);
    procedure StatementToggleInsertInteractiveActionExecute(Sender: TObject);
    procedure NewScriptToolButtonClick(Sender: TObject);
    procedure ViewToggleBottomPanelActionExecute(Sender: TObject);
    procedure ViewFontSizeDecreaseActionExecute(Sender: TObject);
    procedure ViewFontSizeIncreaseActionExecute(Sender: TObject);
    procedure ViewFontSelectActionExecute(Sender: TObject);
    procedure FindInScriptFindDialogFind(Sender: TObject);
    procedure FindReplaceInScriptReplaceDialogFind(Sender: TObject);
    procedure FindReplaceInScriptReplaceDialogReplace(Sender: TObject);
  const
    SYNEDIT_LINE_BREAK = #10#13;
  const
    DEFAULT_WALLPAPER = 'path';
    DEFAULT_DIRECTORY = 'path';
    DEFAULT_BARNCH_NAME = 'BranchName';
    DEFAULT_SELECTOR = WEIGHT_KEYWORD;
    DEFAULT_DELAY = '5m';
    DEFAULT_WAIT_TILL = 'time';
    DEFAULT_TIMES = '5';
    DEFAULT_PROBABILITY = '50';

    MINIMAL_FONT_SIZE = 6;
    MAXIMAL_FONT_SIZE = 32;
  procedure ViewToggleScriptEditorToolbarActionExecute(Sender: TObject);
  private
    // Path to the file in which the current script is saved. Empty string if none.
    FScriptPath : String;
    // Script under editing
    FCurrentScript : TSynEdit;
    // Whether insertion of script components should be done via UI
    FInteractiveInsertion : Boolean;

    FAutocompleteManager : TWpcScriptAutocompletionManager;

    FScript : TWpcScript;
    FScriptTracer : IWpcScriptExecutor;
    FScriptExecutionLogsConsumer : TWpcScriptEditorLogger;

    FOnCloseCallback : TWpcScriptEditorClosedCallback;
  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy(); override;
  public
    procedure SetOnCloseCallback(Callback : TWpcScriptEditorClosedCallback);

    function OpenScript(PathToScript : String; Line : Integer = 1) : Boolean;
  private
    procedure SetupTracer(); inline;
    procedure SetupUI(); inline;
    procedure ApplyFont(); inline;
  private
    procedure OpenScriptInEditor(PathToScript : String);
  private
    procedure FindInEditor(Editor : TSynEdit; TextToFind : String; Options : TSynSearchOptions);
    procedure ReplaceInEditor(Editor : TSynEdit; TextToFind : String; Replacement : String; Options : TSynSearchOptions);
    function IsAnyOccurrence(Editor : TSynEdit; TextToFind : String; Options : TSynSearchOptions) : Boolean;
    function ReadFindReplaceDialogUIOptions(DialogOptions : TFindOptions) : TSynSearchOptions;
    function IsTextSelected(Editor : TSynEdit) : Boolean; inline;
  private
    function AskSave(Sender : TObject = nil) : Boolean;
    function QuoteIfContainsSpaces(Arg : String) : String; inline;

    procedure ShowBottomPanel(Sender : TObject = nil); inline;
  private
    procedure InsertNewLineIfCurrentNotEmpty();
    function GetLeftIndent(Line : String) : String;
    function CountLeadSpaces(Line : String) : Integer;
    function IsEmptyOrWhitespace(Arg : String) : Boolean;

    procedure CheckScript(CheckResources : Boolean);
    procedure TraceScript();
    procedure OnTraceScriptStopCallback(ExitStatus : TWpcScriptExecutionExitStatus);
    procedure HandleScriptParseError(ParseException : TWpcScriptParseException);
  end;


implementation
uses
  WpcApplication;

{$R *.lfm}

{ TWpcScriptEditorLogger }

constructor TWpcScriptEditorLogger.Create(LogsConsumer : TMemo);
begin
  FLogsConsumer := LogsConsumer;
end;

procedure TWpcScriptEditorLogger.LogMessage(Message : String; AddLineBreak : Boolean);
begin
  FLogsConsumer.Append(Message);
end;

{ TScriptEditorForm }

constructor TScriptEditorForm.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);

  FOnCloseCallback := nil;

  FCurrentScript := ScriptSynEdit;
  FScriptPath := '';
  FInteractiveInsertion := False;

  SetupTracer();

  // Create autocomplete resolver
  FAutocompleteManager := TWpcScriptAutocompletionManager.Create(SACM_BASIC);

   // Set highlighter
  ScriptSynEdit.Highlighter := CreateWallpaperChangerScriptHighlighter(ScriptSynEdit);

  SetupUI();
end;

destructor TScriptEditorForm.Destroy();
begin
  FAutocompleteManager.Free();
  FScriptTracer.Free();
  FScriptExecutionLogsConsumer.Free();

  inherited Destroy();
end;

procedure TScriptEditorForm.SetOnCloseCallback(Callback : TWpcScriptEditorClosedCallback);
begin
  FOnCloseCallback := Callback;
end;

function TScriptEditorForm.OpenScript(PathToScript : String; Line : Integer = 1) : Boolean;
begin
  if (PathToScript = '') then begin
    // Fill editor with basic script template
    FileNewBaseScriptActionExecute(Self);
    Result := True;
    exit;
  end;

  if (FileExists(PathToScript)) then begin
    OpenScriptInEditor(PathToScript);
    FCurrentScript.CaretY := Line;
    Result := True;
  end
  else
    Result := False;
end;

procedure TScriptEditorForm.SetupTracer();
var
  ScriptTracer : TWpcInThreadScriptFakeExecutionTracer;
begin
  FScriptExecutionLogsConsumer := TWpcScriptEditorLogger.Create(BottomPanelMemo);

  ScriptTracer := TWpcInThreadScriptFakeExecutionTracer.Create(FScriptExecutionLogsConsumer);
  ScriptTracer.SetOnStopCallback(@OnTraceScriptStopCallback);
  ScriptTracer.LogDateTime := False;
  ScriptTracer.IndentSymbol := ' ';
  ScriptTracer.TraceLevel := TWpcScriptTraceLevel.STL_STATEMENT;

  FScriptTracer := ScriptTracer;
end;

procedure TScriptEditorForm.SetupUI();
begin
  // Show / hide editor toolbar
  ScriptEditorToolBar.Visible := ApplicationManager.ScriptEditorState.ShowEditorToolBar;
  ViewToggleScriptEditorToolbarAction.Checked := ApplicationManager.ScriptEditorState.ShowEditorToolBar;

  // Hide bottom panel
  EditorBottomPanelSplitter.Visible := False;
  BottomPanel.Visible := False;

  // Set editor font from settings
  ApplyFont();

  // Set window size and position
  Self.Width := ApplicationManager.ScriptEditorState.WindowWidth;
  Self.Height := ApplicationManager.ScriptEditorState.WindowHeight;
  Self.Left := ApplicationManager.ScriptEditorState.WindowPositionLeft;
  Self.Top := ApplicationManager.ScriptEditorState.WindowPositionTop;

  // Disable some actions
  ScriptStopAction.Enabled := False;
end;

procedure TScriptEditorForm.ApplyFont();
begin
  with ScriptSynEdit.Font do begin
    Name := ApplicationManager.ScriptEditorState.FontName;
    Size := ApplicationManager.ScriptEditorState.FontSize;
  end;

  with BottomPanelMemo.Font do begin
    Name := ApplicationManager.ScriptEditorState.FontName;
    Size := ApplicationManager.ScriptEditorState.FontSize;
  end;
end;

{
  Discards current editor content and replaces it with content from given file.
  File by given path should exist.
}
procedure TScriptEditorForm.OpenScriptInEditor(PathToScript : String);
begin
  FScriptPath := PathToScript;
  FCurrentScript.Lines.LoadFromFile(FScriptPath);
  FCurrentScript.Modified := False;
  BottomPanelMemo.Append('Opened file: ' + FScriptPath);
end;

(* Actions *)

// File

procedure TScriptEditorForm.CloseEditorWindowActionExecute(Sender : TObject);
begin
  if (AskSave(Sender)) then exit;

  Close();
end;

procedure TScriptEditorForm.FileNewBlankScriptActionExecute(Sender : TObject);
begin
  if (AskSave(Sender)) then exit;

  FCurrentScript.ClearAll();
  FCurrentScript.Lines.Clear();
  FCurrentScript.Modified := False;

  FScriptPath := '';
  BottomPanelMemo.Append('Created new blank script.');
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

    CaretXY := Point(3, 2);
    Modified := False;
  end;

  FScriptPath := '';
  BottomPanelMemo.Append('Created new script (basic scaffold).');
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

    CaretXY := Point(3, 22);
    Modified := False;
  end;

  FScriptPath := '';
  BottomPanelMemo.Append('Created new script (extended scaffold).');
end;

procedure TScriptEditorForm.FileOpenScriptActionExecute(Sender : TObject);
begin
  if (AskSave(Sender)) then exit;

  if (ScriptOpenDialog.Execute()) then
    OpenScriptInEditor(ScriptOpenDialog.FileName);
end;

procedure TScriptEditorForm.FileSaveScriptActionExecute(Sender : TObject);
begin
  if (FScriptPath <> '') then begin
    FCurrentScript.Lines.SaveToFile(FScriptPath);
    FCurrentScript.Modified := False;
    BottomPanelMemo.Append('Changes saved.');
  end
  else
    FileSaveScriptAsActionExecute(Sender);
end;

procedure TScriptEditorForm.FileSaveScriptAsActionExecute(Sender : TObject);
begin
  if (ScriptSaveDialog.Execute()) then begin
    FScriptPath := ScriptSaveDialog.FileName;
    FCurrentScript.Lines.SaveToFile(FScriptPath);
    FCurrentScript.Modified := False;
    BottomPanelMemo.Append('Saved current script into: ' + FScriptPath);
  end
end;

procedure TScriptEditorForm.FileToggleReadOnlyActionExecute(Sender : TObject);
var
  IsReadOnly : Boolean;
begin
  IsReadOnly := not FileToggleReadOnlyAction.Checked;

  FileToggleReadOnlyAction.Checked := IsReadOnly;
  FCurrentScript.ReadOnly := IsReadOnly;
  BranchMenuItem.Enabled := not IsReadOnly;
  StatementMenuItem.Enabled := not IsReadOnly;
  ResourceMenuItem.Enabled := not IsReadOnly;
end;

// View

procedure TScriptEditorForm.ViewToggleScriptEditorToolbarActionExecute(Sender : TObject);
begin
  ScriptEditorToolBar.Visible := not ScriptEditorToolBar.Visible;
  ViewToggleScriptEditorToolbarAction.Checked := not ViewToggleScriptEditorToolbarAction.Checked;
end;

procedure TScriptEditorForm.ViewToggleBottomPanelActionExecute(Sender: TObject);
begin
  BottomPanel.Visible := not BottomPanel.Visible;
  ViewToggleBottomPanelAction.Checked := not ViewToggleBottomPanelAction.Checked;
  EditorBottomPanelSplitter.Visible := not EditorBottomPanelSplitter.Visible;
end;

procedure TScriptEditorForm.ViewFontSizeIncreaseActionExecute(Sender: TObject);
begin
  if (FCurrentScript.Font.Size < MAXIMAL_FONT_SIZE) then begin
    ApplicationManager.ScriptEditorState.FontSize := ApplicationManager.ScriptEditorState.FontSize + 1;

    FCurrentScript.Font.Size := FCurrentScript.Font.Size + 1;
    BottomPanelMemo.Font.Size := BottomPanelMemo.Font.Size + 1;
  end;
end;

procedure TScriptEditorForm.ViewFontSizeDecreaseActionExecute(Sender: TObject);
begin
  if (FCurrentScript.Font.Size > MINIMAL_FONT_SIZE) then begin
    ApplicationManager.ScriptEditorState.FontSize := ApplicationManager.ScriptEditorState.FontSize -1;

    FCurrentScript.Font.Size := FCurrentScript.Font.Size - 1;
    BottomPanelMemo.Font.Size := BottomPanelMemo.Font.Size - 1;
  end;
end;

procedure TScriptEditorForm.ViewFontSelectActionExecute(Sender : TObject);
begin
  if (SelectScriptEditorFontFontDialog.Execute()) then begin
    ApplicationManager.ScriptEditorState.FontName := SelectScriptEditorFontFontDialog.Font.Name;
    ApplicationManager.ScriptEditorState.FontSize := SelectScriptEditorFontFontDialog.Font.Size;

    ApplyFont();
  end;
end;

procedure TScriptEditorForm.CleanBottomPanelOutputActionExecute(Sender : TObject);
begin
  BottomPanelMemo.Clear();
end;

// Script

procedure TScriptEditorForm.ScriptCheckSyntaxActionExecute(Sender : TObject);
begin
  ShowBottomPanel(Sender);
  CheckScript(False);
end;

procedure TScriptEditorForm.ScriptCheckResourcesActionExecute(Sender : TObject);
begin
  ShowBottomPanel(Sender);
  CheckScript(True);
end;

procedure TScriptEditorForm.ScriptRunLogActionExecute(Sender : TObject);
begin
  ShowBottomPanel(Sender);
  TraceScript();
end;

procedure TScriptEditorForm.ScriptStopActionExecute(Sender : TObject);
begin
   FScriptTracer.Terminate();
end;

// Script: Search / Replace

procedure TScriptEditorForm.SearchInScriptActionExecute(Sender : TObject);
begin
  FindInScriptFindDialog.Execute();
end;

procedure TScriptEditorForm.SearchReplaceInScriptActionExecute(Sender : TObject);
begin
  FindReplaceInScriptReplaceDialog.Execute();
end;

// Branch

procedure TScriptEditorForm.BranchAddActionExecute(Sender : TObject);
var
  BranchName : String;
begin
  BranchName := '';
  if (InputQuery('New branch', 'Branch name: ', BranchName)) then
    if (BranchName <> '') then
      with FCurrentScript do begin
        Append('');
        Append(BRANCH_KEYWORD + ' ' + BranchName);
        Append('  ');
        Append(END_KEYWORD + ' ' + BRANCH_KEYWORD);

        CaretXY := Point(3, Lines.Count - 1);
      end;
end;

// Statement

procedure TScriptEditorForm.StatementInsertWaitActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    InsertNewLineIfCurrentNotEmpty();
    FCurrentScript.InsertTextAtCaret(WAIT_KEYWORD + ' ' + DEFAULT_DELAY);
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertSetDirectoryActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    InsertNewLineIfCurrentNotEmpty();
    FCurrentScript.InsertTextAtCaret(SET_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + FROM_KEYWORD + ' ' + DIRECTORY_KEYWORD + ' ' + DEFAULT_DIRECTORY);
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertSetWallpaperActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    InsertNewLineIfCurrentNotEmpty();
    FCurrentScript.InsertTextAtCaret(SET_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + DEFAULT_WALLPAPER);
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertUseBranchActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    InsertNewLineIfCurrentNotEmpty();
    FCurrentScript.InsertTextAtCaret(USE_KEYWORD + ' ' + BRANCH_KEYWORD + ' ' + DEFAULT_BARNCH_NAME);
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertSwitchBranchActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    InsertNewLineIfCurrentNotEmpty();
    FCurrentScript.InsertTextAtCaret(SWITCH_KEYWORD + ' ' + TO_KEYWORD + ' ' + BRANCH_KEYWORD + ' ' + DEFAULT_BARNCH_NAME);
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertWallpaperChooserActionExecute(Sender : TObject);
var
  Indent : String;
  i      : Integer;
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    Indent := GetLeftIndent(FCurrentScript.LineText);
    InsertNewLineIfCurrentNotEmpty();
    FCurrentScript.TextBetweenPoints[FCurrentScript.CaretXY, FCurrentScript.CaretXY] :=
      CHOOSE_KEYWORD + ' ' + WALLPAPER_KEYWORD + ' ' + BY_KEYWORD + ' ' + DEFAULT_SELECTOR + ' ' + FROM_KEYWORD +
      SYNEDIT_LINE_BREAK +
      Indent + END_KEYWORD + ' ' + CHOOSE_KEYWORD;

    for i:=1 to 3 do
      FCurrentScript.CaretXY := FCurrentScript.NextWordPos();
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertUseBranchChooserActionExecute(Sender : TObject);
var
  Indent : String;
  i      : Integer;
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    Indent := GetLeftIndent(FCurrentScript.LineText);
    InsertNewLineIfCurrentNotEmpty();
    FCurrentScript.TextBetweenPoints[FCurrentScript.CaretXY, FCurrentScript.CaretXY] :=
      CHOOSE_KEYWORD + ' ' + BRANCH_KEYWORD + ' ' + TO_KEYWORD + ' ' + USE_KEYWORD + ' ' + BY_KEYWORD + ' ' + DEFAULT_SELECTOR + ' ' + FROM_KEYWORD +
      SYNEDIT_LINE_BREAK +
      Indent + END_KEYWORD + ' ' + CHOOSE_KEYWORD;

    for i:=1 to 5 do
      FCurrentScript.CaretXY := FCurrentScript.NextWordPos();
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertSwitchBranchChooserActionExecute(Sender : TObject);
var
  Indent : String;
  i      : Integer;
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    Indent := GetLeftIndent(FCurrentScript.LineText);
    InsertNewLineIfCurrentNotEmpty();
    FCurrentScript.TextBetweenPoints[FCurrentScript.CaretXY, FCurrentScript.CaretXY] :=
      CHOOSE_KEYWORD + ' ' + BRANCH_KEYWORD + ' ' + TO_KEYWORD + ' ' + SWITCH_KEYWORD + ' ' + BY_KEYWORD + ' ' + DEFAULT_SELECTOR + ' ' + FROM_KEYWORD +
      SYNEDIT_LINE_BREAK +
      Indent + END_KEYWORD + ' ' + CHOOSE_KEYWORD;

    for i:=1 to 5 do
      FCurrentScript.CaretXY := FCurrentScript.NextWordPos();
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertStopActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    InsertNewLineIfCurrentNotEmpty();
    FCurrentScript.InsertTextAtCaret(STOP_KEYWORD + ' ');
  end;
end;

procedure TScriptEditorForm.StatementInsertDelayPropertyActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    FCurrentScript.InsertTextAtCaret(' ' + FOR_KEYWORD + ' ' + DEFAULT_DELAY);
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertTillPropertyActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    FCurrentScript.InsertTextAtCaret(' ' + TILL_KEYWORD + ' ' + DEFAULT_WAIT_TILL);
    FCurrentScript.SelectWord();
  end;
end;


procedure TScriptEditorForm.StatementInsertProbabilityPropertyActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    FCurrentScript.InsertTextAtCaret(' ' + WITH_KEYWORD + ' ' + PROBABILITY_KEYWORD + ' ' + DEFAULT_PROBABILITY);
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertTimesPropertyActionExecute(Sender : TObject);
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    FCurrentScript.InsertTextAtCaret(' ' + DEFAULT_TIMES + ' ' + TIMES_KEYWORD);
    FCurrentScript.CaretXY := FCurrentScript.PrevWordPos();
    FCurrentScript.CaretXY := FCurrentScript.PrevWordPos();
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementInsertWallpaperStylePropertyActionExecute(Sender : TObject);
var
  WallpaperStyleString : String;
begin
  if (FInteractiveInsertion) then begin
    // TODO
  end
  else begin
    WallpaperStyleString := WallpaperStyleToStr(ApplicationManager.CurrentSettings.WallpaperStyle);
    FCurrentScript.InsertTextAtCaret(' ' + STYLE_KEYWORD + ' ' + WallpaperStyleString);
    FCurrentScript.SelectWord();
  end;
end;

procedure TScriptEditorForm.StatementToggleInsertInteractiveActionExecute(Sender : TObject);
begin
  FInteractiveInsertion := not FInteractiveInsertion;
  StatementToggleInsertInteractiveAction.Checked := not StatementToggleInsertInteractiveAction.Checked;
end;

procedure TScriptEditorForm.StatementInsertInteractiveActionExecute(Sender : TObject);
begin
  // TODO
end;

procedure TScriptEditorForm.StatementInsertPropertyInteractiveActionExecute(Sender : TObject);
begin
  // TODO
end;

procedure TScriptEditorForm.StatementEditActionExecute(Sender : TObject);
begin
  // TODO
end;

// Resource

procedure TScriptEditorForm.ResourceInsertImageActionExecute(Sender : TObject);
begin
  if (ResourceOpenPictureDialog.Execute()) then
    FCurrentScript.InsertTextAtCaret(QuoteIfContainsSpaces(ResourceOpenPictureDialog.FileName));
end;

procedure TScriptEditorForm.ResourceInsertDirectoryActionExecute(Sender : TObject);
begin
  if (ResourceSelectDirectoryDialog.Execute()) then
    FCurrentScript.InsertTextAtCaret(QuoteIfContainsSpaces(ResourceSelectDirectoryDialog.FileName));
end;

// Help

procedure TScriptEditorForm.HelpAboutActionExecute(Sender : TObject);
begin
  Application.MessageBox('Wallpaper Changer Script Editor' + LINE_BREAK +
                         'Version 1.0.0',
                         'About Script Editor',
                         MB_ICONINFORMATION + MB_OK);
end;

procedure TScriptEditorForm.HelpDocumentationActionExecute(Sender : TObject);
begin
  ApplicationManager.DocumentationOpener.OpenDocumentation('_3_script_reference');
end;

(* UI handlers *)

procedure TScriptEditorForm.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
begin
  CanClose := not AskSave(Sender);
end;

procedure TScriptEditorForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ApplicationManager.ScriptEditorState.ShowEditorToolBar := ScriptEditorToolBar.Visible;

  ApplicationManager.ScriptEditorState.WindowWidth := Self.Width;
  ApplicationManager.ScriptEditorState.WindowHeight := Self.Height;
  ApplicationManager.ScriptEditorState.WindowPositionLeft := Self.Left;
  ApplicationManager.ScriptEditorState.WindowPositionTop := Self.Top;

  if (Assigned(FOnCloseCallback)) then
    FOnCloseCallback(Self);

  CloseAction := caFree;
end;

procedure TScriptEditorForm.NewScriptToolButtonClick(Sender : TObject);
begin
  FileNewBaseScriptAction.Execute();
end;

(* Autocomplete *)

procedure TScriptEditorForm.ScriptSynCompletionExecute(Sender : TObject);
var
  CompletionWords : TStrings;
begin
  CompletionWords := FAutocompleteManager.CreateAutocompleteList(
    FCurrentScript.Lines, FCurrentScript.CaretXY, ScriptSynCompletion.CurrentString);
  ScriptSynCompletion.ItemList.Assign(CompletionWords);

  CompletionWords.Free();

  // WORKAROUND for SynEdit bug when Search Position is invoked before Execute
  ScriptSynCompletion.Position := 0;
end;

procedure TScriptEditorForm.ScriptSynCompletionSearchPosition(
  var APosition: integer);
var
  CompletionWords : TStrings;
begin
  CompletionWords := FAutocompleteManager.UpdateAutocompleteList(ScriptSynCompletion.CurrentString);
  ScriptSynCompletion.ItemList.Assign(CompletionWords);
  if (CompletionWords.Count > 0) then
    APosition := 0
  else
    APosition := -1;

  CompletionWords.Free();
end;

procedure TScriptEditorForm.ScriptSynCompletionCodeCompletion(
  var Value: string; SourceValue: string; var SourceStart, SourceEnd: TPoint;
  KeyChar: TUTF8Char; Shift: TShiftState);

  function ShowInsertPictureDialog() : String; inline;
  begin
    if (ResourceOpenPictureDialog.Execute()) then
      Result := QuoteIfContainsSpaces(ResourceOpenPictureDialog.FileName)
    else
      Result := '';
   end;

  function ShowInsertDirectoryDialog() : String; inline;
  begin
    if (ResourceSelectDirectoryDialog.Execute()) then
      Result := QuoteIfContainsSpaces(ResourceSelectDirectoryDialog.FileName)
    else
      Result := '';
  end;

begin
  if (Value = TWpcAbstractDynamicScriptAutocompletion.FILE_PATH_INSERTION) then begin
    Value := ShowInsertPictureDialog();
    exit;
  end;

  if (Value = TWpcAbstractDynamicScriptAutocompletion.DIR_PATH_INSERTION) then begin
    Value := ShowInsertDirectoryDialog();
    exit;
  end;

  if (Value <> SourceValue) then begin
    FCurrentScript.InsertTextAtCaret(' ');
    FCurrentScript.CaretX := FCurrentScript.CaretX + 1;
  end;
end;

// To avoid checked state in toolbar handler is not taken from action list directly.
procedure TScriptEditorForm.HideBottomPanelToolButtonClick(Sender : TObject);
begin
  ViewToggleBottomPanelAction.Execute();
end;

(* Find / Replace *)

procedure TScriptEditorForm.FindInScriptFindDialogFind(Sender : TObject);
begin
  FindInEditor(FCurrentScript,
               FindInScriptFindDialog.FindText,
               ReadFindReplaceDialogUIOptions(FindInScriptFindDialog.Options));
end;

procedure TScriptEditorForm.FindReplaceInScriptReplaceDialogFind(Sender : TObject);
begin
  FindInEditor(FCurrentScript,
               FindReplaceInScriptReplaceDialog.FindText,
               ReadFindReplaceDialogUIOptions(FindReplaceInScriptReplaceDialog.Options));
end;

procedure TScriptEditorForm.FindReplaceInScriptReplaceDialogReplace(Sender : TObject);
begin
  ReplaceInEditor(FCurrentScript,
                  FindReplaceInScriptReplaceDialog.FindText,
                  FindReplaceInScriptReplaceDialog.ReplaceText,
                  [ssoReplace] + ReadFindReplaceDialogUIOptions(FindReplaceInScriptReplaceDialog.Options));
end;

procedure TScriptEditorForm.FindInEditor(Editor : TSynEdit; TextToFind : String; Options : TSynSearchOptions);
var
  SearchResults        : Integer;
  Message              : String;
  Title                : String;
  CurrentCaretPosition : TPoint;
begin
  if (IsTextSelected(Editor)) then begin
    // Set caret to the end of current search result
    // to avoid this result again in case of changed search direction.
    if (ssoBackwards in Options) then
      CurrentCaretPosition := Editor.BlockBegin
    else
      CurrentCaretPosition := Editor.BlockEnd;

    SearchResults := Editor.SearchReplaceEx(TextToFind, '', Options, CurrentCaretPosition);
  end
  else begin
    // No selection in editor
    SearchResults := Editor.SearchReplace(TextToFind, '', Options);
  end;

  if (SearchResults = 0) then begin
    // Nothing was found from previous position

    if (not IsAnyOccurrence(Editor, TextToFind, Options)) then begin
      ShowMessage('"' + TextToFind + '" not found in script.');
      exit;
    end;

    if (ssoBackwards in Options) then begin
      Message := 'Do you want to start search from the end?';
      Title := 'Beginning of script reached';
      CurrentCaretPosition := Point(Editor.Lines[Editor.Lines.Count - 1].Length , Editor.Lines.Count);
    end
    else begin
      Message := 'Do you want to start search from beginning?';
      Title := 'End of script reached';
      CurrentCaretPosition := Point(1,1);
    end;

    if (Application.MessageBox(PChar(Message), PChar(Title), MB_ICONQUESTION + MB_YESNO) = IDYES) then begin
      // A result is always present because of the check at the block beginning
      Editor.SearchReplaceEx(TextToFind, '', Options, CurrentCaretPosition);
    end;
  end;
end;

procedure TScriptEditorForm.ReplaceInEditor(Editor : TSynEdit; TextToFind : String; Replacement : String; Options : TSynSearchOptions);
var
  SearchResults        : Integer;
  CurrentCaretPosition : TPoint;
begin
  // Handle replace all
  if (ssoReplaceAll in Options) then begin
    SearchResults := Editor.SearchReplace(TextToFind, Replacement, Options + [ssoEntireScope]);

    if (SearchResults > 0) then
      FindReplaceInScriptReplaceDialog.CloseDialog()
    else
      ShowMessage('No matches found for string "' + TextToFind + '".');

    exit;
  end;

  if (IsTextSelected(Editor)) then begin
    // Text selected, replace it
    if (ssoBackwards in options) then
      CurrentCaretPosition := Editor.BlockEnd
    else
      CurrentCaretPosition := Editor.BlockBegin;

    Editor.SearchReplaceEx(TextToFind, Replacement, Options, CurrentCaretPosition);
  end;

  // Find next occurrence
  FindInEditor(Editor, TextToFind, Options - [ssoReplace]);
end;

{
  Chacks if given text is present in given editor.
  Text search respects given search option.
  Do not change current position of caret in editor.
}
function TScriptEditorForm.IsAnyOccurrence(Editor : TSynEdit; TextToFind : String; Options : TSynSearchOptions) : Boolean;
var
  CurrentCaretPosition : TPoint;
begin
  CurrentCaretPosition := Editor.CaretXY;
  if (Editor.SearchReplaceEx(TextToFind, '', Options + [ssoEntireScope], Point(1,1)) = 0) then
    Result := False
  else
    Result := True;

  Editor.CaretXY := CurrentCaretPosition;
end;

{
  Adds selected on UI options into given SynEdit search options.
}
function TScriptEditorForm.ReadFindReplaceDialogUIOptions(DialogOptions : TFindOptions) : TSynSearchOptions;
var
  SearchOptions : TSynSearchOptions;
begin
  SearchOptions := [];

  if (not (frDown in DialogOptions)) then
    Include(SearchOptions, ssoBackwards);
  if (frMatchCase in DialogOptions) then
    Include(SearchOptions, ssoMatchCase);
  if (frWholeWord in DialogOptions) then
    Include(SearchOptions, ssoWholeWord);
  if (frReplaceAll in DialogOptions) then
    Include(SearchOptions, ssoReplaceAll);

  Result := SearchOptions;
end;

function TScriptEditorForm.IsTextSelected(Editor : TSynEdit) : Boolean;
begin
  Result := Editor.BlockBegin <> Editor.BlockEnd;
end;

(* Helpers *)

{
  Checks whether current script is saved and if no asks user and save it if needed.
  Returns true when user cancelled operation, false otherwise.
}
function TScriptEditorForm.AskSave(Sender : TObject = nil) : Boolean;
begin
  if (FCurrentScript.Modified) then
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

function TScriptEditorForm.QuoteIfContainsSpaces(Arg : String) : String; inline;
 begin
   if (Arg.IndexOf(' ') <> -1) then
     Result := QUOTE_SYMBOL + Arg + QUOTE_SYMBOL
   else
     Result := Arg;
 end;

procedure TScriptEditorForm.ShowBottomPanel(Sender : TObject = nil);
begin
  if (not ViewToggleBottomPanelAction.Checked) then
    ViewToggleBottomPanelActionExecute(Sender);
end;

{
  Checks if current line contains meaning text (whitespaces are ignored) and if so
  inserts new line after current and put caret in it.
}
procedure TScriptEditorForm.InsertNewLineIfCurrentNotEmpty();
var
  Indent : Integer;
begin
  if (not IsEmptyOrWhitespace(FCurrentScript.LineText)) then begin
    Indent := CountLeadSpaces(FCurrentScript.LineText);
    FCurrentScript.TextBetweenPoints[Point(FCurrentScript.LineText.Length + 1, FCurrentScript.CaretY),
                                     Point(0, FCurrentScript.CaretY + 1)] := SYNEDIT_LINE_BREAK;
    FCurrentScript.CaretX := Indent + 1;
    FCurrentScript.CaretY := FCurrentScript.CaretY + 1;
  end;
end;

{
  Returns string with number of spaces like in the given one form the left.
}
function TScriptEditorForm.GetLeftIndent(Line : String) : String;
begin
  Result := Copy(Line, 1, CountLeadSpaces(Line));
end;

{
  Counts number of spaces at the given string beginning.
}
function TScriptEditorForm.CountLeadSpaces(Line : String) : Integer;
var
  i : Integer;
begin
  if (Line = '') then begin
    Result := 0;
    exit;
  end;

  i := 1;
  while (Line[i] = ' ') do
    Inc(i);

  Result := i - 1;
end;

function TScriptEditorForm.IsEmptyOrWhitespace(Arg : String) : Boolean;
var
  i : Integer;
begin
  for i:=1 to Length(Arg) do
    if (not (Arg[i] in WHITESPACE_SET)) then begin
      Result := False;
      exit;
    end;
  Result := True;
end;

procedure TScriptEditorForm.CheckScript(CheckResources : Boolean);
var
  ScriptLines   : TStringList;
  ScriptParser  : TWpcScriptParser;
  Script        : TWpcScript;
begin
  Script := nil;
  ScriptLines := TStringList.Create();
  ScriptLines.Assign(FCurrentScript.Lines);
  ScriptParser := TWpcScriptParser.Create(ScriptLines);
  ScriptParser.BasePath := ExtractFilePath(FScriptPath);
  ScriptParser.CheckScriptResources := CheckResources;
  try
    try
      Script := ScriptParser.Parse();
      BottomPanelMemo.Append('Script syntax is OK');
      if (CheckResources) then
         BottomPanelMemo.Append('Script resources is OK');
    except
      on ParseException : TWpcScriptParseException do
        HandleScriptParseError(ParseException);
      on E : Exception do
        BottomPanelMemo.Append('Unxpected error: ' + E.Message);
    end;
  finally
    if (Script <> nil) then Script.Free();
    ScriptParser.Free();
    ScriptLines.Free();
  end;
end;

procedure TScriptEditorForm.TraceScript();
var
  ScriptLines   : TStringList;
  ScriptParser  : TWpcScriptParser;
begin
  ScriptRunLogAction.Enabled := False;
  ScriptStopAction.Enabled := True;

  ScriptLines := TStringList.Create();
  ScriptLines.Assign(FCurrentScript.Lines);
  ScriptParser := TWpcScriptParser.Create(ScriptLines);
  ScriptParser.BasePath := ExtractFilePath(FScriptPath);
  ScriptParser.CheckScriptResources := False;
  try
    try
      FScript := ScriptParser.Parse();
    except
      on ParseException : TWpcScriptParseException do begin
        HandleScriptParseError(ParseException);
        OnTraceScriptStopCallback(SES_TERMINATED);
        exit;
      end;
      on E : Exception do begin
        BottomPanelMemo.Append('Unxpected error: ' + E.Message);
        OnTraceScriptStopCallback(SES_TERMINATED);
        exit;
      end;
    end;
  finally
    ScriptParser.Free();
    ScriptLines.Free();
  end;

  FScriptTracer.RunScript(FScript);
end;

procedure TScriptEditorForm.OnTraceScriptStopCallback(ExitStatus : TWpcScriptExecutionExitStatus);
begin
  if (FScript <> nil) then FreeAndNil(FScript);

  ScriptRunLogAction.Enabled := True;
  ScriptStopAction.Enabled := False;
end;

procedure TScriptEditorForm.HandleScriptParseError(ParseException : TWpcScriptParseException);
var
  i : Integer;
begin
  BottomPanelMemo.Append('Failed to parse script: ');
  BottomPanelMemo.Append('  ' + ParseException.Message);
  if (ParseException.Line <> TWpcScriptParseException.UNKNOWN_LINE) then
    if (ParseException.WordNumer <> TWpcScriptParseException.UNKNOWN_WORD_NUMBER) then
      BottomPanelMemo.Append('  At: line ' + IntToStr(ParseException.Line + 1) +
                                 ', word ' + IntToStr(ParseException.WordNumer + 1))
    else
      BottomPanelMemo.Append('  At line: ' + IntToStr(ParseException.Line + 1));
  BottomPanelMemo.Append('');

  if (ParseException.Line <> TWpcScriptParseException.UNKNOWN_LINE) then begin
    FCurrentScript.CaretXY := Point(1, ParseException.Line + 1);
    if (ParseException.WordNumer <> TWpcScriptParseException.UNKNOWN_WORD_NUMBER) then begin
      for i:=1 to (ParseException.WordNumer + 1) do
        FCurrentScript.CaretXY := FCurrentScript.NextWordPos();
      FCurrentScript.SelectWord();
    end
    else
      FCurrentScript.SelectLine();
  end;
end;


end.

