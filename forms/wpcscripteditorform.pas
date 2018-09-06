unit WpcScriptEditorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil, SynEdit, SynHighlighterAny, SynCompletion, Forms, Controls,
  Graphics, Dialogs, Menus, ComCtrls, ActnList, ExtDlgs, ExtCtrls, StdCtrls,
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
  WpcExceptions;

type

  { TWpcScriptEditorLogger }

  TWpcScriptEditorLogger = class(IWpcLogger)
  private
    FLogsConsumer : TMemo;
  public
    constructor Create(LogsConsumer : TMemo);

    procedure LogMessage(Message : String; AddLineBreak : Boolean = true); override;
  end;


  { TScriptEditorForm }

  TScriptEditorForm = class(TForm)
    ScriptEditorActionList: TActionList;
    ScriptEditorImageList: TImageList;
    ScriptEditorMainMenu: TMainMenu;
    EditorPanel: TPanel;
    ScriptSynEdit: TSynEdit;
    BottomPanel: TPanel;
    EditorBottomPanelSplitter: TSplitter;
    BottomPanelMemo: TMemo;
    BottomPanelToolBar: TToolBar;
    HideBottomPanelToolButton: TToolButton;
    ClearBottomPanelToolButton: TToolButton;

    ResourceOpenPictureDialog: TOpenPictureDialog;
    ScriptOpenDialog: TOpenDialog;
    ScriptSaveDialog: TSaveDialog;
    ResourceSelectDirectoryDialog: TSelectDirectoryDialog;

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
    ScriptSynCompletion: TSynCompletion;
    ViewFontSizeDecreaseAction: TAction;
    ViewFontSizeIncreaseAction: TAction;
    ViewToggleBottomPanelAction: TAction;
    CleanBottomPanelOutputAction: TAction;
    ScriptStopAction: TAction;
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
    ViewMenuItem: TMenuItem;
    ViewSeparator1MenuItem: TMenuItem;
    ViewFontSizeDecreaseMenuItem: TMenuItem;
    ViewFontSizeIncreaseMenuItem: TMenuItem;
    ViewFontSizeMenuItem: TMenuItem;
    ViewToggleBottomPanelMenuItem: TMenuItem;
    ScriptMenuItem: TMenuItem;
    ScriptStopMenuItem: TMenuItem;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure HelpAboutActionExecute(Sender: TObject);
    procedure HelpDocumentationActionExecute(Sender: TObject);
    procedure ResourceInsertDirectoryActionExecute(Sender: TObject);
    procedure ResourceInsertImageActionExecute(Sender: TObject);
    procedure ScriptCheckResourcesActionExecute(Sender: TObject);
    procedure ScriptCheckSyntaxActionExecute(Sender: TObject);
    procedure ScriptRunLogActionExecute(Sender: TObject);
    procedure ScriptStopActionExecute(Sender: TObject);
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
    procedure StatementInsertTimesPropertyActionExecute(Sender: TObject);
    procedure StatementInsertUseBranchActionExecute(Sender: TObject);
    procedure StatementInsertUseBranchChooserActionExecute(Sender: TObject);
    procedure StatementInsertWaitActionExecute(Sender: TObject);
    procedure StatementInsertWallpaperChooserActionExecute(Sender: TObject);
    procedure StatementInsertWallpaperStylePropertyActionExecute(Sender: TObject);
    procedure StatementToggleInsertInteractiveActionExecute(Sender: TObject);
    procedure ViewFontSizeDecreaseActionExecute(Sender: TObject);
    procedure ViewFontSizeIncreaseActionExecute(Sender: TObject);
    procedure ViewToggleBottomPanelActionExecute(Sender: TObject);
  const
    LINE_BREAK = #10#13;

    DEFAULT_WALLPAPER = 'path';
    DEFAULT_DIRECTORY = 'path';
    DEFAULT_BARNCH_NAME = 'BranchName';
    DEFAULT_SELECTOR = WEIGHT_KEYWORD;
    DEFAULT_DELAY = '5m';
    DEFAULT_TIMES = '5';
    DEFAULT_PROBABILITY = '50';

    MINIMAL_FONT_SIZE = 6;
    MAXIMAL_FONT_SIZE = 32;
  private
    // Path to the file in which the current script is saved. Empty string if none.
    FScriptPath : String;
    // Script under editing
    FCurrentScript : TSynEdit;
    // Determinas if insertion of script components shows UI
    FInteractiveInsertion : Boolean;

    FAutocompleteManager : TWpcScriptAutocompletionManager;

    FScript : TWpcScript;
    FScriptTracer : IWpcScriptExecutor;
    FScriptExecutionLogsConsumer : TWpcScriptEditorLogger;
  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy(); override;
  private
    procedure SetupTracer(); inline;
    procedure SetupUI(); inline;

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
  // Hide bottom panel
  EditorBottomPanelSplitter.Visible := False;
  BottomPanel.Visible := False;

  // Adapt window size to screen resolution
  if ((Screen.Width >= 1024) and (Screen.Height >= 768)) then begin
    Width := 850;
    Height := 620;
  end;

  // Disable some actions
  ScriptStopAction.Enabled := False;

  // Fill editor with basic script template
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
  FCurrentScript.Modified := False;

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

    CaretXY := Point(3, 2);
    Modified := False;
  end;

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

    CaretXY := Point(3, 22);
    Modified := False;
  end;

  FScriptPath := '';
end;

procedure TScriptEditorForm.FileOpenScriptActionExecute(Sender : TObject);
begin
  if (AskSave(Sender)) then exit;

  if (ScriptOpenDialog.Execute()) then begin
    FScriptPath := ScriptOpenDialog.FileName;
    FCurrentScript.Lines.LoadFromFile(FScriptPath);
    FCurrentScript.Modified := False;
  end;
end;

procedure TScriptEditorForm.FileSaveScriptActionExecute(Sender : TObject);
begin
  if (FScriptPath <> '') then begin
    FCurrentScript.Lines.SaveToFile(FScriptPath);
    FCurrentScript.Modified := False;
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

procedure TScriptEditorForm.ViewToggleBottomPanelActionExecute(Sender: TObject);
begin
  BottomPanel.Visible := not BottomPanel.Visible;
  ViewToggleBottomPanelAction.Checked := not ViewToggleBottomPanelAction.Checked;
  EditorBottomPanelSplitter.Visible := not EditorBottomPanelSplitter.Visible;
end;

procedure TScriptEditorForm.ViewFontSizeIncreaseActionExecute(Sender: TObject);
begin
  if (FCurrentScript.Font.Size < MAXIMAL_FONT_SIZE) then
      FCurrentScript.Font.Size := FCurrentScript.Font.Size + 1;
end;

procedure TScriptEditorForm.ViewFontSizeDecreaseActionExecute(Sender: TObject);
begin
  if (FCurrentScript.Font.Size > MINIMAL_FONT_SIZE) then
    FCurrentScript.Font.Size := FCurrentScript.Font.Size - 1;
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
      LINE_BREAK +
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
      LINE_BREAK +
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
      LINE_BREAK +
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
  Application.MessageBox('Wallpaper Changer Script Editor' + sLineBreak +
                         'Version 1.0.0',
                         'About Script Editor',
                         MB_ICONINFORMATION + MB_OK);
end;

procedure TScriptEditorForm.HelpDocumentationActionExecute(Sender : TObject);
begin
  // TODO use app manager
end;

(* UI handlers *)

procedure TScriptEditorForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  CanClose := not AskSave(Sender);
end;

procedure TScriptEditorForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
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
                                     Point(0, FCurrentScript.CaretY + 1)] := LINE_BREAK;
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
  i := 1;
  while (Line[i] = ' ') do
    Inc(i);

  Result := i-1;
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

