unit WpcOptionsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLType,
  WpcWallpaperSetterFactory,
  WpcWallpaperSetter,
  WpcDesktopEnvironments,
  WpcWallpaperStyles,
  WpcTimeMeasurementUnits,
  WpcTimeUtils,
  WpcMainManuActions,
  WpcOptions,
  FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons, ExtCtrls,
  StdCtrls, Spin;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    CancelButton: TButton;
    OnApplicationStartComboBox: TComboBox;
    ConstantDelayRadioButton: TRadioButton;
    DefaultsButton: TButton;
    DelayRadioGroupPanel: TPanel;
    OnTrayClickActionComboBox: TComboBox;
    OnApplicationStartLabel: TLabel;
    OkButton: TButton;
    OnTrayClickActionLabel: TLabel;
    OnStartOptionsPanel: TPanel;
    OnTrayClickActionPanel: TPanel;
    SaveButton: TButton;
    VariableDelayRadioButton: TRadioButton;
    WallpaperSetterAutodetectRadioButton: TRadioButton;
    WallpaperSetterAutodetectValueLabel: TLabel;
    WallpaperSetterCustomRadioButton: TRadioButton;
    WallpaperSetterCustomValueOpenDialogButton: TButton;
    ConstantDelayValueUnitsComboBox: TComboBox;
    WallpaperSetterCustomValueEdit: TEdit;
    WallpaperSetterManualRadioButton: TRadioButton;
    WallpaperSetterManualValuePanel: TPanel;
    WallpaperSetterCustomValuePanel: TPanel;
    WallpaperSetterModeRadioGroupPanel: TPanel;
    WallpaperSetterValuesPanel: TPanel;
    WallpaperSetterAutodetectValuePanel: TPanel;
    VariableDelayValueToLabel: TLabel;
    VariableDelayValueFromLabel: TLabel;
    ConstantDelayValueSpinEdit: TSpinEdit;
    VariableDelayValueToSpinEdit: TSpinEdit;
    VariableDelayValueFromSpinEdit: TSpinEdit;
    VariableDelayValueUnitsComboBox: TComboBox;
    KeepOrderCheckBox: TCheckBox;
    DelaysValuesPanel: TPanel;
    ConstantDelayValuePanel: TPanel;
    VariableDelayValuePanel: TPanel;
    RecursiveSearchCheckBox: TCheckBox;
    WallpaperStyleComboBox: TComboBox;
    CustomSetterOpenDialog: TOpenDialog;
    DelaysGroupBox: TGroupBox;
    WallpaperStyleLabel: TLabel;
    WallpaperStylePanel: TPanel;
    WallpaperSetterManualValueComboBox: TComboBox;
    WallpaperSetterGroupBox: TGroupBox;
    OptionsPageControl: TPageControl;
    ButtonsPanel: TPanel;
    EngineTabSheet: TTabSheet;
    SimpleChangerTabSheet: TTabSheet;
    procedure CancelButtonClick(Sender: TObject);
    procedure ConstantDelayRadioButtonChange(Sender: TObject);
    procedure DefaultsButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OkButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure VariableDelayRadioButtonChange(Sender: TObject);
    procedure VariableDelayValueFromSpinEditChange(Sender: TObject);
    procedure VariableDelayValueToSpinEditChange(Sender: TObject);
    procedure WallpaperSetterAutodetectRadioButtonChange(Sender: TObject);
    procedure WallpaperSetterCustomRadioButtonChange(Sender: TObject);
    procedure WallpaperSetterCustomValueEditChange(Sender: TObject);
    procedure WallpaperSetterCustomValueOpenDialogButtonClick(Sender: TObject);
    procedure WallpaperSetterManualRadioButtonChange(Sender: TObject);
    procedure WallpaperSetterManualValueComboBoxChange(Sender: TObject);
  private const
    APP_START_NONE = 'Do nothing';
    APP_START_RUN_LAST_SCRIPT_IF_WAS_TERMINATED = 'Rerun terminated by shutdown script';
    APP_START_ALWAYS_RUN_LAST_SCRIPT = 'Rerun last script';
  public
    constructor Create(TheOwner: TComponent); override;
    procedure ShowModalOptionsForm(ForceSetEnvironment : Boolean = false);

    procedure FillSettingsOnForm();
    function ReadSettingsFromForm() : Boolean;
    procedure ShowInvalidSettingsMessage(); inline;
  private
    procedure InitConstantUI(); inline;
    procedure OneTimeChnageUI(); inline;

    procedure UpdateAllowedWallpaperStylesList(Styles : TWpcSetOfWallpaperStyles);
  private
    procedure SetComboBoxValue(ComboBox : TComboBox; Value : String); inline;
  end;


implementation

uses
  WpcApplication;

{$R *.lfm}

{ TOptionsForm }

constructor TOptionsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  InitConstantUI();
  OneTimeChnageUI();
end;

procedure TOptionsForm.InitConstantUI();
var
  MeasurementUnit       : TWpcTimeMeasurementUnits;
  MeasurementUnitString : String;
  Environment           : TDesktopEnvironment;
  TrayClickAction       : EMainManuActions;
begin
  // Init timeunits list.
  for MeasurementUnit in TWpcTimeMeasurementUnits do begin
    MeasurementUnitString := TimeMeasurementUnitToStr(MeasurementUnit);
    ConstantDelayValueUnitsComboBox.Items.Add(MeasurementUnitString);
    VariableDelayValueUnitsComboBox.Items.Add(MeasurementUnitString);
  end;

  // Init list of avaliable desktop environments on the system.
  for Environment in ApplicationManager.EnvironmentDetector.GetSupportedEnvironments() do
    WallpaperSetterManualValueComboBox.Items.Add(DesktopEnvironmentToStr(Environment));

  OnApplicationStartComboBox.Items.Add(APP_START_NONE);
  OnApplicationStartComboBox.Items.Add(APP_START_RUN_LAST_SCRIPT_IF_WAS_TERMINATED);
  OnApplicationStartComboBox.Items.Add(APP_START_ALWAYS_RUN_LAST_SCRIPT);

  for TrayClickAction in EMainManuActions do
    OnTrayClickActionComboBox.Items.Add(MainManuActionToStr(TrayClickAction));
end;

{
  Prepares UI before first show.
}
procedure TOptionsForm.OneTimeChnageUI();
begin
  WallpaperSetterAutodetectValuePanel.Enabled := True;
  WallpaperSetterManualValuePanel.Enabled := False;
  WallpaperSetterCustomValuePanel.Enabled := False;
  ConstantDelayValuePanel.Enabled := True;
  VariableDelayValuePanel.Enabled := False;
end;

procedure TOptionsForm.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  CloseAction := caHide;
  if (not CancelButton.Enabled) then begin
    // User should configure environment, otherwise future execution has no sense.
    // If user refuses to configure environment - close program.
    Application.MessageBox('Environment should be set for correct work of the program.',
                           'Program will be terminated', MB_ICONEXCLAMATION);
    Application.Terminate();
  end;
end;

{
  Shows options form and fills current settings.
  If ForceSetEnvironment is true, then user will be forced to set desktop environment different from AUTODETECT.
}
procedure TOptionsForm.ShowModalOptionsForm(ForceSetEnvironment : Boolean);
begin
  FillSettingsOnForm();
  if (ForceSetEnvironment) then begin
    Application.MessageBox('Autodetection of your environment failed. Please set it manually or provide custom wallpaper setter.',
                           'Manual Action Needed', MB_ICONWARNING);

    OptionsPageControl.ActivePageIndex := 0; // set Engine tab active
    WallpaperSetterAutodetectRadioButton.Enabled := False;
    WallpaperSetterManualRadioButton.Checked := True;

    CancelButton.Enabled := False;
  end
  else begin
    WallpaperSetterAutodetectRadioButton.Enabled := True;
    CancelButton.Enabled := True;
  end;

  ShowModal();
end;

{
  Sets all fields according to the current settings.
}
procedure TOptionsForm.FillSettingsOnForm();
var
  Settings : TWpcPersistentSettings;
  WallpaperStyle : TWpcWallpaperStyle;

  TimeUnit      : TWpcTimeMeasurementUnits;
  ReadableDelay : LongWord;
begin
  Settings := ApplicationManager.CurrentSettings;

  // Engine tab
  case (Settings.DesktopEnvironment) of
    DE_AUTODETECT:
      WallpaperSetterAutodetectRadioButton.Checked := True;
    DE_CUSTOM:
      WallpaperSetterCustomRadioButton.Checked := True;
    else
      WallpaperSetterManualRadioButton.Checked := True;
  end;

  if (Settings.DesktopEnvironment = DE_AUTODETECT) then
    if (ApplicationManager.WallpaperSetter <> nil) then
      WallpaperSetterAutodetectValueLabel.Caption := DesktopEnvironmentToStr(ApplicationManager.WallpaperSetter.GetEnvironmet())
    else
      WallpaperSetterAutodetectValueLabel.Caption := 'FAILED'
  else
    WallpaperSetterAutodetectValueLabel.Caption := '';

  if ((Settings.DesktopEnvironment <> DE_AUTODETECT) or (Settings.DesktopEnvironment <> DE_CUSTOM)) then
    WallpaperSetterManualValueComboBox.ItemIndex :=
      WallpaperSetterManualValueComboBox.Items.IndexOf(DesktopEnvironmentToStr(Settings.DesktopEnvironment));

  WallpaperSetterCustomValueEdit.Text := Settings.CustomSetter;

  // On application start drop down
  if (Settings.RunLastTaskOnStart) then
    SetComboBoxValue(OnApplicationStartComboBox, APP_START_ALWAYS_RUN_LAST_SCRIPT)
  else if (Settings.RunTerminatedTaskOnStart) then
    SetComboBoxValue(OnApplicationStartComboBox, APP_START_RUN_LAST_SCRIPT_IF_WAS_TERMINATED)
  else
    SetComboBoxValue(OnApplicationStartComboBox, APP_START_NONE);

  // On tray click action
  SetComboBoxValue(OnTrayClickActionComboBox, MainManuActionToStr(Settings.TrayClickAction));

  // Simple Changer tab
  WallpaperStyleComboBox.Items.Clear();
  if (ApplicationManager.WallpaperSetter <> nil) then
    for WallpaperStyle in ApplicationManager.WallpaperSetter.GetWallpaperStylesSupported() do
      WallpaperStyleComboBox.Items.Add(WallpaperStyleToStr(WallpaperStyle));
  if (WallpaperStyleComboBox.Items.IndexOf(WallpaperStyleToStr(Settings.WallpaperStyle)) <> -1) then
    WallpaperStyleComboBox.ItemIndex := WallpaperStyleComboBox.Items.IndexOf(WallpaperStyleToStr(Settings.WallpaperStyle));

  if (Settings.UseConstantDelay) then
    ConstantDelayRadioButton.Checked := True
  else
    VariableDelayRadioButton.Checked := True;

  // Static delay
  ConvertToReadableUnits(Settings.ConstantDelay, ReadableDelay, TimeUnit);
  ConstantDelayValueSpinEdit.Value := ReadableDelay;
  ConstantDelayValueUnitsComboBox.ItemIndex := ConstantDelayValueUnitsComboBox.Items.IndexOf(TimeMeasurementUnitToStr(TimeUnit));

  // Variable delay
  TimeUnit := GetCommonTimeUnit([Settings.MinimalDelay, Settings.MaximalDelay]);
  VariableDelayValueFromSpinEdit.MinValue := 0;
  VariableDelayValueFromSpinEdit.MaxValue := MAXLONG;
  VariableDelayValueFromSpinEdit.Value := ConvertToUnit(Settings.MinimalDelay, TimeUnit);
  VariableDelayValueToSpinEdit.MinValue := 0;
  VariableDelayValueToSpinEdit.MaxValue := MAXLONG;
  VariableDelayValueToSpinEdit.Value := ConvertToUnit(Settings.MaximalDelay, TimeUnit);
  VariableDelayValueUnitsComboBox.ItemIndex := VariableDelayValueUnitsComboBox.Items.IndexOf(TimeMeasurementUnitToStr(TimeUnit));

  KeepOrderCheckBox.Checked := Settings.KeepOrder;
  RecursiveSearchCheckBox.Checked := Settings.SearchInSubdirectories;
end;

{
  Returns true if all settings are valid, felse otherwise.
}
function TOptionsForm.ReadSettingsFromForm() : Boolean;
var
  Settings : TWpcPersistentSettings;
  VariableDelayMeasurementUnit : TWpcTimeMeasurementUnits;
begin
  Settings := ApplicationManager.CurrentSettings;

  // Engine tab
  if (WallpaperSetterAutodetectRadioButton.Checked) then
    Settings.DesktopEnvironment := DE_AUTODETECT
  else if (WallpaperSetterCustomRadioButton.Checked) then
    Settings.DesktopEnvironment := DE_CUSTOM
  else
    // Manual configuration of environment
    if (WallpaperSetterManualValueComboBox.ItemIndex = -1) then begin
      // Save button clicked but environment is not set.
      Result := False;
      exit;
    end
    else
      Settings.DesktopEnvironment := StrToDesktopEnvironment(
        WallpaperSetterManualValueComboBox.Items[WallpaperSetterManualValueComboBox.ItemIndex]);

  Settings.CustomSetter := WallpaperSetterCustomValueEdit.Text;

  // On application start drop down
  case (OnApplicationStartComboBox.Items[OnApplicationStartComboBox.ItemIndex]) of
    APP_START_ALWAYS_RUN_LAST_SCRIPT: begin
      Settings.RunTerminatedTaskOnStart:= False;
      Settings.RunLastTaskOnStart := True;
    end;
    APP_START_RUN_LAST_SCRIPT_IF_WAS_TERMINATED: begin
      Settings.RunTerminatedTaskOnStart:= True;
      Settings.RunLastTaskOnStart := False;
    end;
    APP_START_NONE: begin
      Settings.RunTerminatedTaskOnStart:= False;
      Settings.RunLastTaskOnStart := False;
    end;
  end;

  // On tray click action
  Settings.TrayClickAction := StrToMainMenuAction(OnTrayClickActionComboBox.Items[OnTrayClickActionComboBox.ItemIndex]);

  // Simple Changer tab
  if (WallpaperStyleComboBox.ItemIndex <> -1) then
    Settings.WallpaperStyle := StrToWallpaperStyle(WallpaperStyleComboBox.Items[WallpaperStyleComboBox.ItemIndex]);

  Settings.UseConstantDelay := ConstantDelayRadioButton.Checked;
  Settings.ConstantDelay := ConvertToMilliseconds(
    ConstantDelayValueSpinEdit.Value,
    StrToTimeMeasurementUnit(ConstantDelayValueUnitsComboBox.Items[ConstantDelayValueUnitsComboBox.ItemIndex])
  );
  VariableDelayMeasurementUnit := StrToTimeMeasurementUnit(variableDelayValueUnitsComboBox.Items[VariableDelayValueUnitsComboBox.ItemIndex]);
  Settings.SetMinMaxDelay(
    ConvertToMilliseconds(VariableDelayValueFromSpinEdit.Value, VariableDelayMeasurementUnit),
    ConvertToMilliseconds(VariableDelayValueToSpinEdit.Value, VariableDelayMeasurementUnit)
  );

  Settings.KeepOrder := KeepOrderCheckBox.Checked;
  Settings.SearchInSubdirectories := RecursiveSearchCheckBox.Checked;

  Result := True;
end;

procedure TOptionsForm.ShowInvalidSettingsMessage();
begin
  Application.MessageBox('Some settings have invalid or empty value. Please set correct value.',
                         'Invalid setting value detected', MB_ICONWARNING);
end;

(* Buttons handlers *)

procedure TOptionsForm.DefaultsButtonClick(Sender: TObject);
begin
  ApplicationManager.CurrentSettings.ResetToDefault();
  FillSettingsOnForm();
end;

procedure TOptionsForm.SaveButtonClick(Sender: TObject);
begin
  if (ReadSettingsFromForm()) then
    ApplicationManager.CurrentSettings.SaveIntoFile()
  else
    ShowInvalidSettingsMessage();
end;

procedure TOptionsForm.CancelButtonClick(Sender: TObject);
begin
  Close();
end;

procedure TOptionsForm.OkButtonClick(Sender: TObject);
begin
  if (ReadSettingsFromForm()) then begin
    ApplicationManager.ApplySettings();
    Close();
  end
  else
    ShowInvalidSettingsMessage();
end;

(* UI auto updating handlers *)

procedure TOptionsForm.WallpaperSetterAutodetectRadioButtonChange(Sender : TObject);
  procedure TryDetectDesktopEnvironment();
  var
    DesktopEnvironment : TDesktopEnvironment;
    WallpaperSetter : IWpcWallpaperSetter;
  begin
    DesktopEnvironment := ApplicationManager.EnvironmentDetector.Detect();
    if (DesktopEnvironment = DE_UNKNOWN) then
      WallpaperSetterAutodetectValueLabel.Caption := 'FAILED'
    else begin
      WallpaperSetterAutodetectValueLabel.Caption := DesktopEnvironmentToStr(DesktopEnvironment);

      WallpaperSetter := ApplicationManager.WallpaperSetterFactory.GetWallpaperSetter(DesktopEnvironment);
      UpdateAllowedWallpaperStylesList(WallpaperSetter.GetWallpaperStylesSupported());
      WallpaperSetter.Free();
    end;
  end;

begin
  WallpaperSetterAutodetectValuePanel.Enabled := WallpaperSetterAutodetectRadioButton.Checked;
  if (WallpaperSetterAutodetectRadioButton.Checked) then
    TryDetectDesktopEnvironment();
end;

procedure TOptionsForm.WallpaperSetterManualRadioButtonChange(Sender : TObject);
begin
  WallpaperSetterManualValuePanel.Enabled := WallpaperSetterManualRadioButton.Checked;
  if (WallpaperSetterManualRadioButton.Checked) then begin
    if (WallpaperSetterManualValueComboBox.ItemIndex <> -1) then
      WallpaperSetterManualValueComboBoxChange(Sender)
    else
      WallpaperStyleComboBox.Items.Clear();
  end;
end;

procedure TOptionsForm.WallpaperSetterCustomRadioButtonChange(Sender : TObject);
  procedure AllowAllWallpaperStyles();
  var
    WallpaperStyle : TWpcWallpaperStyle;
    AllStyles : TWpcSetOfWallpaperStyles;
  begin
    AllStyles := [];
    for WallpaperStyle in TWpcWallpaperStyle do
      Include(AllStyles, WallpaperStyle);
    Exclude(AllStyles, UNKNOWN);

    UpdateAllowedWallpaperStylesList(AllStyles);
  end;

begin
  WallpaperSetterCustomValuePanel.Enabled := WallpaperSetterCustomRadioButton.Checked;
  if (WallpaperSetterCustomRadioButton.Checked) then
    AllowAllWallpaperStyles();
end;

procedure TOptionsForm.ConstantDelayRadioButtonChange(Sender : TObject);
begin
  ConstantDelayValuePanel.Enabled := ConstantDelayRadioButton.Checked;
end;

procedure TOptionsForm.VariableDelayRadioButtonChange(Sender : TObject);
begin
  VariableDelayValuePanel.Enabled := VariableDelayRadioButton.Checked;
end;

procedure TOptionsForm.WallpaperSetterCustomValueOpenDialogButtonClick(Sender : TObject);
begin
  if (CustomSetterOpenDialog.Execute()) then begin
    WallpaperSetterCustomValueEdit.Text := CustomSetterOpenDialog.FileName;
  end;
end;

procedure TOptionsForm.WallpaperSetterCustomValueEditChange(Sender: TObject);
begin
  WallpaperSetterCustomValueEdit.Hint := WallpaperSetterCustomValueEdit.Text;
end;

procedure TOptionsForm.WallpaperSetterManualValueComboBoxChange(Sender : TObject);
var
  WallpaperSetter : IWpcWallpaperSetter;
begin
  WallpaperSetter := ApplicationManager.WallpaperSetterFactory.GetWallpaperSetter(
    StrToDesktopEnvironment(WallpaperSetterManualValueComboBox.Items[WallpaperSetterManualValueComboBox.ItemIndex]));

  UpdateAllowedWallpaperStylesList(WallpaperSetter.GetWallpaperStylesSupported());
end;

procedure TOptionsForm.UpdateAllowedWallpaperStylesList(Styles : TWpcSetOfWallpaperStyles);
var
  WallpaperStyle : TWpcWallpaperStyle;
  PreviousStyleString : String;
begin
  if (WallpaperStyleComboBox.ItemIndex <> -1) then
    PreviousStyleString := WallpaperStyleComboBox.Items[WallpaperStyleComboBox.ItemIndex]
  else
    PreviousStyleString := '';

  WallpaperStyleComboBox.Items.Clear();
  for WallpaperStyle in Styles do
    WallpaperStyleComboBox.Items.Add(WallpaperStyleToStr(WallpaperStyle));

  if (WallpaperStyleComboBox.Items.IndexOf(PreviousStyleString) <> -1) then
    WallpaperStyleComboBox.ItemIndex := WallpaperStyleComboBox.Items.IndexOf(PreviousStyleString);
end;

procedure TOptionsForm.SetComboBoxValue(ComboBox : TComboBox; Value : String);
var
  Index : Integer;
begin
  Index := ComboBox.Items.IndexOf(Value);
  if (Index <> -1) then
    ComboBox.ItemIndex := Index;
end;

procedure TOptionsForm.VariableDelayValueFromSpinEditChange(Sender : TObject);
begin
  VariableDelayValueToSpinEdit.MinValue := VariableDelayValueFromSpinEdit.Value;
end;

procedure TOptionsForm.VariableDelayValueToSpinEditChange(Sender : TObject);
begin
  VariableDelayValueFromSpinEdit.MaxValue := VariableDelayValueToSpinEdit.Value;
end;


end.

