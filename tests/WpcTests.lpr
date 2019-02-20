program WpcTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, FpcUnitTestRunner,
  DateTimeTestUtils,

  // Here should be the list of all test suites

  ParserBaseTestCase,
  WaitStatementParsingTest,
  StopStatementParsingTest,
  SetWallpaperStatementParsingTest,
  SetDirectoryStatementParsingTest,
  SwitchBranchStatementParsingTest,
  UseBranchStatementParsingTest,
  ChooserStatementBaseTestCase,
  WallpaperChooserStatementParsingTest,
  SwitchBranchChooserStatementParsingTest,
  UseBranchChooserStatementParsingTest,
  BranchesParsingTest,

  ValuesParsingBaseTest,
  DelayValuesParsingTest,
  ProbabilityValueParsingTest,
  TimesValueParsingTest,

  SelectorValuesBaseTest,
  SeasonValueParsingTest,
  WeightValueParsingTest,
  WeekdayValueParsingTest,
  MonthValueParsingTest,
  DateValuesParsingTest,
  TimeValuesParsingTest,
  DateTimeValuesParsingTest;


{$R *.res}

begin
  Application.Initialize();
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run();
end.

