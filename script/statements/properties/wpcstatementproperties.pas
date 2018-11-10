unit WpcStatementProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcTimeMeasurementUnits,
  WpcTimeUtils,
  WpcExceptions;

type

  { TWpcDelayStatementProperty }

  TWpcDelayStatementProperty = class(TObject)
  public const
    MAX_DELAY_VALUE = 1000 * 60 * 60 * 24 * 32;
  private
    FDelay: LongWord; // milliseconds
  public
    constructor Create();

    procedure SetDelay(PDelay : LongWord);
    procedure SetDelay(PDelay : LongWord; MeasurementUnit : TWpcTimeMeasurementUnits);

    function ToString(Readable : Boolean = false) : String;
  public
    property Delay : LongWord read FDelay write SetDelay;
  end;

  { TWpcProbabilityStatementProperty }

  TWpcProbabilityStatementProperty = class(TObject)
  private
    FProbability: Byte; // percents
    procedure SetProbability(Probability : Byte);
  public
    constructor Create();

    function ToString() : String; override;
  public
    property Probability : Byte read FProbability write SetProbability;
  end;

  { TWpcTimesStatementProperty }

  TWpcTimesStatementProperty = class(TObject)
  public const
    MAX_TIMES = high(LongWord);
  private
    FTimes: LongWord;
    procedure SetTimes(Times : LongWord);
  public
    constructor Create();

    function ToString() : String; override;
  public
    property Times : LongWord read FTimes write SetTimes;
  end;

  { TWpcWeightStatementProperty }

  TWpcWeightStatementProperty = class(TObject)
  private
    FWeight: LongWord;
    procedure SetWeight(Weight : LongWord);
  public
    constructor Create();

    function ToString() : String; override;
  public
    property Weight : LongWord read FWeight write SetWeight;
  end;


implementation


{ TWpcDelayStatementProperty }

constructor TWpcDelayStatementProperty.Create();
begin
  FDelay := 0;
end;

procedure TWpcDelayStatementProperty.SetDelay(PDelay : LongWord);
begin
  if (PDelay > MAX_DELAY_VALUE) then
    raise TWpcIllegalArgumentException.Create('Too long wait time.');
  FDelay := PDelay;
end;

procedure TWpcDelayStatementProperty.SetDelay(PDelay : LongWord; MeasurementUnit : TWpcTimeMeasurementUnits);
begin
  FDelay := ConvertToMilliseconds(PDelay, MeasurementUnit);
end;

function TWpcDelayStatementProperty.ToString(Readable : Boolean): String;
var
  ReadableDelay   : LongWord;
  MeasurementUnit : TWpcTimeMeasurementUnits;
begin
  if (Readable) then begin
    ConvertToReadableUnits(FDelay, ReadableDelay, MeasurementUnit);
    Result := 'delay: ' + IntToStr(ReadableDelay) + ' ' + TimeMeasurementUnitToStr(MeasurementUnit);
  end
  else
    Result := 'delay: ' + IntToStr(FDelay);
end;

{ TWpcProbabilityStatementProperty }

constructor TWpcProbabilityStatementProperty.Create();
begin
  FProbability := 100;
end;

procedure TWpcProbabilityStatementProperty.SetProbability(Probability: Byte);
begin
  if ((Probability < 0) or (Probability > 100)) then
    raise TWpcIllegalArgumentException.Create('Probability should be in percents.');
  FProbability := Probability;
end;

function TWpcProbabilityStatementProperty.ToString() : String;
begin
  Result := 'probability: ' + IntToStr(FProbability) + '%';
end;

{ TWpcTimesStatementProperty }

constructor TWpcTimesStatementProperty.Create();
begin
  FTimes := 1;
end;

procedure TWpcTimesStatementProperty.SetTimes(Times : LongWord);
begin
  if (Times < 1 ) then
    raise TWpcIllegalArgumentException.Create('Times should be positive number.');
  FTimes := Times;
end;

function TWpcTimesStatementProperty.ToString() : String;
begin
  Result := 'times: ' + IntToStr(FTimes);
end;

{ TWpcWeightStatementProperty }

constructor TWpcWeightStatementProperty.Create();
begin
  FWeight := 1;
end;

procedure TWpcWeightStatementProperty.SetWeight(Weight : LongWord);
begin
  if (Weight < 1) then
    raise TWpcIllegalArgumentException.Create('Weight value should be positive.');
  FWeight := Weight;
end;

function TWpcWeightStatementProperty.ToString() : String;
begin
  Result := 'weight: ' + IntToStr(FWeight);
end;


end.

