unit WpcStatementProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcTimeMeasurementUnits,
  WpcExceptions;

type

  { TWpcDelayStatementProperty }

  TWpcDelayStatementProperty = class(TObject)
  public const
    MAX_DELAY_VALUE = 1000 * 60 * 60 * 24 * 32;
  private
    FDelay: LongWord; // milliseconds
  public
    procedure SetDelay(PDelay : LongWord);
    procedure SetDelay(PDelay : LongWord; MeasurementUnit : TWpcTimeMeasurementUnits);
  public
    property Delay : LongWord read FDelay write SetDelay default 0;
  public
    class function ConvertToMilliseconds(PDelay : LongWord; MeasurementUnit : TWpcTimeMeasurementUnits) : LongWord;
  end;

  { TWpcProbabilityStatementProperty }

  TWpcProbabilityStatementProperty = class(TObject)
  private
    FProbability: Byte;  // percents
    procedure SetProbability(Probability: Byte);
  public
    property Probability : Byte read FProbability write SetProbability default 100;
  end;

  { TWpcTimesStatementProperty }

  TWpcTimesStatementProperty = class(TObject)
  private
    FTimes: LongWord;
    procedure SetTimes(Times: LongWord);
  public
    property Times : LongWord read FTimes write SetTimes default 1;
  end;

  { TWpcWeightStatementProperty }

  TWpcWeightStatementProperty = class(TObject)
  private
    FWeight: LongWord;
    procedure SetWeight(Weight: LongWord);
  public
    property Weight : LongWord read FWeight write SetWeight default 1;
  end;


implementation


{ TWpcDelayStatementProperty }

procedure TWpcDelayStatementProperty.SetDelay(PDelay: LongWord);
begin
  if (PDelay > MAX_DELAY_VALUE) then
    raise TWpcIllegalArgumentException.Create('Too long wait time.');
  FDelay := PDelay;
end;

procedure TWpcDelayStatementProperty.SetDelay(PDelay : LongWord; MeasurementUnit : TWpcTimeMeasurementUnits);
begin
  FDelay := ConvertToMilliseconds(PDelay, MeasurementUnit);
end;

{$Q+}
class function TWpcDelayStatementProperty.ConvertToMilliseconds(PDelay: LongWord; MeasurementUnit: TWpcTimeMeasurementUnits): LongWord;
begin
  try
    case MeasurementUnit of
      MILLISECONDS: Result := PDelay;
      SECONDS:      Result := PDelay * 1000;
      MINUTES:      Result := PDelay * 1000 * 60;
      HOURS:        Result := PDelay * 1000 * 60 * 60;
      DAYS:         Result := PDelay * 1000 * 60 * 60 * 24;
      else
        raise TWpcIllegalArgumentException.Create('Wrong wait time measurement unit.');
    end;
  except
    on EIntOverflow do
     raise TWpcIllegalArgumentException.Create('Delay is too big.');
  end;
end;
{$Q-}

{ TWpcProbabilityStatementProperty }

procedure TWpcProbabilityStatementProperty.SetProbability(Probability: Byte);
begin
  if ((Probability < 0) or (Probability > 100)) then
    raise TWpcIllegalArgumentException.Create('Probability should be in percents.');
  FProbability := Probability;
end;

{ TWpcTimesStatementProperty }

procedure TWpcTimesStatementProperty.SetTimes(Times: LongWord);
begin
  if (Times < 1 ) then
    raise TWpcIllegalArgumentException.Create('Times should be positive number.');
  FTimes:=Times;
end;

{ TWpcWeightStatementProperty }

procedure TWpcWeightStatementProperty.SetWeight(Weight: LongWord);
begin
  if (Weight < 1) then
    raise TWpcIllegalArgumentException.Create('Weight value should be positive.');
  FWeight := Weight;
end;

end.

