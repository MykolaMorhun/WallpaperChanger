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
  private const
    MS_IN_SECOND = 1000;
    MS_IN_MINUTE = 1000 * 60;
    MS_IN_HOUR = 1000 * 60 * 60;
    MS_IN_DAY = 1000 * 60 * 60 * 24;
  private
    FDelay: LongWord; // milliseconds
  public
    procedure SetDelay(PDelay : LongWord);
    procedure SetDelay(PDelay : LongWord; MeasurementUnit : TWpcTimeMeasurementUnits);
  public
    property Delay : LongWord read FDelay write SetDelay default 0;
  public
    class function ConvertToMilliseconds(PDelay : LongWord; PMeasurementUnit : TWpcTimeMeasurementUnits) : LongWord;
    class function ConvertToUnit(PMilliseconds : LongWord; PMeasurementUnit : TWpcTimeMeasurementUnits) : LongWord;
    class procedure ConvertToReadableUnits(PMilliseconds : LongWord; var MsDelay : LongWord; var MeasurementUnit : TWpcTimeMeasurementUnits);
  end;

  { TWpcProbabilityStatementProperty }

  TWpcProbabilityStatementProperty = class(TObject)
  private
    FProbability: Byte; // percents
    procedure SetProbability(Probability: Byte);
  public
    property Probability : Byte read FProbability write SetProbability default 100;
  end;

  { TWpcTimesStatementProperty }

  TWpcTimesStatementProperty = class(TObject)
  public const
    MAX_TIMES = high(LongWord);
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
class function TWpcDelayStatementProperty.ConvertToMilliseconds(PDelay: LongWord; PMeasurementUnit: TWpcTimeMeasurementUnits) : LongWord;
begin
  try
    case (PMeasurementUnit) of
      MILLISECONDS: Result := PDelay;
      SECONDS:      Result := PDelay * MS_IN_SECOND;
      MINUTES:      Result := PDelay * MS_IN_MINUTE;
      HOURS:        Result := PDelay * MS_IN_HOUR;
      DAYS:         Result := PDelay * MS_IN_DAY;
      else
        raise TWpcIllegalArgumentException.Create('Wrong wait time measurement unit.');
    end;
  except
    on EIntOverflow do
     raise TWpcIllegalArgumentException.Create('Delay is too big.');
  end;
end;
{$Q-}

{$Q+}
// This function ignores fraction remainder and rounds to lower value.
class function TWpcDelayStatementProperty.ConvertToUnit(PMilliseconds : LongWord; PMeasurementUnit : TWpcTimeMeasurementUnits) : LongWord;
begin
  case (PMeasurementUnit) of
    MILLISECONDS: Result := PMilliseconds;
    SECONDS:      Result := PMilliseconds div MS_IN_SECOND;
    MINUTES:      Result := PMilliseconds div MS_IN_MINUTE;
    HOURS:        Result := PMilliseconds div MS_IN_HOUR;
    DAYS:         Result := PMilliseconds div MS_IN_DAY;
    else
      raise TWpcIllegalArgumentException.Create('Wrong wait time measurement unit.');
  end;
end;
{$Q-}

class procedure TWpcDelayStatementProperty.ConvertToReadableUnits(PMilliseconds : LongWord; var MsDelay : LongWord; var MeasurementUnit : TWpcTimeMeasurementUnits);
begin
  if ((PMilliseconds div MS_IN_DAY > 0) and (PMilliseconds mod MS_IN_DAY = 0)) then begin
    MeasurementUnit := DAYS;
    MsDelay := PMilliseconds div MS_IN_DAY;
  end
  else if ((PMilliseconds div MS_IN_HOUR > 0) and (PMilliseconds mod MS_IN_HOUR = 0)) then begin
    MeasurementUnit := HOURS;
    MsDelay := PMilliseconds div MS_IN_HOUR;
  end
  else if ((PMilliseconds div MS_IN_MINUTE > 0) and (PMilliseconds mod MS_IN_MINUTE = 0)) then begin
    MeasurementUnit := MINUTES;
    MsDelay := PMilliseconds div MS_IN_MINUTE;
  end
  else if ((PMilliseconds div MS_IN_SECOND > 0) and (PMilliseconds mod MS_IN_SECOND = 0)) then begin
    MeasurementUnit := SECONDS;
    MsDelay := PMilliseconds div MS_IN_SECOND;
  end
  else begin
    MeasurementUnit := MILLISECONDS;
    MsDelay := PMilliseconds;
  end;
end;

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

