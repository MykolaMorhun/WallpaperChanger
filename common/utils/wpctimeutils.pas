unit WpcTimeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcTimeMeasurementUnits,
  WpcExceptions;

const
  MS_IN_SECOND = 1000;
  MS_IN_MINUTE = 1000 * 60;
  MS_IN_HOUR   = 1000 * 60 * 60;
  MS_IN_DAY    = 1000 * 60 * 60 * 24;

function ConvertToMilliseconds(PDelay : LongWord; PMeasurementUnit : TWpcTimeMeasurementUnits) : LongWord;
function ConvertToUnit(PMilliseconds : LongWord; PMeasurementUnit : TWpcTimeMeasurementUnits) : LongWord;
procedure ConvertToReadableUnits(PMilliseconds : LongWord; out ReadableDelay : LongWord; out MeasurementUnit : TWpcTimeMeasurementUnits);
function GetCommonTimeUnit(TimeUnits : Array of TWpcTimeMeasurementUnits) : TWpcTimeMeasurementUnits;
function GetCommonTimeUnit(Periods : Array of Integer) : TWpcTimeMeasurementUnits;

implementation

{$Q+}
function ConvertToMilliseconds(PDelay : LongWord; PMeasurementUnit : TWpcTimeMeasurementUnits) : LongWord;
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
function ConvertToUnit(PMilliseconds : LongWord; PMeasurementUnit : TWpcTimeMeasurementUnits) : LongWord;
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

procedure ConvertToReadableUnits(PMilliseconds : LongWord; out ReadableDelay : LongWord; out MeasurementUnit : TWpcTimeMeasurementUnits);
begin
  if ((PMilliseconds div MS_IN_DAY > 0) and (PMilliseconds mod MS_IN_DAY = 0)) then begin
    MeasurementUnit := DAYS;
    ReadableDelay := PMilliseconds div MS_IN_DAY;
  end
  else if ((PMilliseconds div MS_IN_HOUR > 0) and (PMilliseconds mod MS_IN_HOUR = 0)) then begin
    MeasurementUnit := HOURS;
    ReadableDelay := PMilliseconds div MS_IN_HOUR;
  end
  else if ((PMilliseconds div MS_IN_MINUTE > 0) and (PMilliseconds mod MS_IN_MINUTE = 0)) then begin
    MeasurementUnit := MINUTES;
    ReadableDelay := PMilliseconds div MS_IN_MINUTE;
  end
  else if ((PMilliseconds div MS_IN_SECOND > 0) and (PMilliseconds mod MS_IN_SECOND = 0)) then begin
    MeasurementUnit := SECONDS;
    ReadableDelay := PMilliseconds div MS_IN_SECOND;
  end
  else begin
    MeasurementUnit := MILLISECONDS;
    ReadableDelay := PMilliseconds;
  end;
end;

function GetCommonTimeUnit(TimeUnits : Array of TWpcTimeMeasurementUnits) : TWpcTimeMeasurementUnits;
var
  MinimalTimeUnit : TWpcTimeMeasurementUnits;
  TimeUnit        : TWpcTimeMeasurementUnits;
begin
  if (Length(TimeUnits) = 0) then
    raise TWpcIllegalArgumentException.Create('At least one time unit expected.');

  MinimalTimeUnit := TimeUnits[0];
  for TimeUnit in TimeUnits do
    if (MinimalTimeUnit > TimeUnit) then
      MinimalTimeUnit := TimeUnit;

  Result := MinimalTimeUnit;
end;

function GetCommonTimeUnit(Periods : Array of Integer) : TWpcTimeMeasurementUnits;
var
  MinimalTimeUnit : TWpcTimeMeasurementUnits;
  TimeUnit        : TWpcTimeMeasurementUnits;
  Period          : LongWord;
  ConvertedPeriod : LongWord;
begin
  if (Length(Periods) = 0) then
    raise TWpcIllegalArgumentException.Create('At least one time unit expected.');

  MinimalTimeUnit := DAYS; // use maximum by default
  for Period in Periods do begin
    if (Period = 0) then
      continue;
    ConvertToReadableUnits(Period, ConvertedPeriod, TimeUnit);
    if (MinimalTimeUnit > TimeUnit) then
      MinimalTimeUnit := TimeUnit;
  end;

  Result := MinimalTimeUnit;
end;


end.

