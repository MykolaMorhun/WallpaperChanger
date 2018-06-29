unit WpcTimeMeasurementUnits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWpcTimeMeasurementUnits = (MILLISECONDS, SECONDS, MINUTES, HOURS, DAYS);

const
  MU_MILLISECONDS_STRING = 'ms';
  MU_SECONDS_STRING = 's';
  MU_MINUTES_STRING = 'm';
  MU_HOURS_STRING = 'h';
  MU_DAYS_STRING = 'd';

function TimeMeasurementUnitToStr(MeasurementUnit : TWpcTimeMeasurementUnits) : String;
function StrToTimeMeasurementUnit(MeasurementUnit : String) : TWpcTimeMeasurementUnits;

implementation

function TimeMeasurementUnitToStr(MeasurementUnit : TWpcTimeMeasurementUnits) : String;
begin
  case (MeasurementUnit) of
    MILLISECONDS: Result := MU_MILLISECONDS_STRING;
    SECONDS:      Result := MU_SECONDS_STRING;
    MINUTES:      Result := MU_MINUTES_STRING;
    HOURS:        Result := MU_HOURS_STRING;
    DAYS:         Result := MU_DAYS_STRING;
  end;
end;

function StrToTimeMeasurementUnit(MeasurementUnit : String) : TWpcTimeMeasurementUnits;
begin
  case (MeasurementUnit) of
    MU_MILLISECONDS_STRING: Result := MILLISECONDS;
    MU_SECONDS_STRING:      Result := SECONDS;
    MU_MINUTES_STRING:      Result := MINUTES;
    MU_HOURS_STRING:        Result := HOURS;
    MU_DAYS_STRING:         Result := DAYS;
  end;
end;

end.

