unit DateTimeTestUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DateUtils;


function DateStringToDayOfYear(DateString : String) : LongWord;
function TimeStringToSecondOfDay(TimeString : String) : LongWord;
function DateTimeStringToSecondOfYear(DateTimeString : String) : LongWord;

implementation

function DateStringToDayOfYear(DateString : String) : LongWord;
begin
  Result := DayOfTheYear(StrToDate(DateString));
end;

function TimeStringToSecondOfDay(TimeString : String) : LongWord;
begin
  Result := SecondOfTheDay(StrToTime(TimeString));
end;

function DateTimeStringToSecondOfYear(DateTimeString : String) : LongWord;
var
  DateTimeParts : TStringList;
begin
  DateTimeParts := TStringList.Create();
  DateTimeParts.Delimiter := '-';
  DateTimeParts.DelimitedText := DateTimeString;
  try
    Result := (DateStringToDayOfYear(DateTimeParts[0]) - 1) * 24 * 60 * 60 + TimeStringToSecondOfDay(DateTimeParts[1]);
  finally
    DateTimeParts.Free();
  end;
end;

end.

