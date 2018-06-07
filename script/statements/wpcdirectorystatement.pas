unit WpcDirectoryStatement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil,
  WpcBaseStatement,
  WpcStatementProperties,
  WpcScriptCommons,
  WpcImage, WpcDirectory,
  WpcWallpaperStyles,
  WpcExceptions;

const
  IMAGES_MASK = '*.jpg;*.jpeg;*.png;*.gif;*.bmp;';

type

  { TWpcDirectoryStatement }

  TWpcDirectoryStatement = class(IWpcBaseScriptStatement)
  private
      FDirectory   : TWpcDirectory;
      FStyle       : TWallpaperStyle;
      FDelay       : TWpcDelayStatementProperty;
      FTimes       : TWpcTimesStatementProperty;
      FProbability : TWpcProbabilityStatementProperty;
      // Component specific property.
      // If true then list from images will be created and next will return next from the list,
      // If false each next image will be chosen with random and could select the same image as before.
      FIsOrdered   : Boolean;

      FImagesList        : TWpcImagesList;
      FCurrentImageIndex : Integer;
    public
      constructor Create(Directory : TWpcDirectory;
                         IsOrdered : Boolean = False;
                         IsRecursive : Boolean = False);
      destructor Destroy(); override;
    public
      function GetDirectory() : TWpcDirectory;
      function GetOrdered(): Boolean;
      function CountImages() : Integer;

      procedure SetStyle(Style : TWallpaperStyle);
      function GetStyle() : TWallpaperStyle;
      procedure SetDelay(Delay : LongWord);
      function GetDelay() : LongWord;
      procedure SetTimes(Times : LongWord);
      function GetTimes() : LongWord;
      procedure SetProbability(Probability : Byte);
      function GetProbability() : Byte;

      function GetNextImage() : TWpcImage;
      function GetPrevImage() : TWpcImage;

      function GetId() : TWpcStatemetId; override;
    private
      procedure ReadImages(IsRecursive : Boolean);
      procedure ShuffleImages();
    end;

implementation

{ TWpcDirectoryStatement }

constructor TWpcDirectoryStatement.Create(Directory : TWpcDirectory; IsOrdered : Boolean; IsRecursive : Boolean);
begin
  if (Directory = nil) then
    raise TWpcIllegalArgumentException.Create('Directory is mandatory.');
  FDirectory := Directory;
  FIsOrdered := IsOrdered;
  FDelay := TWpcDelayStatementProperty.Create();
  FTimes := TWpcTimesStatementProperty.Create();
  FProbability := TWpcProbabilityStatementProperty.Create();

  FImagesList := TWpcImagesList.Create();
  ReadImages(IsRecursive);
  if (not FIsOrdered) then
    ShuffleImages();
  FCurrentImageIndex := -1;
end;

destructor TWpcDirectoryStatement.Destroy();
var
  Image : TWpcImage;
begin
  FDirectory.Free();
  FDelay.Free();
  FTimes.Free();
  FProbability.Free();

  for Image in FImagesList do
    Image.Free();
  FImagesList.Free();

  inherited Destroy();
end;

function TWpcDirectoryStatement.GetDirectory(): TWpcDirectory;
begin
  Result := FDirectory;
end;

function TWpcDirectoryStatement.GetOrdered(): Boolean;
begin
  Result := FIsOrdered;
end;

{
  Returns number of images in given directory.
}
function TWpcDirectoryStatement.CountImages(): Integer;
begin
  Result := FImagesList.Count;
end;

procedure TWpcDirectoryStatement.SetStyle(Style : TWallpaperStyle);
begin
  FStyle := Style;
end;

function TWpcDirectoryStatement.GetStyle(): TWallpaperStyle;
begin
  Result := FStyle;
end;

procedure TWpcDirectoryStatement.SetDelay(Delay : LongWord);
begin
  FDelay.Delay := Delay;
end;

function TWpcDirectoryStatement.GetDelay(): LongWord;
begin
  Result := FDelay.Delay;
end;

procedure TWpcDirectoryStatement.SetTimes(Times : LongWord);
begin
  FTimes.Times := Times;
end;

function TWpcDirectoryStatement.GetTimes(): LongWord;
begin
  Result := FTimes.Times;
end;

procedure TWpcDirectoryStatement.SetProbability(Probability : Byte);
begin
  FProbability.Probability := Probability;
end;

function TWpcDirectoryStatement.GetProbability(): Byte;
begin
  Result := FProbability.Probability;
end;

{
  Selects next image.
}
function TWpcDirectoryStatement.GetNextImage(): TWpcImage;
begin
  if (FIsOrdered) then begin
    FCurrentImageIndex := (FCurrentImageIndex + 1) mod FImagesList.Count;
    Result := FImagesList[FCurrentImageIndex];
  end
  else
    Result := FImagesList[Random(FImagesList.Count)];
end;

{
  Selects previoous image.
}
function TWpcDirectoryStatement.GetPrevImage(): TWpcImage;
begin
  if (FIsOrdered) then begin
    FCurrentImageIndex := (FCurrentImageIndex - 1 + FImagesList.Count) mod FImagesList.Count;
    Result := FImagesList[FCurrentImageIndex];
  end
  else
    Result := FImagesList[Random(FImagesList.Count)];
end;

function TWpcDirectoryStatement.GetId(): TWpcStatemetId;
begin
  Result := WPC_DIRECTORY_STATEMENT_ID;
end;

procedure TWpcDirectoryStatement.ReadImages(IsRecursive : Boolean);
var
  ImagePath   : String;
  ImagesPaths : TStringList;
begin
  ImagesPaths := TStringList.Create();
  FindAllFiles(ImagesPaths, FDirectory.GetPath(), IMAGES_MASK, IsRecursive);
  for ImagePath in ImagesPaths do
    FImagesList.Add(TWpcImage.Create(ImagePath));
  ImagesPaths.Free();
end;

function ImageRandomComparator(const Item1, Item2 : TWpcImage) : Integer;
begin
  Result := Random(3) - 1; // [-1,0,1]
end;

procedure TWpcDirectoryStatement.ShuffleImages();
begin
  FImagesList.Sort(@ImageRandomComparator);
end;


end.

