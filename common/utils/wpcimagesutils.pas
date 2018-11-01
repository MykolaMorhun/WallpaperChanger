unit WpcImagesUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics;

procedure ConvertImageToBmp(OriginalImagePath : String; BmpImagePath : String);


implementation

{
  Converts given by path image to image in bmp format and saves the result by given path.
  If image format is not supported or invalid then FPImageException wil be thrown.
}
procedure ConvertImageToBmp(OriginalImagePath : String; BmpImagePath : String);
var
  Picture : TPicture;
begin
  Picture := TPicture.Create();
  try
    Picture.LoadFromFile(OriginalImagePath);
    // TODO exclude alpha channel from images which doesn't support it.
    Picture.Bitmap.SaveToFile(BmpImagePath);
  finally
    Picture.Free();
  end;
end;


end.

