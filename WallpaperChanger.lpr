{
  Wallpaper Changer project.
  Copyright (C) 2017-2019  Mykola Morhun

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  The author keeps right to add new or change the list of licenses.
}
program WallpaperChanger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Interfaces,
  Forms, MainForm;

{$R *.res}

begin
  // TODO check application uniqueness.
  RequireDerivedFormResource := True;
  Application.ShowMainForm := False;
  Application.Scaled := True;
  Application.Initialize();
  Application.CreateForm(TBannerForm, WPCBannerForm);
  Application.Run();
end.

