unit WpcBudgieWallpaperSetter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WpcGnome3WallpaperSetter,
  WpcWallpaperStyles,
  WpcDesktopEnvironments;

type

  { TWpcBudgieWallpaperSetter }

  TWpcBudgieWallpaperSetter = class(TWpcGnome3WallpaperSetter)
  public
    constructor Create();
  end;


implementation

{ TWpcBudgieWallpaperSetter }

constructor TWpcBudgieWallpaperSetter.Create();
begin
  TargetDesktopEnvironment := DE_BUDGIE;
  SupportedStyles := [ TILED, CENTERED, SCALED, STRETCHED, ZOOMED, SPANNED ];
  DefaultWallpaperStyle := ZOOMED;
end;


end.
