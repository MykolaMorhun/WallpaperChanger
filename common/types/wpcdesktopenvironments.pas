unit WpcDesktopEnvironments;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  DE_WINDOWS_2000_ID = 'WINDOWS_2000';
  DE_WINDOWS_XP_ID = 'WINDOWS_XP';
  DE_WINDOWS_VISTA_ID = 'WINDOWS_VISTA';
  DE_WINDOWS_7_ID = 'WINDOWS_7';
  DE_WINDOWS_8_ID = 'WINDOWS_8';
  DE_WINDOWS_8_1_ID = 'WINDOWS_8.1';
  DE_WINDOWS_10_ID = 'WINDOWS_10';
  DE_REACTOS_ID = 'REACTOS';

  DE_GNOME_ID = 'GNOME';
  DE_GNOME_CLASSIC_ID = 'GNOME-CLASSIC';
  DE_MATE_ID = 'MATE';
  DE_CINNAMON_ID = 'CINNAMON';
  DE_KDE_ID = 'KDE';
  DE_XFCE_ID = 'XFCE';
  DE_LXDE_ID = 'LXDE';
  DE_LXQT_ID = 'LXQT';
  DE_UNITY_ID = 'UNITY';
  DE_PANTHEON_ID = 'PANTHEON';
  DE_ENLIGHTENMENT_ID = 'ENLIGHTENMENT';
  DE_BUDGIE_ID = 'BUDGIE';
  DE_DEEPIN_ID = 'DEEPIN';

  DE_CUSTOM_ID = 'CUSTOM';
  DE_AUTODETECT_ID = 'AUTODETECT';
  DE_UNKNOWN_ID = 'UNKNOWN';

type

  TDesktopEnvironment = (
    DE_WINDOWS_2000,
    DE_WINDOWS_XP,
    DE_WINDOWS_VISTA,
    DE_WINDOWS_7,
    DE_WINDOWS_8,
    DE_WINDOWS_8_1,
    DE_WINDOWS_10,
    DE_REACTOS,

    DE_GNOME,
    DE_GNOME_CLASSIC,
    DE_MATE,
    DE_CINNAMON,
    DE_KDE,
    DE_XFCE,
    DE_LXDE,
    DE_LXQT,
    DE_UNITY,
    DE_PANTHEON,
    DE_ENLIGHTENMENT,
    DE_BUDGIE,
    DE_DEEPIN,

    DE_CUSTOM,
    DE_AUTODETECT,
    DE_UNKNOWN
  );

  TDesktopEnvironmentsSet = Set of TDesktopEnvironment;

function DesktopEnvironmentToStr(DesktopEnvironment : TDesktopEnvironment) : String;
function StrToDesktopEnvironment(DesktopEnvironment : String) : TDesktopEnvironment;

implementation

function DesktopEnvironmentToStr(DesktopEnvironment : TDesktopEnvironment) : String;
begin
  case (DesktopEnvironment) of
    DE_WINDOWS_2000:     Result := DE_WINDOWS_2000_ID;
    DE_WINDOWS_XP:       Result := DE_WINDOWS_XP_ID;
    DE_WINDOWS_VISTA:    Result := DE_WINDOWS_VISTA_ID;
    DE_WINDOWS_7:        Result := DE_WINDOWS_7_ID;
    DE_WINDOWS_8:        Result := DE_WINDOWS_8_ID;
    DE_WINDOWS_8_1:      Result := DE_WINDOWS_8_1_ID;
    DE_WINDOWS_10:       Result := DE_WINDOWS_10_ID;
    DE_REACTOS:          Result := DE_REACTOS_ID;

    DE_GNOME:            Result := DE_GNOME_ID;
    DE_GNOME_CLASSIC:    Result := DE_GNOME_CLASSIC_ID;
    DE_MATE:             Result := DE_MATE_ID;
    DE_CINNAMON:         Result := DE_CINNAMON_ID;
    DE_KDE:              Result := DE_KDE_ID;
    DE_XFCE:             Result := DE_XFCE_ID;
    DE_LXDE:             Result := DE_LXDE_ID;
    DE_LXQT:             Result := DE_LXQT_ID;
    DE_UNITY:            Result := DE_UNITY_ID;
    DE_PANTHEON:         Result := DE_PANTHEON_ID;
    DE_ENLIGHTENMENT:    Result := DE_ENLIGHTENMENT_ID;
    DE_BUDGIE:           Result := DE_BUDGIE_ID;
    DE_DEEPIN:           Result := DE_DEEPIN_ID;

    DE_CUSTOM:           Result := DE_CUSTOM_ID;
    DE_AUTODETECT:       Result := DE_AUTODETECT_ID;
    else
      Result := DE_UNKNOWN_ID;
  end;
end;

function StrToDesktopEnvironment(DesktopEnvironment : String): TDesktopEnvironment;
begin
  case (UpperCase(DesktopEnvironment)) of
    DE_WINDOWS_2000_ID:     Result := DE_WINDOWS_2000;
    DE_WINDOWS_XP_ID:       Result := DE_WINDOWS_XP;
    DE_WINDOWS_VISTA_ID:    Result := DE_WINDOWS_VISTA;
    DE_WINDOWS_7_ID:        Result := DE_WINDOWS_7;
    DE_WINDOWS_8_ID:        Result := DE_WINDOWS_8;
    DE_WINDOWS_8_1_ID:      Result := DE_WINDOWS_8_1;
    DE_WINDOWS_10_ID:       Result := DE_WINDOWS_10;
    DE_REACTOS_ID:          Result := DE_REACTOS;

    DE_GNOME_ID:            Result := DE_GNOME;
    DE_GNOME_CLASSIC_ID:    Result := DE_GNOME_CLASSIC;
    DE_MATE_ID:             Result := DE_MATE;
    DE_CINNAMON_ID:         Result := DE_CINNAMON;
    DE_KDE_ID:              Result := DE_KDE;
    DE_XFCE_ID:             Result := DE_XFCE;
    DE_LXDE_ID:             Result := DE_LXDE;
    DE_LXQT_ID:             Result := DE_LXQT;
    DE_UNITY_ID:            Result := DE_UNITY;
    DE_PANTHEON_ID:         Result := DE_PANTHEON;
    DE_ENLIGHTENMENT_ID:    Result := DE_ENLIGHTENMENT;
    DE_BUDGIE_ID:           Result := DE_BUDGIE;
    DE_DEEPIN_ID:           Result := DE_DEEPIN;

    DE_CUSTOM_ID:           Result := DE_CUSTOM;
    DE_AUTODETECT_ID:       Result := DE_AUTODETECT;
    else
      Result := DE_UNKNOWN;
  end;
end;


end.

