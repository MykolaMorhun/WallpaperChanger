unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls,
  WpcExceptions,
  WpcScript,
  WpcScriptParser,
  WpcScriptExecutor,
  WallpaperSetter;

type

  { TBannerForm }

  TBannerForm = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  BannerForm: TBannerForm;

implementation

{$R *.lfm}

procedure RunScript(PathToScript : String);
var
  ScriptContent   : TStringList;
  ScriptParser    : TWpcScriptParser;
  Script          : TWpcScript;
  ScriptExecutor  : TWpcScriptExecutor;
  WallpaperSetter : IWallpaperSetter;

  ErrorMessage : String;
begin
    try
      try
        ScriptContent := TStringList.Create();
        // TODO check file type and size (limit into property)
        ScriptContent.LoadFromFile(PathToScript);
        ScriptParser := TWpcScriptParser.Create(ScriptContent);
        Script := ScriptParser.Parse();
        WallpaperSetter := TWpcDebugWallpaperSetter.Create('log.txt');
        ScriptExecutor := TWpcScriptExecutor.Create(Script, WallpaperSetter);
        ScriptExecutor.ExecuteScript();
      except
        on ParseExcepton : TWpcScriptParseException do begin
          ErrorMessage := Concat('Failed to parse script: ', ParseExcepton.Message);
          if (ParseExcepton.Line <> TWpcScriptParseException.UNKNOWN_LINE) then
            ErrorMessage := Concat(ErrorMessage, ' Line: ', IntToStr(ParseExcepton.Line + 1));
          if (ParseExcepton.WordNumer <> TWpcScriptParseException.UNKNOWN_WORD_NUMBER) then
            ErrorMessage := Concat(ErrorMessage, ' Word: ', IntToStr(ParseExcepton.WordNumer + 1));
          WriteLn(ErrorMessage);
        end;
        on WpcException : TWpcException do begin
          ErrorMessage := Concat('Error: ', WpcException.Message);
          WriteLn(ErrorMessage);
        end;
        on E : Exception do begin
          ErrorMessage := Concat('Unknown error: ', E.Message);
          WriteLn(ErrorMessage);
        end;
      end;
    finally
      {if (ScriptParser <> nil) then ScriptParser.Free();
      if (ScriptExecutor <> nil) then ScriptExecutor.Free();
      if (WallpaperSetter <> nil) then WallpaperSetter.Free(); }
      ScriptContent.Free();
    end;
end;

{ TBannerForm }

procedure TBannerForm.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute() then
    RunScript(OpenDialog1.Filename);

  WriteLn('The Script exits without erros.');
end;

end.

