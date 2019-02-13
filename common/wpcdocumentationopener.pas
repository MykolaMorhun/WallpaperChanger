unit WpcDocumentationOpener;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil,
  LCLIntf,
  OSUtils;

type

  EWpcDocsFormat = (
    HTML,
    PDF
  );

  { TWpcDocumentationOpener }

  TWpcDocumentationOpener = class
  private const
    ONLINE_DOCS_URI = 'https://github.com/MykolaMorhun/WallpaperChanger/blob/master/resources/docs/en/wallpaper-changer.adoc';

    DOCS_FORMATS_EXTS : Array[EWpcDocsFormat] of String = (
      'html',
      'pdf'
    );
  private
    FDocsPath : String;
    FDocsLang : String;

    FBaseDir : String;

    // Preferable format for opening documentation.
    FDefaultDocsFormat : EWpcDocsFormat;
    // Holds full path to documentation or empty string for each format.
    FAvailableDocs : Array[EWpcDocsFormat] of String;
  public
    constructor Create(DocumentationPath : String; LanguageId : String);
  public
    property PreferableDocsFormat : EWpcDocsFormat read FDefaultDocsFormat write FDefaultDocsFormat;
  public
    function OpenDocumentation(ChapterId : String = '') : Boolean;
  private
    function TryOpenDocs(Format : EWpcDocsFormat; ChapterId : String) : Boolean;
    function AddChapterIdToUrl(Base : String; ChapterId : String) : String; inline;
    function ConvertChapterIdToGithubChapterId(ChapterId : String) : String; inline;

    procedure SearchDocumentation(); inline;
  end;

implementation

{ TWpcDocumentationOpener }

constructor TWpcDocumentationOpener.Create(DocumentationPath : String; LanguageId : String);
var
  Format : EWpcDocsFormat;
begin
  FDocsPath := DocumentationPath;
  FDocsLang := LanguageId;

  if (DocumentationPath.EndsWith(PathSep)) then
    FBaseDir := DocumentationPath + LanguageId + PATH_SEPARATOR
  else
    FBaseDir := DocumentationPath + PATH_SEPARATOR + LanguageId + PATH_SEPARATOR;

  for Format in EWpcDocsFormat do
    FAvailableDocs[Format] := '';
  SearchDocumentation();

  FDefaultDocsFormat := HTML;
end;

procedure TWpcDocumentationOpener.SearchDocumentation();
var
  Format : EWpcDocsFormat;
  FoundFiles : TStringList;
begin
  for Format in EWpcDocsFormat do begin
    FoundFiles := FindAllFiles(FBaseDir, '*.' + DOCS_FORMATS_EXTS[Format], False);
    try
      if (FoundFiles.Count > 0) then
        FAvailableDocs[Format] := FoundFiles[0];
    finally
      FoundFiles.Free();
    end;
  end;
end;

{
  Opens specified documentation at given chapter if any.
  Returns true if opening was successful, false otherwise.
}
function TWpcDocumentationOpener.OpenDocumentation(ChapterId : String) : Boolean;
var
  Format : EWpcDocsFormat;
begin
  if ((FAvailableDocs[FDefaultDocsFormat] <> '') and TryOpenDocs(FDefaultDocsFormat, ChapterId)) then begin
    Result := True;
    exit;
  end;

  for Format in EWpcDocsFormat do
    if ((FAvailableDocs[Format] <> '') and TryOpenDocs(Format, ChapterId)) then begin
      Result := True;
      exit;
    end;

  Result := OpenURL(AddChapterIdToUrl(ONLINE_DOCS_URI, ConvertChapterIdToGithubChapterId(ChapterId)));
end;

function TWpcDocumentationOpener.TryOpenDocs(Format : EWpcDocsFormat; ChapterId : String) : Boolean;
begin
  case (Format) of
    HTML:
      Result := OpenURL('file://' + AddChapterIdToUrl(FAvailableDocs[Format], ChapterId));
    PDF:
      Result := OpenDocument(FAvailableDocs[Format]);
    else
      // Should never happen
      raise Exception.Create('Uknown documentation format: ' + IntToStr(Ord(Format)));
  end;
end;

function TWpcDocumentationOpener.AddChapterIdToUrl(Base : String; ChapterId : String) : String;
begin
  if (ChapterId <> '') then
    Result := Base + '#' + ChapterId
  else
    Result := Base;
end;

function TWpcDocumentationOpener.ConvertChapterIdToGithubChapterId(ChapterId : String) : String;
begin
  Delete(ChapterId, 1, 1);
  Result := StringReplace(ChapterId, '_', '-', [rfReplaceAll]);
end;


end.

