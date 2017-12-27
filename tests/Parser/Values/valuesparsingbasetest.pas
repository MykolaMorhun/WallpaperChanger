unit ValuesParsingBaseTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ParserBaseTestCase;

const
  VALUES_PARSER_TEST_SUITE_NAME = PARSER_TEST_SUITE_NAME + '.' + 'ValuesParser';

  FAILED_TO_PARSE = 'Failed to parse: ';

type

  TValuesParsingBaseTest = class abstract(TParserBaseTestCase)

  end;

implementation

end.

