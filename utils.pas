unit Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function BytesToStr(ABytes: Int64{; APrecision: Integer = 2}): String;
function StrToBytes(const AStr: String): Int64;

implementation

uses Math, StrUtils;

const
  SizeUnit: Array [0 .. 8] of string = ('Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB');

function BytesToStr(ABytes: Int64{; APrecision: Integer = 2}): String;
var
  i: Integer;
begin
  i := 0;

  while ABytes > IntPower(1024, i + 1) do
    Inc(i);

  Result := FormatFloat('###0.##', ABytes / IntPower(1024, i)) + ' ' + SizeUnit[i];
end;

function StrToBytes(const AStr: String): Int64;
var
  Val: Double;
  Exp: Integer;
begin
  Val := StrToFloat(ExtractWord(1, AStr, [' ']));
  Exp := IndexText(ExtractWord(2, AStr, [' ']), SizeUnit);
  Result := Round(Val * IntPower(1024, Exp));
end;

end.

