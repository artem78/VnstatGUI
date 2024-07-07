unit Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function BytesToStr(ABytes: Int64{; APrecision: Integer = 2}): String;

implementation

uses Math;

function BytesToStr(ABytes: Int64{; APrecision: Integer = 2}): String;
const
  SizeUnit: Array [0 .. 8] of string = ('Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB');
var
  i: Integer;
begin
  i := 0;

  while ABytes > IntPower(1024, i + 1) do
    Inc(i);

  Result := FormatFloat('###0.##', ABytes / IntPower(1024, i)) + ' ' + SizeUnit[i];
end;

end.

