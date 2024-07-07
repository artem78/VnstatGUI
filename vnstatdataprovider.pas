unit VnstatDataProvider;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  TStringArray = array of string;

  { TVnstatDataProvider }

  TVnstatDataProvider = class(TObject)
    public
      constructor Create;
      destructor Destroy; override;

      function VnstatInstalled: Boolean;
      function GetInterfaces: TStringArray;
      function GetYearlyStats(AInterfaceId: Integer): TJSONArray;
      function GetMontlyStats(AInterfaceId: Integer): TJSONArray;
      function GetDailyStats(AInterfaceId: Integer): TJSONArray;
      function GetHourlyStats(AInterfaceId: Integer): TJSONArray;
      procedure Refresh;

    private
      Data: TJsonObject;

      procedure LoadData;
      function GetStats(AInterfaceId: Integer; const ATimeUnit: String): TJSONArray;
  end;

implementation

uses process, FileUtil, jsonparser;

const
  {$IFOPT D+}
  Executable = '/home/artem/Projects/Lazarus/VnstatGUI/data/fakevnstat.py';
  {$ELSE}
  Executable = 'vnstat';
  {$ENDIF}

{ TVnstatDataProvider }

constructor TVnstatDataProvider.Create;
begin
  Data := nil;
end;

destructor TVnstatDataProvider.Destroy;
begin
  FreeAndNil(Data);
end;

function TVnstatDataProvider.VnstatInstalled: Boolean;
var
  Path: String;
begin
  Path := FindDefaultExecutablePath(Executable);
  Result := (Path <> '') and FileExists(Path);
end;

function TVnstatDataProvider.GetInterfaces: TStringArray;
var
  I: Integer;
begin
  if not Assigned(Data) then
    LoadData;

  SetLength(Result, TJSONArray(Data.FindPath('interfaces')).Count);
  for I := 0 to TJSONArray(Data.FindPath('interfaces')).Count - 1 do
  begin
    Result[I] := Data.FindPath('interfaces[' + IntToStr(I) + '].name').AsString;
  end;
end;

function TVnstatDataProvider.GetYearlyStats(AInterfaceId: Integer): TJSONArray;
begin
  Result := GetStats(AInterfaceId, 'year');
end;

function TVnstatDataProvider.GetDailyStats(AInterfaceId: Integer): TJSONArray;
begin
  Result := GetStats(AInterfaceId, 'day');
end;

function TVnstatDataProvider.GetHourlyStats(AInterfaceId: Integer): TJSONArray;
begin
  Result := GetStats(AInterfaceId, 'hour');
end;

function TVnstatDataProvider.GetMontlyStats(AInterfaceId: Integer): TJSONArray;
begin
  Result := GetStats(AInterfaceId, 'month');
end;

procedure TVnstatDataProvider.Refresh;
begin
  FreeAndNil(Data);

  LoadData;
end;

procedure TVnstatDataProvider.LoadData;
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
var
  Proc         : TProcess;
  OutputStream : TStream;
  BytesRead    : longint;
  Buffer       : array[1..BUF_SIZE] of byte;
  JsonParser: TJSONParser;
begin
  Proc := TProcess.Create(nil);
  OutputStream := TMemoryStream.Create;
  try
    Proc.Executable := FindDefaultExecutablePath(Executable);
    Proc.Parameters.Add('--json');
    Proc.Options := [poUsePipes{, poWaitOnExit}];
    Proc.Execute;

    repeat
      // Get the new data from the process to a maximum of the buffer size that was allocated.
      // Note that all read(...) calls will block except for the last one, which returns 0 (zero).
      BytesRead := Proc.Output.Read(Buffer, BUF_SIZE);

      // Add the bytes that were read to the stream for later usage
      OutputStream.Write(Buffer, BytesRead)

    until BytesRead = 0;  // Stop if no more data is available

    OutputStream.Position := 0;



    JsonParser := TJSONParser.Create(OutputStream{, DefaultOptions});
    try
      Data := JsonParser.Parse as TJSONObject;
    finally
      JsonParser.Free;
    end;


  finally
    OutputStream.Free;
    Proc.Free;
  end;
end;

function TVnstatDataProvider.GetStats(AInterfaceId: Integer; const ATimeUnit: String
  ): TJSONArray;
begin
  if not Assigned(Data) then
    LoadData;

  Result := Data.FindPath('interfaces[' + IntToStr(AInterfaceId) + '].traffic.' + ATimeUnit) as TJSONArray;
end;

end.

