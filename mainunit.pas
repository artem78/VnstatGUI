unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, EditBtn, TAGraph, TASeries, TAIntervalSources,
  TATools, TASources, DateTimePicker, VnstatDataProvider, usplashabout, Types;

type

  TTimeUnit = (tuYears, tuMonths, tuDays, tuHours);

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TButton;
    BeginDTPicker: TDateTimePicker;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointHintTool1: TDataPointHintTool;
    BytesChartSource: TListChartSource;
    EndDTPicker: TDateTimePicker;
    Label1: TLabel;
    Label3: TLabel;
    OpenHomepageButton: TButton;
    Chart1: TChart;
    RefreshButton: TBitBtn;
    SideBySideBarsCheckBox: TCheckBox;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    ChartSettingsPanel: TPanel;
    RxLineSeries: TBarSeries;
    SplashAbout1: TSplashAbout;
    TotalSeries: TBarSeries;
    TxSeries: TBarSeries;
    RxSeries: TBarSeries;
    Label2: TLabel;
    PageControl1: TPageControl;
    TimeUnitRadioGroup: TRadioGroup;
    InterfaceComboBox: TComboBox;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    TableTabSheet: TTabSheet;
    ChartTabSheet: TTabSheet;
    procedure AboutButtonClick(Sender: TObject);
    procedure BeginDTPickerChange(Sender: TObject);
    procedure BeginDTPickerCheckBoxChange(Sender: TObject);
    procedure BeginDTPickerEnter(Sender: TObject);
    procedure ChartToolset1DataPointHintTool1Hint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure DateTimeIntervalChartSource1DateTimeStepChange(Sender: TObject;
      ASteps: TDateTimeStep);
    procedure EndDTPickerChange(Sender: TObject);
    procedure EndDTPickerCheckBoxChange(Sender: TObject);
    procedure EndDTPickerEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InterfaceComboBoxChange(Sender: TObject);
    procedure OpenHomepageButtonClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SideBySideBarsCheckBoxChange(Sender: TObject);
    procedure StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure TimeUnitRadioGroupClick(Sender: TObject);
    procedure TxSeriesBeforeDrawBar(ASender: TBarSeries; ACanvas: TCanvas;
      const ARect: TRect; APointIndex, AStackIndex: Integer;
      var ADoDefaultDrawing: Boolean);
    procedure UseBeginDateCheckBoxChange(Sender: TObject);
    procedure UseEndDateCheckBoxChange(Sender: TObject);
  private
    DataProvider: TVnstatDataProvider;

    procedure ReloadAndRefresh;
    procedure RefreshData;
    procedure RefreshGrid;
    procedure RefreshChart;

    function GetTimeUnit: TTimeUnit;
    function GetInterfaceId: Integer;
    function GetUseBeginDate: Boolean;
    function GetUseEndDate: Boolean;
    function GetBeginDate: TDate;
    function GetEndDate: TDate;
  public
    property TimeUnit: TTimeUnit read GetTimeUnit;
    property InterfaceId: Integer read GetInterfaceId;
    property UseBeginDate: Boolean read GetUseBeginDate;
    property UseEndDate: Boolean read GetUseEndDate;
    property BeginDate: TDate read GetBeginDate;
    property EndDate: TDate read GetEndDate;
  end;

var
  MainForm: TMainForm;

implementation

uses process, fpjson, jsonparser, Math, DateUtils, FileUtil, TACustomSource,
  TAChartUtils, LCLIntf, utils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Interfaces: TStringArray;
  InterfaceName: String;
begin
  DataProvider := TVnstatDataProvider.Create;

  if not DataProvider.VnstatInstalled then
  begin
    MessageDlg('Could''t find ''vnstat'' executable. Check VnStat installed on your computer.', mtError, [mbOK], 0);
    Application.Terminate;
    Exit;
  end;

  Interfaces := DataProvider.GetInterfaces;
  if Length(Interfaces) = 0 then
  begin
    MessageDlg('No network interfaces found.', mtError, [mbOK], 0);
    Application.Terminate;
    Exit;
  end;

  InterfaceComboBox.Clear;
  for InterfaceName in Interfaces do
    InterfaceComboBox.Items.Append(InterfaceName);
  //InterfaceComboBox.ItemIndex := 0;

  StringGrid1.AutoSizeColumns;
  StringGrid1.SortColRow(True, 0);
  StringGrid1.SortOrder:=soAscending;
  //fixme: where green triangle?

  BeginDTPicker.Date := IncDay(Now, -14);
  EndDTPicker.Date := Now;

  {$ifopt D+}
  Caption:=Caption+' - [DEBUG BUILD]';
  {$endif}
end;

procedure TMainForm.DateTimeIntervalChartSource1DateTimeStepChange(
  Sender: TObject; ASteps: TDateTimeStep);
begin

end;

procedure TMainForm.EndDTPickerChange(Sender: TObject);
begin
  ReloadAndRefresh;
end;

procedure TMainForm.EndDTPickerCheckBoxChange(Sender: TObject);
begin
  ReloadAndRefresh;
end;

procedure TMainForm.EndDTPickerEnter(Sender: TObject);
begin
  if UseBeginDate then
    TDateTimePicker(Sender).MinDate := BeginDate
  else
    TDateTimePicker(Sender).MinDate := MinDateTime;
end;

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  SplashAbout1.ShowAbout;
end;

procedure TMainForm.BeginDTPickerChange(Sender: TObject);
begin
  ReloadAndRefresh;
end;

procedure TMainForm.BeginDTPickerCheckBoxChange(Sender: TObject);
begin
  ReloadAndRefresh;
end;

procedure TMainForm.BeginDTPickerEnter(Sender: TObject);
begin
  if UseEndDate then
    TDateTimePicker(Sender).MaxDate := EndDate
  else
    TDateTimePicker(Sender).MaxDate := MaxDateTime;
end;

procedure TMainForm.ChartToolset1DataPointHintTool1Hint(
  ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
var
  X, Y: Double;
  DateStr: string;
begin
  AHint:='';

  with ATool as TDataPointHintTool do
    if (Series is TBarSeries) then
      with TBarSeries(Series) do begin
        X := GetXValue(PointIndex);
        //Y := GetYValue(PointIndex);
        case TimeUnit of
          tuYears:  DateStr := IntToStr(Round(X));
          tuMonths: DateStr := FormatDateTime('mmmmm YYYY', X);
          tuDays:   DateStr := FormatDateTime('d mmmmm YYYY', X);
          tuHours:  DateStr := FormatDateTime('d mmmmm YYYY HH', X) + 'h';
        end;
        AHint := Format('    %s - in: %s, out: %s, total: %s', [
                    DateStr,
                    BytesToStr(Round(BytesChartSource.Item[PointIndex]^.Y)),
                    BytesToStr(Round(BytesChartSource.Item[PointIndex]^.YList[0])),
                    BytesToStr(Round(BytesChartSource.Item[PointIndex]^.YList[1]))
        ]);
      end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DataProvider.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if InterfaceComboBox.ItemIndex = -1 then
  begin
    InterfaceComboBox.ItemIndex := 0;
    ReloadAndRefresh;
  end;
end;

procedure TMainForm.InterfaceComboBoxChange(Sender: TObject);
begin
  RefreshData;
end;

procedure TMainForm.OpenHomepageButtonClick(Sender: TObject);
begin
  OpenURL('https://github.com/artem78/VnstatGUI');
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin
  RefreshData;
end;

procedure TMainForm.RefreshButtonClick(Sender: TObject);
begin
  ReloadAndRefresh;
end;

procedure TMainForm.SideBySideBarsCheckBoxChange(Sender: TObject);
begin
  RefreshData;
end;

procedure TMainForm.StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
var
  BytesA, BytesB: Int64;
begin
  if (ACol in [1..3]) and (BCol = ACol) then
  begin
    BytesA := StrToBytes(StringGrid1.Cells[ACol,ARow]);
    BytesB := StrToBytes(StringGrid1.Cells[BCol,BRow]);
    if BytesA > BytesB then
      Result := 1
    else if BytesA < BytesB then
      Result := -1
    else
      Result := 0;
  end
  else
    Result := CompareText(StringGrid1.Cells[ACol,ARow], StringGrid1.Cells[BCol,BRow]);

 if StringGrid1.SortOrder = soDescending then
    Result := -Result;
end;

procedure TMainForm.TimeUnitRadioGroupClick(Sender: TObject);
begin
  RefreshData;
end;

procedure TMainForm.TxSeriesBeforeDrawBar(ASender: TBarSeries;
  ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
  var ADoDefaultDrawing: Boolean);
begin

end;

procedure TMainForm.UseBeginDateCheckBoxChange(Sender: TObject);
begin
  ReloadAndRefresh;
end;

procedure TMainForm.UseEndDateCheckBoxChange(Sender: TObject);
begin
  ReloadAndRefresh;
end;

procedure TMainForm.ReloadAndRefresh;
begin
  DataProvider.UseBeginDate := UseBeginDate;
  DataProvider.UseEndDate := UseEndDate;
  DataProvider.BeginDate := BeginDate;
  DataProvider.EndDate := EndDate;
  DataProvider.Refresh;

  RefreshData;
end;

procedure TMainForm.RefreshData;
begin
  if TableTabSheet.IsVisible then
    RefreshGrid
  else if ChartTabSheet.IsVisible then
    RefreshChart;
end;

procedure TMainForm.RefreshGrid;
const
  ColTitles: array[0..3] of string = ('Year', 'Month', {'Day'} 'Date', 'Hour');
var
  JsonArray: TJSONArray;
  JsonArrayEnum: TBaseJSONEnumerator;
  DateStr, RxStr, TxStr, TotalStr{, AvgRateStr}: String;
  Rx, Tx: Int64;
  Year, Month, Day, Hour, Minute: Integer;
begin
  StringGrid1.Clean;
  StringGrid1.RowCount := 1;

  try
    InterfaceId;
    TimeUnit;
  except
    Exit;
  end;


  StringGrid1.BeginUpdate;
  try
    StringGrid1.Columns.Items[0].Title.Caption := ColTitles[Ord(TimeUnit)];

    case TimeUnit of
      tuYears:  JsonArray := DataProvider.GetYearlyStats(InterfaceId);
      tuMonths: JsonArray := DataProvider.GetMontlyStats(InterfaceId);
      tuDays:   JsonArray := DataProvider.GetDailyStats(InterfaceId);
      tuHours:  JsonArray := DataProvider.GetHourlyStats(InterfaceId);
    end;

    if Assigned(JsonArray) then
    begin
      JsonArrayEnum := JsonArray.GetEnumerator;
      try
        while JsonArrayEnum.MoveNext do
        begin
          Rx := JsonArrayEnum.Current.Value.FindPath('rx').AsInt64;
          Tx := JsonArrayEnum.Current.Value.FindPath('tx').AsInt64;
          Year := JsonArrayEnum.Current.Value.FindPath('date.year').AsInteger;
          if Assigned(JsonArrayEnum.Current.Value.FindPath('date.month')) then
            Month := JsonArrayEnum.Current.Value.FindPath('date.month').AsInteger
          else
            Month := -1;
          if Assigned(JsonArrayEnum.Current.Value.FindPath('date.day')) then
            Day := JsonArrayEnum.Current.Value.FindPath('date.day').AsInteger
          else
            Day := -1;
          if Assigned(JsonArrayEnum.Current.Value.FindPath('time.hour')) then
            Hour := JsonArrayEnum.Current.Value.FindPath('time.hour').AsInteger
          else
            Hour := -1;
          if Assigned(JsonArrayEnum.Current.Value.FindPath('time.minute')) then
            Minute := JsonArrayEnum.Current.Value.FindPath('time.minute').AsInteger
          else
            Minute := -1;

          case TimeUnit of
            tuYears:  DateStr := IntToStr(Year);
            tuMonths: DateStr := Format('%.4d-%.2d', [Year, Month]);
            tuDays:   DateStr := Format('%.4d-%.2d-%.2d', [Year, Month, Day]);
            //tuHours:  DateStr := Format('%.4d-%.2d-%.2d %.2dh', [Year, Month, Day, Hour]);
            tuHours:  DateStr := Format('%.4d-%.2d-%.2d %.2d:%.2d', [Year, Month, Day, Hour, Minute]);
          end;

          RxStr := BytesToStr(Rx);
          TxStr := BytesToStr(Tx);
          TotalStr := BytesToStr(Rx + Tx);
          //AvgRateStr := BytesToStr(JsonArrayEnum.Current.Value.FindPath('a').AsInt64);


          StringGrid1.InsertRowWithValues(StringGrid1.RowCount, [DateStr, RxStr, TxStr, TotalStr]);
          (*StringGrid1.Objects[1, StringGrid1.RowCount - 1] := TObject(JsonArrayEnum.Current.Value.FindPath('rx').AsInt64);
          StringGrid1.Objects[2, StringGrid1.RowCount - 1] := TObject(JsonArrayEnum.Current.Value.FindPath('tx').AsInt64);
          StringGrid1.Objects[3, StringGrid1.RowCount - 1] := TObject(JsonArrayEnum.Current.Value.FindPath('rx').AsInt64 + JsonArrayEnum.Current.Value.FindPath('tx').AsInt64);*)
        end;

      finally
        FreeAndNil(JsonArrayEnum);
      end;
    end;
  finally
    StringGrid1.EndUpdate();
    StringGrid1.AutoSizeColumns;

    // after update sorting lost, but sort arrows still remain
    // fix this
    StringGrid1.HideSortArrow;
  end;
end;

procedure TMainForm.RefreshChart;
const
  RxColor = TColor($CF9F72);
  TxColor = TColor($34E28A);
  TotalColor = TColor($3EAFFC);

  SizeUnit: Array [0 .. 8] of string = ('Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB');
var
  JsonArray: TJSONArray;
  JsonArrayEnum: TBaseJSONEnumerator;
  Rx, Tx: Int64;
  Year, Month, Day, Hour, Minute: Integer;
  X: {Integer} Double;
  //DateTime: TDateTime;
  MaxBytes: Int64 = 0;
  K: Integer;
  Rx2, Tx2: Double;
  //AxisOpts: TAxisIntervalParamOptions;
begin
  BytesChartSource.Clear;

  RxSeries.Clear;
  TxSeries.Clear;
  TotalSeries.Clear;

  RxSeries.SeriesColor := RxColor;
  TxSeries.SeriesColor := TxColor;
  TotalSeries.SeriesColor := TotalColor;

  try
    InterfaceId;
    TimeUnit;
  except
    Exit;
  end;

  if SideBySideBarsCheckBox.Checked then
  begin
    TxSeries.ShowInLegend := True;
    TotalSeries.Title := 'Total';
    TotalSeries.SeriesColor := TotalColor;

    RxSeries.BarWidthPercent  := 25;
    RxSeries.BarOffsetPercent := -25;

    TxSeries.BarWidthPercent  := 25;
    TxSeries.BarOffsetPercent := 0;

    TotalSeries.BarWidthPercent  := 25;
    TotalSeries.BarOffsetPercent := 25;
  end
  else
  begin
    TxSeries.ShowInLegend := False;
    //TxSeries.Visible := False;
    TotalSeries.Title := TxSeries.Title;
    TotalSeries.SeriesColor := TxSeries.SeriesColor;

    // Defaults
    RxSeries.BarWidthPercent  := 70;
    RxSeries.BarOffsetPercent := 0;

    TxSeries.BarWidthPercent  := 70;
    TxSeries.BarOffsetPercent := 0;

    TotalSeries.BarWidthPercent  := 70;
    TotalSeries.BarOffsetPercent := 0;
  end;

  case TimeUnit of
    tuYears:
      begin
        Chart1.BottomAxis.Marks.Source := nil;
        Chart1.BottomAxis.Marks.Style:=smsValue;
        Chart1.BottomAxis.Marks.Format:='%0:.0f';
        Chart1.BottomAxis.Intervals.Options := [];
      end

    else
      begin
        case TimeUnit of
          tuMonths: DateTimeIntervalChartSource1.Steps := [dtsMonth];
          tuDays:   DateTimeIntervalChartSource1.Steps := [dtsDay];
          tuHours:  DateTimeIntervalChartSource1.Steps := [dtsHour];
        end;

        Chart1.BottomAxis.Marks.Source := DateTimeIntervalChartSource1;
        Chart1.BottomAxis.Marks.Style:=smsLabel;
        //Chart1.BottomAxis.Marks.Format:='%0:.9g';
        Chart1.BottomAxis.Intervals.Options := [aipUseMaxLength, aipUseMinLength, aipUseNiceSteps];
      end;
  end;

  case TimeUnit of
    tuYears:  JsonArray := DataProvider.GetYearlyStats(InterfaceComboBox.ItemIndex);
    tuMonths: JsonArray := DataProvider.GetMontlyStats(InterfaceComboBox.ItemIndex);
    tuDays:   JsonArray := DataProvider.GetDailyStats(InterfaceComboBox.ItemIndex);
    tuHours:  JsonArray := DataProvider.GetHourlyStats(InterfaceComboBox.ItemIndex);
  end;

  if Assigned(JsonArray) then
  begin
    JsonArrayEnum := JsonArray.GetEnumerator;
    try
      while JsonArrayEnum.MoveNext do
      begin
        Rx := JsonArrayEnum.Current.Value.FindPath('rx').AsInt64;
        Tx := JsonArrayEnum.Current.Value.FindPath('tx').AsInt64;
        MaxBytes := Max(MaxBytes, Max(Rx, Tx));
      end;
    finally
      FreeAndNil(JsonArrayEnum);
    end;

    K := 0;
    while MaxBytes > IntPower(1024, K + 1) do
      Inc(K);

    Chart1.LeftAxis.Title.Caption := SizeUnit[K];


    JsonArrayEnum := JsonArray.GetEnumerator;
    try
      while JsonArrayEnum.MoveNext do
      begin
        Rx := JsonArrayEnum.Current.Value.FindPath('rx').AsInt64;
        Tx := JsonArrayEnum.Current.Value.FindPath('tx').AsInt64;
        Rx2 := Rx / IntPower(1024, K);
        Tx2 := Tx / IntPower(1024, K);
        Year := JsonArrayEnum.Current.Value.FindPath('date.year').AsInteger;
        if Assigned(JsonArrayEnum.Current.Value.FindPath('date.month')) then
          Month := JsonArrayEnum.Current.Value.FindPath('date.month').AsInteger
        else
          Month := -1;
        if Assigned(JsonArrayEnum.Current.Value.FindPath('date.day')) then
          Day := JsonArrayEnum.Current.Value.FindPath('date.day').AsInteger
        else
          Day := -1;
        if Assigned(JsonArrayEnum.Current.Value.FindPath('time.hour')) then
          Hour := JsonArrayEnum.Current.Value.FindPath('time.hour').AsInteger
        else
          Hour := -1;
        if Assigned(JsonArrayEnum.Current.Value.FindPath('time.minute')) then
          Minute := JsonArrayEnum.Current.Value.FindPath('time.minute').AsInteger
        else
          Minute := -1;

        case TimeUnit of
          tuYears:  X := Year {DateTime := EncodeDateTime(Year, 1, 1, 0, 0, 0, 0)};
          tuMonths: {DateTime} X := EncodeDateTime(Year, Month, 1, 0, 0, 0, 0);
          tuDays:   {DateTime} X := EncodeDateTime(Year, Month, Day, 0, 0, 0, 0);
          tuHours:  {DateTime} X := EncodeDateTime(Year, Month, Day, Hour, Minute {0}, 0, 0);
        end;

        RxSeries.AddXY(X {DateTime}, {Rx} Rx2);
        TxSeries.AddXY(X {DateTime}, {Tx} Tx2);
        TotalSeries.AddXY(X {DateTime}, {Rx + Tx} Rx2 + Tx2);

        BytesChartSource.AddXYList(X, [Rx, Tx, Rx + Tx]);
      end;

    finally
      FreeAndNil(JsonArrayEnum);
    end;
  end;
end;

function TMainForm.GetTimeUnit: TTimeUnit;
begin
  if TimeUnitRadioGroup.ItemIndex = -1 then
    raise Exception.Create('TimeUnit not set');

  Result := TTimeUnit(TimeUnitRadioGroup.ItemIndex);
end;

function TMainForm.GetInterfaceId: Integer;
begin
  if InterfaceComboBox.ItemIndex = -1 then
    raise Exception.Create('Network interface not set');

  Result := InterfaceComboBox.ItemIndex;
end;

function TMainForm.GetUseBeginDate: Boolean;
begin
  Result := BeginDTPicker.Checked;
end;

function TMainForm.GetUseEndDate: Boolean;
begin
  Result := EndDTPicker.Checked;
end;

function TMainForm.GetBeginDate: TDate;
begin
  Result := BeginDTPicker.Date;
end;

function TMainForm.GetEndDate: TDate;
begin
  Result := EndDTPicker.Date;
end;

end.

