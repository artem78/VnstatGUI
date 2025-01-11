unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, TAGraph, TASeries, TAIntervalSources,
  VnstatDataProvider, Types;

type

  { TMainForm }

  TMainForm = class(TForm)
    Chart1: TChart;
    RefreshButton: TBitBtn;
    SideBySideBarsCheckBox: TCheckBox;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    ChartSettingsPanel: TPanel;
    RxLineSeries: TBarSeries;
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
    procedure DateTimeIntervalChartSource1DateTimeStepChange(Sender: TObject;
      ASteps: TDateTimeStep);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InterfaceComboBoxChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SideBySideBarsCheckBoxChange(Sender: TObject);
    procedure TimeUnitRadioGroupClick(Sender: TObject);
    procedure TxSeriesBeforeDrawBar(ASender: TBarSeries; ACanvas: TCanvas;
      const ARect: TRect; APointIndex, AStackIndex: Integer;
      var ADoDefaultDrawing: Boolean);
  private
    DataProvider: TVnstatDataProvider;

    procedure RefreshData;
    procedure RefreshGrid;
    procedure RefreshChart;
  public

  end;

var
  MainForm: TMainForm;

implementation

uses process, fpjson, jsonparser, Math, DateUtils, FileUtil, TACustomSource,
  utils;

type
  TTimeUnit = (tuYears, tuMonths, tuDays, tuHours);

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
    Exit;
  end;


  InterfaceComboBox.Clear;
  Interfaces := DataProvider.GetInterfaces;
  for InterfaceName in Interfaces do
    InterfaceComboBox.Items.Append(InterfaceName);
  //InterfaceComboBox.ItemIndex := 0;

  StringGrid1.AutoSizeColumns;
end;

procedure TMainForm.DateTimeIntervalChartSource1DateTimeStepChange(
  Sender: TObject; ASteps: TDateTimeStep);
begin

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
    DataProvider.Refresh;
    RefreshData;
  end;
end;

procedure TMainForm.InterfaceComboBoxChange(Sender: TObject);
begin
  RefreshData;
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin
  RefreshData;
end;

procedure TMainForm.RefreshButtonClick(Sender: TObject);
begin
  DataProvider.Refresh;

  RefreshData;
end;

procedure TMainForm.SideBySideBarsCheckBoxChange(Sender: TObject);
begin
  RefreshData;
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

  if (InterfaceComboBox.ItemIndex = -1) or (TimeUnitRadioGroup.ItemIndex = -1) then
    Exit;


  StringGrid1.BeginUpdate;
  try
    StringGrid1.Columns.Items[0].Title.Caption := ColTitles[TimeUnitRadioGroup.ItemIndex];

    case TTimeUnit(TimeUnitRadioGroup.ItemIndex) of
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

          case TTimeUnit(TimeUnitRadioGroup.ItemIndex) of
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
  DateTime: TDateTime;
  MaxBytes: Int64 = 0;
  K: Integer;
  Rx2, Tx2: Double;
  AxisOpts: TAxisIntervalParamOptions;
begin
  RxSeries.Clear;
  TxSeries.Clear;
  TotalSeries.Clear;

  RxSeries.SeriesColor := RxColor;
  TxSeries.SeriesColor := TxColor;
  TotalSeries.SeriesColor := TotalColor;

  if (InterfaceComboBox.ItemIndex = -1) or (TimeUnitRadioGroup.ItemIndex = -1) then
    Exit;

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

  {case TTimeUnit(TimeUnitRadioGroup.ItemIndex) of
    tuYears:  DateTimeIntervalChartSource1.Steps := [dtsYear];
    tuMonths: DateTimeIntervalChartSource1.Steps := [dtsMonth];
    tuDays:   DateTimeIntervalChartSource1.Steps := [dtsDay];
    tuHours:  DateTimeIntervalChartSource1.Steps := [dtsHour];
  end;}

  case TTimeUnit(TimeUnitRadioGroup.ItemIndex) of
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

        case TTimeUnit(TimeUnitRadioGroup.ItemIndex) of
          tuYears:  {X := Year} DateTime := EncodeDateTime(Year, 1, 1, 0, 0, 0, 0);
          tuMonths: {DateTime} X := EncodeDateTime(Year, Month, 1, 0, 0, 0, 0);
          tuDays:   {DateTime} X := EncodeDateTime(Year, Month, Day, 0, 0, 0, 0);
          tuHours:  {DateTime} X := EncodeDateTime(Year, Month, Day, Hour, Minute {0}, 0, 0);
        end;

        RxSeries.AddXY(X {DateTime}, {Rx} Rx2);
        TxSeries.AddXY(X {DateTime}, {Tx} Tx2);
        TotalSeries.AddXY(X {DateTime}, {Rx + Tx} Rx2 + Tx2);


      end;

    finally
      FreeAndNil(JsonArrayEnum);
    end;
  end;
end;

end.

