unit USatManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  ComCtrls, Buttons, SQLdb, db;

type
  { TFrSatManager }
  TFrSatManager = class(TForm)
    BitBtn1: TBitBtn;
    dsSat: TDataSource;
    grdSat: TDBGrid;
    Panel1: TPanel;
    querySat: TSQLQuery;
    StatusBar1: TStatusBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure OnGetFreqText(Sender: TField; var aText: string; {%H-}DisplayText: Boolean);
  public
  end;

var
  FrSatManager: TFrSatManager;

implementation

uses
  UMain; // Serve per accedere alla connessione e alla funzione FormatFreq

{$R *.lfm}

{ TFrSatManager }

procedure TFrSatManager.FormShow(Sender: TObject);
begin
  // Colleghiamo la query alla connessione della Main
  querySat.Database := FrMain.SQLite3Connection1;
  querySat.Transaction := FrMain.trans;

  querySat.Close;
  querySat.SQL.Text := 'SELECT * FROM sat_memories ORDER BY id ASC';
  querySat.Open;

  // Agganciamo la formattazione a entrambi i campi
  querySat.FieldByName('rx_freq').OnGetText := @OnGetFreqText;
  querySat.FieldByName('tx_freq').OnGetText := @OnGetFreqText;

  // Rinominiamo le colonne per chiarezza assoluta
  grdSat.Columns[0].Title.Caption := 'CH';
  grdSat.Columns[1].Title.Caption := 'FREQ RX (Down)';
  grdSat.Columns[2].Title.Caption := 'MODO RX';
  grdSat.Columns[3].Title.Caption := 'FREQ TX (Up)';
  grdSat.Columns[4].Title.Caption := 'MODO TX';
  grdSat.Columns[5].Title.Caption := 'NOTE / SATELLITE';

  // Centriamo le frequenze
  grdSat.Columns[1].Alignment := taCenter;
  grdSat.Columns[3].Alignment := taCenter;
end;

procedure TFrSatManager.BitBtn1Click(Sender: TObject);
begin
  Close; // Chiude la form attuale e torna alla Main
end;

procedure TFrSatManager.OnGetFreqText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if (Sender.AsString = '') or (Sender.AsString = '---') then
    aText := '---'
  else
    // Usiamo la funzione Gold della Main
    aText := FrMain.FormatFreqRetrograde(Sender.AsString);
end;

end.

