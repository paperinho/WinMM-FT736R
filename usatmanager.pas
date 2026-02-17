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
    Panel1: TPanel;
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
  // 1. SICUREZZA: Verifichiamo che la Main sia pronta (evita l'Access Violation)
  if (FrMain = nil) or (FrMain.conn = nil) then begin
    ShowMessage('Errore: La connessione principale non è disponibile.');
    Exit;
  end;

  try
    // 2. COLLEGAMENTO DEI COMPONENTI (Saldatura software)
    // Non farlo nel Designer, forzalo qui via codice
    FrMain.querySat.Database    := FrMain.conn;
    FrMain.querySat.Transaction := FrMain.trans;
    FrMain.dsSat.DataSet        := FrMain.querySat;
    FrMain.grdSat.DataSource    := FrMain.dsSat;

    // 3. APERTURA DATI
    FrMain.querySat.Close;
    FrMain.querySat.SQL.Text := 'SELECT * FROM sat_memories ORDER BY id ASC';
    FrMain.querySat.Open;

    // 4. CONFIGURAZIONE COLONNE (Tutte quelle che volevi)
    FrMain.grdSat.Columns.Clear;
    with FrMain.grdSat.Columns.Add do begin FieldName := 'id';      Title.Caption := 'CH'; Width := 40; Alignment := taCenter; end;
    with FrMain.grdSat.Columns.Add do begin FieldName := 'rx_freq'; Title.Caption := 'FREQ RX (Down)'; Width := 120; Alignment := taCenter; end;
    with FrMain.grdSat.Columns.Add do begin FieldName := 'rx_mode'; Title.Caption := 'MODO RX'; Width := 80; Alignment := taCenter; end;
    with FrMain.grdSat.Columns.Add do begin FieldName := 'tx_freq'; Title.Caption := 'FREQ TX (Up)'; Width := 120; Alignment := taCenter; end;
    with FrMain.grdSat.Columns.Add do begin FieldName := 'tx_mode'; Title.Caption := 'MODO TX'; Width := 80; Alignment := taCenter; end;
    with FrMain.grdSat.Columns.Add do begin FieldName := 'mem_tag'; Title.Caption := 'NOTE / SATELLITE'; Width := 180; end;

    // 5. AGGANCIO FORMATTAZIONE GOLD STANDARD
    FrMain.querySat.FieldByName('rx_freq').OnGetText := @OnGetFreqText;
    FrMain.querySat.FieldByName('tx_freq').OnGetText := @OnGetFreqText;

  except
    on E: Exception do
      ShowMessage('EVE: Errore durante il caricamento Satelliti -> ' + E.Message);
  end;
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

