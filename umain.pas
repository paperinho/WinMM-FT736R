unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, DBGrids, ComCtrls, DBCtrls, SQLite3Conn, SQLdb, db;

type
  { TFrMain }
  TFrMain = class(TForm)
    btnSync: TBitBtn;
    btnExit: TBitBtn;
    btnInfo: TBitBtn;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    dsMain: TDataSource;
    dsSat: TDataSource;
    grdMemories: TDBGrid;
    grdSat: TDBGrid;
    DBSelezionatoLabel: TLabel;
    DBNameLabel: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pnlTop: TPanel;
    lblTitle: TLabel;
    conn: TSQLite3Connection;
    querySat: TSQLQuery;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StatusBar1: TStatusBar;
    Memories: TTabSheet;
    SatMemories: TTabSheet;
    Settings: TTabSheet;
    trans: TSQLTransaction;
    query: TSQLQuery;
    procedure btnExitClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnSyncClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    procedure InitDatabase;
    procedure PrePopulateMemories;
    procedure PrePopulateSatMemories;
    procedure OnGetFreqText(Sender: TField; var aText: string; DisplayText: Boolean);
  public
    function FormatFreqRetrograde(RawS: string): string;
  end;

var
  FrMain: TFrMain;

implementation

{$R *.lfm}

{ TFrMain }

function TFrMain.FormatFreqRetrograde(RawS: string): string;
var
  Clean: string;
  i: integer;
begin
  // Rimuoviamo tutto ciò che non è un numero
  Clean := '';
  for i := 1 to Length(RawS) do
    if RawS[i] in ['0'..'9'] then Clean := Clean + RawS[i];

  if Length(Clean) < 5 then Exit(RawS);

  // LOGICA DA DESTRA (Gold Standard):
  // .100Hz = ultimo carattere
  // .kHz   = i 3 caratteri precedenti
  //  MHz    = tutto il resto (gestisce 2, 3 o 4 cifre iniziali)
  Result := Copy(Clean, 1, Length(Clean)-4) + '.' +      // MHz
            Copy(Clean, Length(Clean)-3, 3) + '.' +      // kHz
            Copy(Clean, Length(Clean), 1);               // 100Hz
end;

procedure TFrMain.InitDatabase;
var
  DBPath: string;
begin
  // 1. SICUREZZA: Verifichiamo che i componenti fondamentali esistano
  if (conn = nil) or (SQLQuery1 = nil) or (querySat = nil) or (trans = nil) then
  begin
    ShowMessage('EVE: Errore critico - Componenti database non trovati sulla Form.');
    Exit;
  end;

  // Impostazione percorso database
  DBPath := ExtractFilePath(ParamStr(0)) + 'ft736r_memory.db';

  // Configurazione Connessione (usiamo solo "conn")
  conn.Connected := False;
  conn.DatabaseName := DBPath;
  conn.Transaction := trans;
  conn.Params.Clear;
  conn.Params.Add('sqlite_force_create_database=NULL');

  // Saldatura software dei componenti alle transazioni
  SQLQuery1.Database := conn;
  SQLQuery1.Transaction := trans;
  querySat.Database := conn;
  querySat.Transaction := trans;

  // Saldatura delle sorgenti dati alle griglie
  dsMain.DataSet := SQLQuery1;
  grdMemories.DataSource := dsMain;
  dsSat.DataSet := querySat;
  grdSat.DataSource := dsSat;

  try
    // Apertura fisica del file e della transazione
    conn.Connected := True;
    trans.Active := True;

    // 2. CREAZIONE TABELLE (se non esistono)
    // Tabella Standard Memories
    conn.ExecuteDirect('CREATE TABLE IF NOT EXISTS memories (' +
                       'id TEXT PRIMARY KEY, ' +
                       'freq TEXT, ' +
                       'mode TEXT, ' +
                       'rpt_shift TEXT, ' +
                       'rpt_tone TEXT, ' +
                       'band_mod TEXT, ' +
                       'mem_tag TEXT)');

    // Tabella Satelliti Dedicata
    conn.ExecuteDirect('CREATE TABLE IF NOT EXISTS sat_memories (' +
                       'id TEXT PRIMARY KEY, ' +
                       'rx_freq TEXT, ' +
                       'rx_mode TEXT, ' +
                       'tx_freq TEXT, ' +
                       'tx_mode TEXT, ' +
                       'mem_tag TEXT)');

    trans.CommitRetaining;

    // 3. CARICAMENTO DATI CON SELECT CAST (Logica anti-MEMO e ordinamento numerico)

    // Configurazione Query Satelliti
    querySat.Close;
    querySat.SQL.Text := 'SELECT CAST(id AS VARCHAR(5)) as id, ' +
                         'CAST(rx_freq AS VARCHAR(20)) as rx_freq, ' +
                         'CAST(rx_mode AS VARCHAR(10)) as rx_mode, ' +
                         'CAST(tx_freq AS VARCHAR(20)) as tx_freq, ' +
                         'CAST(tx_mode AS VARCHAR(10)) as tx_mode, ' +
                         'CAST(mem_tag AS VARCHAR(100)) as mem_tag ' +
                         'FROM sat_memories ORDER BY CAST(id AS INTEGER) ASC';
    querySat.Open;
    if querySat.IsEmpty then PrePopulateSatMemories;

    // Configurazione Query Standard
    SQLQuery1.Close;
    SQLQuery1.SQL.Text := 'SELECT CAST(id AS VARCHAR(5)) as id, ' +
                          'CAST(freq AS VARCHAR(20)) as freq, ' +
                          'CAST(mode AS VARCHAR(10)) as mode, ' +
                          'CAST(rpt_shift AS VARCHAR(10)) as rpt_shift, ' +
                          'CAST(rpt_tone AS VARCHAR(10)) as rpt_tone, ' +
                          'CAST(band_mod AS VARCHAR(10)) as band_mod, ' +
                          'CAST(mem_tag AS VARCHAR(100)) as mem_tag ' +
                          'FROM memories ORDER BY CAST(id AS INTEGER) ASC';
    SQLQuery1.Open;
    if SQLQuery1.IsEmpty then PrePopulateMemories;

    // 4. AGGANCIO FORMATTAZIONE FREQUENZE (Logica IW2NOY da destra)
    // Nota: L'aggancio va fatto DOPO l'Open dei dataset
    if SQLQuery1.FindField('freq') <> nil then
       SQLQuery1.FieldByName('freq').OnGetText := @OnGetFreqText;

    if querySat.FindField('rx_freq') <> nil then
       querySat.FieldByName('rx_freq').OnGetText := @OnGetFreqText;

    if querySat.FindField('tx_freq') <> nil then
       querySat.FieldByName('tx_freq').OnGetText := @OnGetFreqText;

    // Aggiornamento Interfaccia
    StatusBar1.Panels.Items[2].Text := 'DB: Connesso | Percorso: ' + DBPath;
    DBNameLabel.Caption := DBPath;

  except
    on E: Exception do
      ShowMessage('EVE: Errore durante l''inizializzazione del Database -> ' + E.Message);
  end;
end;

procedure TFrMain.PrePopulateMemories;
var
  i: integer;
begin
  for i := 1 to 100 do
  begin
    SQLQuery1.Append;
    SQLQuery1.FieldByName('id').AsString := FormatFloat('000', i);
    SQLQuery1.FieldByName('freq').AsString := '---';
    SQLQuery1.FieldByName('mode').AsString := '---';
    SQLQuery1.FieldByName('rpt_shift').AsString := '---';
    SQLQuery1.FieldByName('rpt_tone').AsString := '---';
    SQLQuery1.FieldByName('band_mod').AsString := '---';
    SQLQuery1.FieldByName('mem_tag').AsString := '---';
    SQLQuery1.Post;
  end;
  SQLQuery1.ApplyUpdates;
  trans.CommitRetaining;
end;

procedure TFrMain.PrePopulateSatMemories;
var
  i: integer;
begin
  // Usiamo querySat, non SQLQuery1!
  for i := 101 to 110 do
  begin
    querySat.Append;
    querySat.FieldByName('id').AsString := IntToStr(i);
    querySat.FieldByName('rx_freq').AsString := '---';
    querySat.FieldByName('rx_mode').AsString := '---';
    querySat.FieldByName('tx_freq').AsString := '---';
    querySat.FieldByName('tx_mode').AsString := '---';
    querySat.FieldByName('mem_tag').AsString := 'CH ' + IntToStr(i);
    querySat.Post;
  end;
  querySat.ApplyUpdates;
  trans.CommitRetaining;
end;

procedure TFrMain.FormCreate(Sender: TObject);
begin
  //TStatusPanels.Items[0].Text:='Online';
  StatusBar1.Panels.Items[0].Text:='Online';
  StatusBar1.Panels.Items[1].Alignment:= taCenter;
  StatusBar1.Panels.Items[1].Text:='Memory Manager Yaesu FT-736R - by IW2NOY';
  //Attivazione delle DBgrid e delle info nel pannello settings
  //Attivazione delle DBgrid Memories
  begin
    if not SQLQuery1.Active then
      SQLQuery1.Active:= True;
      SQLQuery1.Open;
  end;
  //Attivazione delle DBgrid sat_Memories
  begin
    if not querySat.Active then
      querySat.Active:= True;
      querySat.Open;
  end;
  //Attivazione delle info nel pannello Settings
  begin
    DBNameLabel.Caption:=SQLite3Connection1.DatabaseName;
  end;
end;

procedure TFrMain.FormDestroy(Sender: TObject);
begin
  SQLite3Connection1.Connected:=false;
end;

procedure TFrMain.FormShow(Sender: TObject);
begin
  InitDatabase;      // Inizializza il DB
  SQLite3Connection1.Connected:=true;
end;

procedure TFrMain.PageControl1Change(Sender: TObject);
begin

end;

procedure TFrMain.btnInfoClick(Sender: TObject);
begin
  ShowMessage('Memory Manager Yaesu FT-736R v0.3a' + sLineBreak +
              'Stato: In sviluppo' + sLineBreak +
              'Sviluppatore: Graziano - IW2NOY' + sLineBreak +
              'Collaboratore AI: EVE');
end;

//procedure TFrMain.btnSatModeClick(Sender: TObject);
//begin
//  FrSatManager.ShowModal; // Apre la gestione satelliti e blocca la main finché non chiudi
//end;

procedure TFrMain.btnExitClick(Sender: TObject);
begin
  if QuestionDlg('Esci', 'Vuoi chiudere il software?', mtConfirmation, [mrYes, mrNo], 0) = mrYes then
    querySat.Active:= False;
    SQLQuery1.Active:= False;
    SQLite3Connection1.Connected:= False;
    Application.Terminate;
end;

procedure TFrMain.btnSyncClick(Sender: TObject);
begin
  // Placeholder per la logica CAT Serial
  ShowMessage('EVE: Inizializzazione lettura CAT...' + sLineBreak +
              'Interrogazione dei 100 canali di memoria in corso.');
end;

procedure TFrMain.OnGetFreqText(Sender: TField; var aText: string; DisplayText: Boolean);
begin
  if (Sender.AsString = '') or (Sender.AsString = '---') then
    aText := '---'
  else
    aText := FormatFreqRetrograde(Sender.AsString);
end;

end.
