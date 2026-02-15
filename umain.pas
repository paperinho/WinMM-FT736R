<<<<<<< HEAD
unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, DBGrids, ComCtrls, SQLite3Conn, SQLdb, db, USatManager;

type
  { TFrMain }
  TFrMain = class(TForm)
    btnSatMode: TBitBtn;
    btnSync: TBitBtn;
    btnExit: TBitBtn;
    btnInfo: TBitBtn;
    dsMain: TDataSource;
    grdMemories: TDBGrid;
    Panel1: TPanel;
    pnlTop: TPanel;
    lblTitle: TLabel;
    conn: TSQLite3Connection;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StatusBar1: TStatusBar;
    trans: TSQLTransaction;
    query: TSQLQuery;
    procedure btnExitClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnSatModeClick(Sender: TObject);
    procedure btnSyncClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  // MHz    = tutto il resto (gestisce 2, 3 o 4 cifre iniziali)
  Result := Copy(Clean, 1, Length(Clean)-4) + '.' +      // MHz
            Copy(Clean, Length(Clean)-3, 3) + '.' +      // kHz
            Copy(Clean, Length(Clean), 1);               // 100Hz
end;

procedure TFrMain.InitDatabase;
var
  DBPath: string;
begin
  if (conn = nil) or (SQLQuery1 = nil) then Exit;
  DBPath := ExtractFilePath(ParamStr(0)) + 'ft736r_memory.db';
  ShowMessage('DBPath: ' + DBPath);

  conn.Connected := True;
  conn.DatabaseName := DBPath;
  conn.Transaction := trans;
  SQLQuery1.Database := conn;

  conn.Params.Clear;
  conn.Params.Add('sqlite_force_create_database=NULL');

  try
    conn.Connected := True;
    trans.Active := True;

    // Tabella completa con Shift e Tono
    conn.ExecuteDirect('CREATE TABLE IF NOT EXISTS memories (' +
                       'id TEXT PRIMARY KEY, ' +
                       'freq TEXT, ' +
                       'mode TEXT, ' +
                       'rpt_shift TEXT, ' +
                       'rpt_tone TEXT, ' +
                       'band_mod TEXT, ' +
                       'mem_tag TEXT)');
    trans.CommitRetaining;

    // ... dopo la creazione della tabella memories ...

        // Tabella Satelliti Dedicata (101-110)
        conn.ExecuteDirect('CREATE TABLE IF NOT EXISTS sat_memories (' +
                           'id TEXT PRIMARY KEY, ' +
                           'rx_freq TEXT, ' +
                           'rx_mode TEXT, ' +
                           'tx_freq TEXT, ' +
                           'tx_mode TEXT, ' +
                           'mem_tag TEXT)');
        trans.CommitRetaining;

        // Controllo se popolare i satelliti
        SQLQuery1.Close;
        SQLQuery1.SQL.Text := 'SELECT * FROM sat_memories';
        SQLQuery1.Open;
        if SQLQuery1.IsEmpty then PrePopulateSatMemories;

        // Torno alla visualizzazione standard per la DBGrid principale
        SQLQuery1.Close;
        SQLQuery1.SQL.Text := 'SELECT * FROM memories ORDER BY id ASC';
        SQLQuery1.Open;

        // ... riaggancio OnGetText ...

    // Colleghiamo la formattazione Gold alla nuova colonna 'freq'
    if SQLQuery1.FindField('freq') <> nil then
       SQLQuery1.FieldByName('freq').OnGetText := @OnGetFreqText;

    if SQLQuery1.IsEmpty then PrePopulateMemories;

    StatusBar1.Panels.Items[1].Text := 'DB: Connesso | IW2NOY';
  except
    on E: Exception do ShowMessage('EVE: Errore Database -> ' + E.Message);
  end;
end;

procedure TFrMain.PrePopulateMemories;
var
  i: integer;
begin
  for i := 1 to 115 do
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
  for i := 101 to 110 do
  begin
    SQLQuery1.Append;
    SQLQuery1.FieldByName('id').AsString := IntToStr(i);
    SQLQuery1.FieldByName('rx_freq').AsString := '---';
    SQLQuery1.FieldByName('rx_mode').AsString := '---';
    SQLQuery1.FieldByName('tx_freq').AsString := '---';
    SQLQuery1.FieldByName('tx_mode').AsString := '---';
    SQLQuery1.FieldByName('mem_tag').AsString := '---';
    SQLQuery1.Post;
  end;
  SQLQuery1.ApplyUpdates;
  trans.CommitRetaining;
end;

procedure TFrMain.FormCreate(Sender: TObject);
begin
  //TStatusPanels.Items[0].Text:='Online';
  StatusBar1.Panels.Items[0].Text:='Online';
  StatusBar1.Panels.Items[1].Alignment:= taCenter;
  StatusBar1.Panels.Items[1].Text:='Memory Manager Yaesu FT-736R - by IW2NOY';
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

procedure TFrMain.btnInfoClick(Sender: TObject);
begin
  ShowMessage('Memory Manager Yaesu FT-736R v0.1a' + sLineBreak +
              'Stato: In sviluppo' + sLineBreak +
              'Sviluppatore: Graziano - IW2NOY' + sLineBreak +
              'Collaboratore AI: EVE');
end;

procedure TFrMain.btnSatModeClick(Sender: TObject);
begin
  FrSatManager.ShowModal; // Apre la gestione satelliti e blocca la main finché non chiudi
end;

procedure TFrMain.btnExitClick(Sender: TObject);
begin
  if QuestionDlg('Esci', 'Vuoi chiudere il software?', mtConfirmation, [mrYes, mrNo], 0) = mrYes then
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
=======
unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, DBGrids, ComCtrls, SQLite3Conn, SQLdb, db, USatManager;

type
  { TFrMain }
  TFrMain = class(TForm)
    btnSatMode: TBitBtn;
    btnSync: TBitBtn;
    btnExit: TBitBtn;
    btnInfo: TBitBtn;
    dsMain: TDataSource;
    grdMemories: TDBGrid;
    Panel1: TPanel;
    pnlTop: TPanel;
    lblTitle: TLabel;
    conn: TSQLite3Connection;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StatusBar1: TStatusBar;
    trans: TSQLTransaction;
    query: TSQLQuery;
    procedure btnExitClick(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure btnSatModeClick(Sender: TObject);
    procedure btnSyncClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  // MHz    = tutto il resto (gestisce 2, 3 o 4 cifre iniziali)
  Result := Copy(Clean, 1, Length(Clean)-4) + '.' +      // MHz
            Copy(Clean, Length(Clean)-3, 3) + '.' +      // kHz
            Copy(Clean, Length(Clean), 1);               // 100Hz
end;

procedure TFrMain.InitDatabase;
var
  DBPath: string;
begin
  if (conn = nil) or (SQLQuery1 = nil) then Exit;
  DBPath := ExtractFilePath(ParamStr(0)) + 'ft736r_memory.db';
  ShowMessage('DBPath: ' + DBPath);

  conn.Connected := True;
  conn.DatabaseName := DBPath;
  conn.Transaction := trans;
  SQLQuery1.Database := conn;

  conn.Params.Clear;
  conn.Params.Add('sqlite_force_create_database=NULL');

  try
    conn.Connected := True;
    trans.Active := True;

    // Tabella completa con Shift e Tono
    conn.ExecuteDirect('CREATE TABLE IF NOT EXISTS memories (' +
                       'id TEXT PRIMARY KEY, ' +
                       'freq TEXT, ' +
                       'mode TEXT, ' +
                       'rpt_shift TEXT, ' +
                       'rpt_tone TEXT, ' +
                       'band_mod TEXT, ' +
                       'mem_tag TEXT)');
    trans.CommitRetaining;

    // ... dopo la creazione della tabella memories ...

        // Tabella Satelliti Dedicata (101-110)
        conn.ExecuteDirect('CREATE TABLE IF NOT EXISTS sat_memories (' +
                           'id TEXT PRIMARY KEY, ' +
                           'rx_freq TEXT, ' +
                           'rx_mode TEXT, ' +
                           'tx_freq TEXT, ' +
                           'tx_mode TEXT, ' +
                           'mem_tag TEXT)');
        trans.CommitRetaining;

        // Controllo se popolare i satelliti
        SQLQuery1.Close;
        SQLQuery1.SQL.Text := 'SELECT * FROM sat_memories';
        SQLQuery1.Open;
        if SQLQuery1.IsEmpty then PrePopulateSatMemories;

        // Torno alla visualizzazione standard per la DBGrid principale
        SQLQuery1.Close;
        SQLQuery1.SQL.Text := 'SELECT * FROM memories ORDER BY id ASC';
        SQLQuery1.Open;

        // ... riaggancio OnGetText ...

    // Colleghiamo la formattazione Gold alla nuova colonna 'freq'
    if SQLQuery1.FindField('freq') <> nil then
       SQLQuery1.FieldByName('freq').OnGetText := @OnGetFreqText;

    if SQLQuery1.IsEmpty then PrePopulateMemories;

    StatusBar1.Panels.Items[1].Text := 'DB: Connesso | IW2NOY';
  except
    on E: Exception do ShowMessage('EVE: Errore Database -> ' + E.Message);
  end;
end;

procedure TFrMain.PrePopulateMemories;
var
  i: integer;
begin
  for i := 1 to 115 do
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
  for i := 101 to 110 do
  begin
    SQLQuery1.Append;
    SQLQuery1.FieldByName('id').AsString := IntToStr(i);
    SQLQuery1.FieldByName('rx_freq').AsString := '---';
    SQLQuery1.FieldByName('rx_mode').AsString := '---';
    SQLQuery1.FieldByName('tx_freq').AsString := '---';
    SQLQuery1.FieldByName('tx_mode').AsString := '---';
    SQLQuery1.FieldByName('mem_tag').AsString := '---';
    SQLQuery1.Post;
  end;
  SQLQuery1.ApplyUpdates;
  trans.CommitRetaining;
end;

procedure TFrMain.FormCreate(Sender: TObject);
begin
  //TStatusPanels.Items[0].Text:='Online';
  StatusBar1.Panels.Items[0].Text:='Online';
  StatusBar1.Panels.Items[1].Alignment:= taCenter;
  StatusBar1.Panels.Items[1].Text:='Memory Manager Yaesu FT-736R - by IW2NOY';
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

procedure TFrMain.btnInfoClick(Sender: TObject);
begin
  ShowMessage('Memory Manager Yaesu FT-736R v0.1a' + sLineBreak +
              'Stato: In sviluppo' + sLineBreak +
              'Sviluppatore: Graziano - IW2NOY' + sLineBreak +
              'Collaboratore AI: EVE');
end;

procedure TFrMain.btnSatModeClick(Sender: TObject);
begin
  FrSatManager.ShowModal; // Apre la gestione satelliti e blocca la main finché non chiudi
end;

procedure TFrMain.btnExitClick(Sender: TObject);
begin
  if QuestionDlg('Esci', 'Vuoi chiudere il software?', mtConfirmation, [mrYes, mrNo], 0) = mrYes then
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
>>>>>>> 1cfa27605bdedc6d6b2e6eb114899d54c04ab38c
