unit rz_an_frm_dataeditor_exporttomysql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, odbcconn, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, rz_an_cmp_dataeditor, rz_an_cmp_imagelist;

type

  { TFormDataEditorExportToMySQL }

  TFormDataEditorExportToMySQL = class(TForm)
    BitBtnClose: TBitBtn;
    BitBtnExport: TBitBtn;
    BitBtnConnect: TBitBtn;
    ComboBoxDatabase: TComboBox;
    EditDSN: TEdit;
    EditPassword: TEdit;
    EditTable: TEdit;
    EditUserName: TEdit;
    GroupBoxStep1: TGroupBox;
    GroupBoxStep2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    SQLConnectorMain: TSQLConnector;
    SQLQueryMain: TSQLQuery;
    SQLTransactionMain: TSQLTransaction;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure BitBtnConnectClick(Sender: TObject);
    procedure BitBtnExportClick(Sender: TObject);
    procedure EditDSNChange(Sender: TObject);
    procedure EditPasswordChange(Sender: TObject);
    procedure EditUserNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FRZImageList : TRZANImageList;
  public
    RZDataEditor : TRZANCustomDataEditor;
  end;

var
  FormDataEditorExportToMySQL: TFormDataEditorExportToMySQL;

implementation

{$R *.lfm}

{ TFormDataEditorExportToMySQL }

procedure TFormDataEditorExportToMySQL.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormDataEditorExportToMySQL.FormCreate(Sender: TObject);
begin
  Self.FRZImageList := TRZANImageList.Create(Self);
  Self.BitBtnConnect.Images := Self.FRZImageList;
  Self.BitBtnExport.Images := Self.FRZImageList;
  Self.BitBtnClose.Images := Self.FRZImageList;
  Self.BitBtnConnect.ImageIndex := 15;
  Self.BitBtnExport.ImageIndex := 15;
  Self.BitBtnClose.ImageIndex := 1;
end;

procedure TFormDataEditorExportToMySQL.BitBtnConnectClick(Sender: TObject);
var
  i : Integer;
begin
  try
    Self.SQLConnectorMain.DatabaseName := Self.EditDSN.Text;
    Self.SQLConnectorMain.UserName := Self.EditUserName.Text;
    Self.SQLConnectorMain.Password := Self.EditPassword.Text;
    Self.SQLConnectorMain.Connected := True;
    Self.SQLTransactionMain.Active := True;
    if Self.SQLQueryMain.Active then Self.SQLQueryMain.Close;
    Self.SQLQueryMain.SQL.Clear;
    Self.SQLQueryMain.SQL.Add('show databases');
    Self.SQLQueryMain.Open;
    Self.SQLQueryMain.Last;
    Self.SQLQueryMain.First;
    Self.ComboBoxDatabase.Items.Clear;
    for i := 1 to Self.SQLQueryMain.RecordCount do
    begin
      Self.ComboBoxDatabase.Items.Add(Self.SQLQueryMain.FieldByName('Database').AsString);
      Self.SQLQueryMain.Next;
    end;
    Self.GroupBoxStep2.Enabled := True;
  except
    MessageDlg('Error','Error while connecting to database',mtError,[mbOK],0);
  end;
end;

procedure TFormDataEditorExportToMySQL.BitBtnExportClick(Sender: TObject);
var
  LSQL,LSQLColumn,LSQLRecord : WideString;
  LDatabase, LTable : string;
  i,j,LColumnCount,LRowCount : Integer;
begin
  try
    LColumnCount := Self.RZDataEditor.ColCount;
    LRowCount := Self.RZDataEditor.RowCount;
    {Create Table}
    LSQLColumn := '';
    for i := 1 to LColumnCount do
    begin
      if i = 1 then
        LSQLColumn := Self.RZDataEditor.Cells[i-1,0] + ' varchar(255) '
      else
        LSQLColumn := LSQLColumn + ' , ' + Self.RZDataEditor.Cells[i-1,0] + ' varchar(255) '
      ;
    end;
    LDatabase := Self.ComboBoxDatabase.Text;
    LTable := Self.EditTable.Text;
    LSQL := ''
      + ' create table ' + LDatabase + '.' + LTable
      + #13#10 + ' ( '
      + #13#10 + LSQLColumn
      + #13#10 + ' ) '
      + #13#10 + ' engine = MYISAM '
    ;
    if Self.SQLQueryMain.Active then Self.SQLQueryMain.Close;
    Self.SQLQueryMain.SQL.Clear;
    Self.SQLQueryMain.SQL.Add(LSQL);
    Self.SQLQueryMain.ExecSQL;
    {Insert Records}
    LSQLRecord := '';
    for j := 1 to LRowCount - 1 do
    begin
      LSQLColumn := '';
      for i := 1 to LColumnCount do
      begin
        if i = 1 then
          LSQLColumn := '"' + Self.RZDataEditor.Cells[i-1,j] + '"'
        else
          LSQLColumn := LSQLColumn + ' , ' + '"' + Self.RZDataEditor.Cells[i-1,j] + '"'
        ;
      end;
      LSQLRecord := ''
        + ' insert into ' + LDatabase + '.' + LTable
        + #13#10 + ' values ( '
        + #13#10 + LSQLColumn
        + #13#10 + ' ) '
      ;
      if Self.SQLQueryMain.Active then Self.SQLQueryMain.Close;
      Self.SQLQueryMain.SQL.Clear;
      Self.SQLQueryMain.SQL.Add(LSQLRecord);
      Self.SQLQueryMain.ExecSQL;
    end;
    MessageDlg('Success','Data successfully exported.',mtInformation,[mbOK],0);
  except
    MessageDlg('Error'
      ,'Error while exporting data.'
        + #13#10 + 'Check if the table exists.'
      ,mtError,[mbOK],0
    );
  end;
end;

procedure TFormDataEditorExportToMySQL.BitBtnCloseClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormDataEditorExportToMySQL.EditDSNChange(Sender: TObject);
begin
  Self.GroupBoxStep2.Enabled := False;
end;

procedure TFormDataEditorExportToMySQL.EditPasswordChange(Sender: TObject);
begin
  Self.GroupBoxStep2.Enabled := False;
end;

procedure TFormDataEditorExportToMySQL.EditUserNameChange(Sender: TObject);
begin
  Self.GroupBoxStep2.Enabled := False;
end;

end.

