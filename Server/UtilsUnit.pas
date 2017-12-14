unit UtilsUnit;

{$mode objfpc}{$H+}



interface

uses
  Classes, SysUtils, jwaWinUser, IDSync, IDContext;

type
 stringArray = array of string;


{FORWARDS}
function ExtractTag(var str: string; tag: string): string;

function getAppropriateStringForVirtualKey(vKeyCode: longInt): string;

function CosineInterpolation(Val1, Val2, Angle: Double): Double;

procedure DeleteStringFromStringArray(var arr: stringArray; pos: integer);
{/FORWARDS}




implementation

function ExtractTag(var str: string; tag: string): string;
var
  i: longInt;
  endStr: string;
begin
  i := AnsiPos(tag, str) + Length(tag);

  for i := i to Length(str)-1 do
  begin
    //
    if (str[i] = '<') then // closing tag, exit
    begin
      result := endStr;
      exit;
    end;

    endStr := endStr + str[i];
  end;
end;

function getAppropriateStringForVirtualKey(vKeyCode: longInt): string;
var
  fKeyNum: longInt;
begin
  if (vKeyCode = VK_TAB) then
  begin
    exit('TAB');
  end
  else if (vKeyCode = VK_CAPITAL) then
  begin
    exit('CAPS');
  end
  else if (vKeyCode = VK_LSHIFT) then
  begin
    exit('LSHIFT');
  end
  else if (vKeyCode = VK_RSHIFT) then
  begin
    exit('RSHIFT');
  end
  else if (vKeyCode = VK_LCONTROL) then
  begin
    exit('LCTRL');
  end
  else if (vKeyCode = VK_RCONTROL) then
  begin
    exit('RCTRL');
  end
  else if (vKeyCode = VK_LWIN) then
  begin
    exit('LWIN');
  end
  else if (vKeyCode = VK_RWIN) then
  begin
    exit('RWIN');
  end
  else if (vKeyCode = VK_LMENU) then
  begin
    exit('LALT');
  end
  else if (vKeyCode = VK_RMENU) then
  begin
    exit('RALT');
  end
  else if (vKeyCode in [32..47]) then
  begin
    if (vKeyCode = 32) then
    begin
      exit('SPACEBAR');
    end
    else if (vKeyCode = 33) then
    begin
      exit('PG UP');
    end
    else if (vKeyCode = 34) then
    begin
      exit('PG DOWN');
    end
    else if (vKeyCode = 35) then
    begin
      exit('END');
    end
    else if (vKeyCode = 36) then
    begin
      exit('HOME');
    end
    else if (vKeyCode = 37) then
    begin
      exit('LEFT ARROW');
    end
    else if (vKeyCode = 38) then
    begin
      exit('UP ARROW');
    end
    else if (vKeyCode = 39) then
    begin
      exit('RIGHT ARROW');
    end
    else if (vKeyCode = 40) then
    begin
      exit('DOWN ARROW');
    end
    else if (vKeyCode = 41) then
    begin
      exit('SELECT');
    end
    else if (vKeyCode = 42) then
    begin
      exit('PREINT');
    end
    else if (vKeyCode = 43) then
    begin
      exit('EXECUTE');
    end
    else if (vKeyCode = 44) then
    begin
      exit('PRINT SCREEN');
    end
    else if (vKeyCode = 45) then
    begin
      exit('INS');
    end
    else if (vKeyCode = 46) then
    begin
      exit('DEL');
    end
    else if (vKeyCode = 47) then
    begin
      exit('HELP');
    end;
  end
  else if (vKeyCode in [112..135]) then  // F1-F24
  begin
    fKeyNum := vKeyCode - 111;
    exit('F'+IntToStr(fKeyNum));
  end
  else if (vKeyCode in [144..145]) then
  begin
    if (vKeyCode = 144) then
    begin
      exit('NUM LOCK');
    end
    else if (vKeyCode = 145) then
    begin
      exit('SCROLL LOCK');
    end;
  end;
end;

function CosineInterpolation(Val1, Val2, Angle: Double): Double;
var
  Percent: Double;
begin
  Percent := (1-Cos(Angle*PI))/2;
  Result := (Val1 * (1 - Percent) + Val2 * Percent);
end;

procedure DeleteStringFromStringArray(var arr: stringArray; pos: integer);
var
   len, j : integer;
begin
   len := Length(arr);

   if (pos > len-1) then
   begin
     exit;
   end
   else if (pos = len-1) then
   begin
     SetLength(arr, len-1);
     exit;
   end;

   for j := pos to len-2 do//we move all elements from aPosition+1 left...
     arr[j] := arr[j+1];//...with a position

   SetLength(arr, len-1);//now we have one element less
end;

end.

