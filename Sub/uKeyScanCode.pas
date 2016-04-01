unit uKeyScanCode;

interface

const
  {************键盘上各键的扫描码以及组合键的扫描码***************/ \\}
  KEY_L1            = $4F;     {小写键盘上的键}
  KEY_L2            = $50;
  KEY_L3            = $51;  
  KEY_L4            = $4B;  
  KEY_L6            = $4D;
  KEY_L7            = $47;
  KEY_L8            = $48;
  KEY_L9            = $49;  
  KEY_ADD           = $2B;
  KEY_SUB           = $2D;
  KEY_SHIFT         = $2A;
  KEY_LEFT          = 75;             {左箭头}
  KEY_RIGHT         = 77;             {右箭头}
  KEY_UP            = 72;             {上箭头}
  KEY_DOWN          = 80;             {下箭头}                        
  KEY_F1            = 59;
  KEY_F2            = 60;  
  KEY_F3            = 61;  
  KEY_F4            = 62;  
  KEY_F5            = 63;
  KEY_F6            = 64;  
  KEY_F7            = 65;                  
  KEY_F8            = 66;  
  KEY_F9            = 67; 
  KEY_F10           = 68;  
  KEY_INSERT        = 82;
  KEY_HOME          = 71;
  KEY_PAGEUP        = 73;
  KEY_PAGEDOWN      = 81;
  KEY_DEL           = 83;
  KEY_END           = 79;
   
  KEY_DASH          = 12;   {   _-   }
  KEY_EQUAL         = 13;   {   +=   }
  KEY_LBRACKET      = 26;   {   {[   }
  KEY_RBRACKET      = 27;   {   ]   }  
  KEY_SEMICOLON     = 39;   {   :;   }  
  KEY_RQUOTE        = 40;   {  "'   } 
  KEY_LQUOTE        = 41;   {   ~`   }  
  KEY_PERIOD        = 52;   {   >.   }  
  KEY_COMMA         = 51;   {  <,   }  
  KEY_SLASH         = 53;   {  ?/   }  
  KEY_BACKSLASH     = 43;   { |\   }   
  KEY_ENTER         = 28;                   {回车键}  
  KEY_BACKSPACE     = 14;           {退格键}  
  KEY_SPACE         = 57;                  {空格键}  
  KEY_TAB           = 15;  
  KEY_ESC           = 1;
  KEY_Q             = 16;
  KEY_W             = 17;
  KEY_E             = 18;
  KEY_R             = 19;  
  KEY_T             = 20;  
  KEY_Y             = 21;  
  KEY_U             = 22;  
  KEY_I             = 23;  
  KEY_O             = 24;  
  KEY_P             = 25;  
  KEY_A             = 30;  
  KEY_S             = 31;  
  KEY_D             = 32;  
  KEY_F             = 33;  
  KEY_G             = 34;  
  KEY_H             = 35;  
  KEY_J             = 36;  
  KEY_K             = 37;  
  KEY_L             = 38;  
  KEY_Z             = 44;  
  KEY_X             = 45;  
  KEY_C             = 46;  
  KEY_V             = 47;  
  KEY_B             = 48;  
  KEY_N             = 49;  
  KEY_M             = 50;  
  KEY_1             = 2 ;   
  KEY_2             = 3;  
  KEY_3             = 4;  
  KEY_4             = 5;  
  KEY_5             = 6;  
  KEY_6             = 7 ; 
  KEY_7             = 8;  
  KEY_8             = 9;  
  KEY_9             = 10;  
  KEY_0             = 11;  
                       
  {+++++++++++++++++++++++++CTR+各键扫描码+++++++++++++++++++++++++}
  KEY_CTR_F1            = $5E;
  KEY_CTR_F2            = $5F;  
  KEY_CTR_F3            = $60;  
  KEY_CTR_F4            = $61;  
  KEY_CTR_F5            = $62;  
  KEY_CTR_F6            = $63;  
  KEY_CTR_F7            = $64;  
  KEY_CTR_F8            = $65 ; 
  KEY_CTR_F9            = $66;  
  KEY_CTR_F10           = $67 ; 
  KEY_CTR_2             = $03 ; 
  KEY_CTR_6             = $1E ; 
  KEY_CTR_Q             = $11 ; 
  KEY_CTR_W             = $17 ; 
  KEY_CTR_E             = $05 ; 
  KEY_CTR_R             = $12;  
  KEY_CTR_T             = $14 ; 
  KEY_CTR_Y             = $19 ; 
  KEY_CTR_U             = $15 ; 
  KEY_CTR_I             = $09 ; 
  KEY_CTR_O             = $0F ; 
  KEY_CTR_P             = $10 ; 
  KEY_CTR_LBRACKET      = $1B;       {   [   }  
  KEY_CTR_RBRACKET      = $1D;       {   ]   }  
  KEY_CTR_A             = $01;  
  KEY_CTR_S             = $13 ; 
  KEY_CTR_D             = $04;  
  KEY_CTR_F             = $06;  
  KEY_CTR_G             = $07;  
  KEY_CTR_H             = $08;  
  KEY_CTR_J             = $0A;  
  KEY_CTR_K             = $0B;  
  KEY_CTR_L             = $0C;  
  KEY_CTR_Z             = $1A;  
  KEY_CTR_X             = $18;  
  KEY_CTR_C             = $03;  
  KEY_CTR_V             = $16;  
  KEY_CTR_B             = $02;  
  KEY_CTR_N             = $0E;  
  KEY_CTR_M             = $0D;  
  KEY_CTR_SPACE         = $20;  
  KEY_CTR_BACKSPACE     = $7F;  
  KEY_CTR_ENTER         = $0A;  
  KEY_CTR_BACKSLASH     = $1C;     {   |\   }  
  KEY_CTR_L1            = $75;     {小写键盘上的键}  
  KEY_CTR_L3            = $76;  
  KEY_CTR_L4            = $73;  
  KEY_CTR_L6            = $74;  
  KEY_CTR_L7            = $77 ; 
  KEY_CTR_L9            = $84 ; 
   
   
  {+++++++++++++++++++++++++SHIFT+各键扫描码+++++++++++++++++++++++++/  }
  KEY_SHIFT_LQUOTE        = $7E;       {   ~`   }  
  KEY_SHIFT_1             = $21;  
  KEY_SHIFT_2             = $40;  
  KEY_SHIFT_3             = $23;  
  KEY_SHIFT_4             = $24;  
  KEY_SHIFT_5             = $25;  
  KEY_SHIFT_6             = $5E;  
  KEY_SHIFT_7             = $26;  
  KEY_SHIFT_8             = $2A;  
  KEY_SHIFT_9             = $28;  
  KEY_SHIFT_0             = $29;  
  KEY_SHIFT_DASH          = $5F;       {   _SHIFT_-   }  
  KEY_SHIFT_EQUAL         = $2B;     {   +=   }  
  KEY_SHIFT_BACKSPACE     = $08;  
  KEY_SHIFT_Q             = $51;  
  KEY_SHIFT_W             = $57;  
  KEY_SHIFT_E             = $45;  
  KEY_SHIFT_R             = $52;  
  KEY_SHIFT_T             = $54;  
  KEY_SHIFT_Y             = $59;  
  KEY_SHIFT_U             = $55;  
  KEY_SHIFT_I             = $49;  
  KEY_SHIFT_O             = $4F;  
  KEY_SHIFT_P             = $50;  
  KEY_SHIFT_LBRACKET      = $7B;     {   [   }  
  KEY_SHIFT_RBRACKET      = $7D;     {   ]   }  
  KEY_SHIFT_ENTER         = $0D;  
  KEY_SHIFT_A             = $41;  
  KEY_SHIFT_S             = $53;  
  KEY_SHIFT_D             = $44;  
  KEY_SHIFT_F             = $46;  
  KEY_SHIFT_G             = $47;  
  KEY_SHIFT_H             = $48;  
  KEY_SHIFT_J             = $4A;  
  KEY_SHIFT_K             = $4B;  
  KEY_SHIFT_L             = $4C;  
  KEY_SHIFT_SEMICOLON     = $3A;       {   :;   }  
  KEY_SHIFT_RQUOTE        = $22;             {   "'   }  
  KEY_SHIFT_Z             = $5A;  
  KEY_SHIFT_X             = $58;  
  KEY_SHIFT_C             = $43;  
  KEY_SHIFT_V             = $56;  
  KEY_SHIFT_B             = $42;  
  KEY_SHIFT_N             = $4E;  
  KEY_SHIFT_M             = $4D;  
  KEY_SHIFT_COMMA         = $3E;             {   <,   }  
  KEY_SHIFT_PERIOD        = $3F;           {   >.   }  
  KEY_SHIFT_SLASH         = $3F;             {   ?/   }  
  KEY_SHIFT_BACKSLASH     = $7C;     {   |\   }  
  KEY_SHIFT_F1            = $54;  
  KEY_SHIFT_F2            = $55;  
  KEY_SHIFT_F3            = $56;  
  KEY_SHIFT_F4            = $57;  
  KEY_SHIFT_F5            = $58;  
  KEY_SHIFT_F6            = $59;  
  KEY_SHIFT_F7            = $5A;  
  KEY_SHIFT_F8            = $5B;  
  KEY_SHIFT_F9            = $5C;  
  KEY_SHIFT_F10           = $5D;  
  KEY_SHIFT_L1            = $31;     {小写键盘上的键}  
  KEY_SHIFT_L2            = $32;  
  KEY_SHIFT_L3            = $33;  
  KEY_SHIFT_L4            = $34;  
  KEY_SHIFT_L5            = $35;  
  KEY_SHIFT_L6            = $36;  
  KEY_SHIFT_L7            = $37;  
  KEY_SHIFT_L8            = $38;  
  KEY_SHIFT_L9            = $39;  
  KEY_SHIFT_ADD           = $2B;  
  KEY_SHIFT_SUB           = $2D;  
   
  {/+++++++++++++++++++++++++ALT+各键扫描码+++++++++++++++++++++++++/ }
  KEY_ALT_F1            = $68;  
  KEY_ALT_F2            = $69;  
  KEY_ALT_F3            = $6A;  
  KEY_ALT_F4            = $6B;  
  KEY_ALT_F5            = $6C;  
  KEY_ALT_F6            = $6D;  
  KEY_ALT_F7            = $6E;  
  KEY_ALT_F8            = $6F;  
  KEY_ALT_F9            = $70;  
  KEY_ALT_F10           = $71;  
  KEY_ALT_1             = $78;  
  KEY_ALT_2             = $75;  
  KEY_ALT_3             = $7A;  
  KEY_ALT_4             = $7B;  
  KEY_ALT_5             = $7C;  
  KEY_ALT_6             = $7D;  
  KEY_ALT_7             = $7E;  
  KEY_ALT_8             = $7F;  
  KEY_ALT_9             = $80;  
  KEY_ALT_0             = $81;  
  KEY_ALT_DASH          = $82;     {   _ALT_-   }  
  KEY_ALT_EQUAL         = $83;   {   +=   }  
  KEY_ALT_Q             = $10;  
  KEY_ALT_W             = $11;  
  KEY_ALT_E             = $12;  
  KEY_ALT_R             = $13;  
  KEY_ALT_T             = $14 ; 
  KEY_ALT_Y             = $15 ; 
  KEY_ALT_U             = $16;  
  KEY_ALT_I             = $17;  
  KEY_ALT_O             = $18;  
  KEY_ALT_P             = $19;  
  KEY_ALT_A             = $1E;  
  KEY_ALT_S             = $1F;  
  KEY_ALT_D             = $20;  
  KEY_ALT_F             = $21;  
  KEY_ALT_G             = $22;  
  KEY_ALT_H             = $23;  
  KEY_ALT_J             = $24;  
  KEY_ALT_K             = $25;  
  KEY_ALT_L             = $26;  
  KEY_ALT_Z             = $2C ; 
  KEY_ALT_X             = $2D ; 
  KEY_ALT_C             = $2E ; 
  KEY_ALT_V             = $2F;  
  KEY_ALT_B             = $30;  
  KEY_ALT_N             = $31;  
  KEY_ALT_M             = $32;   

function CharToScanCode(const AChar: AnsiChar; var AScanCode: Word; var AIsShift: Boolean): Boolean;

implementation

function CharToScanCode(const AChar: AnsiChar; var AScanCode: Word; var AIsShift: Boolean): Boolean;
var
  nChr: AnsiChar;
begin
  AScanCode := 0;
  nChr := Char(0);
  if (AChar in ['a'..'z', '0'..'9']) or
     (AChar in ['`', '-', '=', '[', ']', '\', ';','''', ',', '.', '/']) then
  begin
    AIsShift := False;
    nChr := AChar;
  end
  else
  begin
    AIsShift := True;
    if AChar in ['A'..'Z'] then
      nChr := Char( Byte(AChar) + 32 )
    else 
    begin
      case AChar of
        '~': nChr := '`';
        '!': nChr := '1';
        '@': nChr := '2';  
        '#': nChr := '3';
        '$': nChr := '4';
        '%': nChr := '5';
        '^': nChr := '6';
        '&': nChr := '7';
        '*': nChr := '8';
        '(': nChr := '9';
        ')': nChr := '0';
        '_': nChr := '-';
        '+': nChr := '=';
        '{': nChr := '[';
        '}': nChr := ']';
        '|': nChr := '\';
        ':': nChr := ';';
        '"': nChr := '''';
        '<': nChr := ',';
        '>': nChr := '.';
        '?': nChr := '/';
      end;
    end;
  end;
       
  if nChr = Char(0) then
  begin
    Result := False;
    Exit;
  end;
  
  case nChr of
    'q': AScanCode := KEY_Q;
    'w': AScanCode := KEY_W;
    'e': AScanCode := KEY_E;
    'r': AScanCode := KEY_R;  
    't': AScanCode := KEY_T;  
    'y': AScanCode := KEY_Y;
    'u': AScanCode := KEY_U;
    'i': AScanCode := KEY_I;
    'o': AScanCode := KEY_O;
    'p': AScanCode := KEY_P;
    'a': AScanCode := KEY_A;
    's': AScanCode := KEY_S;
    'd': AScanCode := KEY_D;
    'f': AScanCode := KEY_F;
    'g': AScanCode := KEY_G;
    'h': AScanCode := KEY_H;
    'j': AScanCode := KEY_J;
    'k': AScanCode := KEY_K;
    'l': AScanCode := KEY_L;
    'z': AScanCode := KEY_Z;
    'x': AScanCode := KEY_X;
    'c': AScanCode := KEY_C;
    'v': AScanCode := KEY_V;
    'b': AScanCode := KEY_B;
    'n': AScanCode := KEY_N;
    'm': AScanCode := KEY_M;
    '1': AScanCode := KEY_1;
    '2': AScanCode := KEY_2;
    '3': AScanCode := KEY_3;
    '4': AScanCode := KEY_4;
    '5': AScanCode := KEY_5;
    '6': AScanCode := KEY_6;
    '7': AScanCode := KEY_7;
    '8': AScanCode := KEY_8;
    '9': AScanCode := KEY_9;
    '0': AScanCode := KEY_0;
    '-': AScanCode := KEY_DASH;
    '=': AScanCode := KEY_EQUAL;
    '[': AScanCode := KEY_LBRACKET;
    ']': AScanCode := KEY_RBRACKET;
    ';': AScanCode := KEY_SEMICOLON;
   '''': AScanCode := KEY_RQUOTE;
    '`': AScanCode := KEY_LQUOTE;
    '.': AScanCode := KEY_PERIOD;
    ',': AScanCode := KEY_COMMA;
    '/': AScanCode := KEY_SLASH;
    '\': AScanCode := KEY_BACKSLASH;
  end;
  Result := AScanCode <> 0;
end;

end.
