{

单元名称: uJxdDataStruct
单元作者: 江晓德(Terry)
邮    箱: jxd524@163.com
说    明: 数据结构操作
开始时间: 2011-04-18
修改时间: 2011-04-18 (最后修改)
注意事项: 非线程安全

http://blog.csdn.net/hqd_acm/archive/2010/09/23/5901955.aspx

HashArray 中ID是不可重复的。
散列函数方法：除留余数法；f(k) = k mod m  其中 m = 散列值, n: 散列列表

为让散列比较均衡，减少冲突，m 的取值比较重要, m 的取值按以下算法进行

m要求： 素数（只能被1和本身整除);


常用的字符串Hash函数还有ELFHash，APHash等等，都是十分简单有效的方法。这些函数使用位运算使得每一个字符都对最后的函数值产生影响。
  另外还有以MD5和SHA1为代表的杂凑函数，这些函数几乎不可能找到碰撞。

常用字符串哈希函数有BKDRHash，APHash，DJBHash，JSHash，RSHash，SDBMHash，PJWHash，ELFHash等等。对于以上几种哈希函数，我对其进行了一个小小的评测。

 

Hash函数 	数据1 	数据2 	数据3 	数据4 	数据1得分 	数据2得分 	数据3得分 	数据4得分 	平均分
BKDRHash  	2 	    0 	  4774 	   481 	   96.55 	      100 	     90.95 	     82.05 	     92.64
APHash 	    2 	    3 	  4754 	   493 	   96.55 	      88.46 	   100 	       51.28 	     86.28
DJBHash 	  2 	    2 	  4975 	   474 	   96.55 	      92.31 	   0 	         100 	       83.43
JSHash 	    1 	    4 	  4761 	   506 	   100 	        84.62 	   96.83 	     17.95 	     81.94
RSHash 	    1 	    0 	  4861 	   505 	   100 	        100 	     51.58 	     20.51 	     75.96
SDBMHash 	  3 	    2 	  4849 	   504 	   93.1 	      92.31 	   57.01 	     23.08 	     72.41
PJWHash    	30 	    26 	  4878 	   513 	   0            	0 	      43.89 	   0 	         21.95
ELFHash 	  30 	    26 	  4878 	   513 	   0 	            0 	      43.89 	   0 	         21.95

其中
  数据1为 100000个字母和数字组成的随机串哈希冲突个数。
  数据2为100000个有意义的英文句子哈希冲突个数。
  数据3为数据1的哈希值与1000003(大素数)求模后存储到线性表中冲突的个数。
  数据4为数据1的哈希值与10000019(更大素数)求模后存储到线性表中冲突的个数。

经过比较，得出以上平均得分。平均数为平方平均数。可以发现，
  BKDRHash无论是在实际效果还是编码实现中，效果都是最突出的。
  APHash也是较为优秀的算法。
  DJBHash,JSHash,RSHash与SDBMHash各有千秋。PJWHash与ELFHash效果最差，但得分相似，其算法本质是相似的。

在信息修竞赛中，要本着易于编码调试的原则，个人认为BKDRHash是最适合记忆和使用的


// BKDR Hash Function  
unsigned int BKDRHash(char*str) 
[
    unsigned int seed=131 ;// 31 131 1313 13131 131313 etc..
    unsigned int hash=0 ;

    while(*str)
    [
        hash=hash*seed+(*str++);
    [

    return(hash % M);
[

// RS Hash Function
function RSHash(S: string): Cardinal;
var
a, b: Cardinal;
I: Integer;
begin
Result := 0;
a := 63689;
b := 378551;

for I := 1 to Length(S) do
begin
Result := Result * a + Ord(S[I]);
a := a * b;
end;

Result := Result and $7FFFFFFF;
end;

// JS Hash Function
function JSHash(S: string): Cardinal;
var
I: Integer;
begin
Result := 1315423911;

for I := 1 to Length(S) do
begin
Result := ((Result shl 5) + Ord(S[I]) + (Result shr 2)) xor Result;
end;

Result := Result and $7FFFFFFF;
end;

// P.J.Weinberger Hash Function
function PJWHash(S: string): Cardinal;
var
OneEighth,
ThreeQuarters,
BitsInUnignedInt,
HighBits,
test: Cardinal;
I: Integer;
begin
Result := 0;
test := 0;

BitsInUnignedInt := SizeOf(Cardinal) * 8;
ThreeQuarters := BitsInUnignedInt * 3 div 4;
OneEighth := BitsInUnignedInt div 8;
HighBits := $FFFFFFFF shl (BitsInUnignedInt - OneEighth);

for I := 1 to Length(S) do
begin
Result := (Result shl OneEighth) + Ord(S[I]);
test := Result and HighBits;
if test <> 0 then Result := ((Result xor (test shr ThreeQuarters)) and not HighBits);
end;

Result := Result and $7FFFFFFF;
end;

// ELF Hash Function
function ELFHash(S: string): Cardinal;
var
X: Cardinal;
I: Integer;
begin
Result := 0;
X := 0;

for I := 1 to Length(S) do
begin
Result := (Result shl 4) + Ord(S[I]);
X := Result and $F0000000;
if X <> 0 then
begin
Result := Result xor (X shr 24);
Result := Result and not X;
end;
end;

Result := Result and $7FFFFFFF;
end;

// BKDR Hash Function
function BKDRHash(S: string): Cardinal;
var
seed: Cardinal;
I: Integer;
begin
Result := 0;
seed := 131; // 31 131 1313 13131 131313 etc..

for I := 1 to Length(S) do
begin
Result := Result * seed + Ord(S[I]);
end;

Result := Result and $7FFFFFFF;
end;

// SDBM Hash Function
function SDBMHash(S: string): Cardinal;
var
I: Integer;
begin
Result := 0;

for I := 1 to Length(S) do
begin
Result := Ord(S[I]) + (Result shl 6) + (Result shl 16) - Result;
end;

Result := Result and $7FFFFFFF;
end;

// DJB Hash Function
function DJBHash(S: string): Cardinal;
var
I: Integer;
begin
Result := 5381;

for I := 1 to Length(S) do
begin
Result := Result + (Result shl 5) + Ord(S[I]);
end;

Result := Result and $7FFFFFFF;
end;

// AP Hash Function
function APHash(S: string): Cardinal;
var
I: Integer;
begin
Result := 0;

for I := 1 to Length(S) do
begin
if (i and 1) <> 0 then
Result := Result xor ((Result shl 7) xor Ord(S[I]) xor (Result shr 3))
else
Result := Result xor (not (Result shl 11) xor Ord(S[I]) xor (Result shr 5));
end;

Result := Result and $7FFFFFFF;
end;


function THashTable.HashOf(const Key: string): Cardinal;
    var
      I: Integer;
    begin
      Result := 0;
      for I := 1 to Length(Key) do
      Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
        Ord(Key[I]);
    end;
}
unit uHashFun;

interface

function HashFun_BKDR(const ApKey: PByte; const ALen: Integer): Cardinal;
function HashFun_AP(const ApKey: PByte; const ALen: Integer): Cardinal;
function HashFun_DJB(const ApKey: PByte; const ALen: Integer): Cardinal;

var
  HashSeed: Cardinal;

implementation

function HashFun_BKDR(const ApKey: PByte; const ALen: Integer): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ALen - 1 do
    Result := Result * HashSeed + Ord(PByte(Integer(ApKey) + i)^);
end;

function HashFun_AP(const ApKey: PByte; const ALen: Integer): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to ALen - 1 do
  begin
    if (i and 1) <> 0 then
      Result := Result xor ((Result shl 7) xor Ord(PByte(Integer(ApKey) + i)^) xor (Result shr 3))
    else
      Result := Result xor (not (Result shl 11) xor Ord(PByte(Integer(ApKey) + i)^) xor (Result shr 5));
  end;
end;

function HashFun_DJB(const ApKey: PByte; const ALen: Integer): Cardinal;
var
  i: Integer;
begin
  Result := 5381;
  for i := 1 to ALen - 1 do
    Result := Result + (Result shl 5) + Ord(PByte(Integer(ApKey) + i)^);
end;


initialization
  HashSeed := 1313;
  
end.
