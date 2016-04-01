//将此文件COPY到工程目录下，更改常量的值可改变设置
{$DEFINE LOGINFO}              //是否记录Debug信息
{$DEFINE LogName}              //记录Debug信息文件名
{$DEFINE CombiPackageCount}    //自定义组合包个数
{$DEFINE MTU}                  //自定义MTU大小

{$IFDEF LogName}
const
  CtLogName = 'P2SpUdpInfo.txt';
{$ENDIF}

{$IFDEF CombiPackageCount}
const
  CtMaxCombiPackageCount = 8; //不能超过16
{$ENDIF}

{$IFDEF MTU}
const
  CtMTU = 576;
{$ENDIF}
