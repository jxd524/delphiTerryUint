//�����ļ�COPY������Ŀ¼�£����ĳ�����ֵ�ɸı�����
{$DEFINE LOGINFO}              //�Ƿ��¼Debug��Ϣ
{$DEFINE LogName}              //��¼Debug��Ϣ�ļ���
{$DEFINE CombiPackageCount}    //�Զ�����ϰ�����
{$DEFINE MTU}                  //�Զ���MTU��С

{$IFDEF LogName}
const
  CtLogName = 'P2SpUdpInfo.txt';
{$ENDIF}

{$IFDEF CombiPackageCount}
const
  CtMaxCombiPackageCount = 8; //���ܳ���16
{$ENDIF}

{$IFDEF MTU}
const
  CtMTU = 576;
{$ENDIF}