unit JxdCmdConst;

interface

const
  UDPMTU = 1400; //UDP 最大传输单元

  CMSG_Str               = 0 + $30;  //纯文本
  CMSG_Data              = 1 + $30;  //我的数据流
  CCodStr                = 9 + $30; //加密的文本
  CMSG_NEWDATA           = 3 + $30;
  CMSG_NEWDATABT         = 4 + $30;
  CNewProtocolFlag       = 201; //新协议版本号

resourcestring
  SException = '"%s" raised exception class [%s] with message "%s"';
  
implementation

end.
