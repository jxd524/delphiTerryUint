unit uJxdUdpDefine;

interface

uses
  uJxdUdpIoHandle, uJxdHashCalc;

type
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///
  ///                                 共性信息定义
  ///
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////

  TServerStyle = (srvOnline, srvFileShare, srvHash, srvStat);
  TReplySign = (rsSuccess, rsExistsID, rsOverdueVersion, rsMustRegNewID, rsNotExistsID, rsNotFind, rsPart, rsError);
  THashStyle = (hsFileHash, hsWebHash);
  TConnectState = (csNULL, csConneting, csConnetFail, csConnetSuccess);

  PServerManageInfo = ^TServerManageInfo;
  TServerManageInfo = record
    FServerStyle: TServerStyle;
    FServerID: Cardinal; 
    FServerIP: Cardinal;
    FServerPort: Word;
    FTag: Cardinal;
  end;
  TAryServerInfo = array of TServerManageInfo;
  
  //文件传输事件定义l
  TOnFileTrasmintEvent = procedure(const ACmd: Word; const AIP: Cardinal; const APort: Word; const ABuffer: pAnsiChar; const ABufLen: Cardinal;
    const AIsSynchroCmd: Boolean; const ASynchroID: Word) of object;  
  //获取指定类型的服务器信息
  TOnGetServerInfo = function(const AServerStyle: TServerStyle; var AServerInfos: TAryServerInfo): Boolean of object;
  //HASH信息
  TOnHashInfo = procedure(Sender: TObject; const AFileHash, AWebHash: TxdHash) of object;



  ///////////////////////////////////////////////////////////////////////////////////////////////////////////
  ///
  ///                                 UDP通信定义
  ///
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////

  PCmdHead = ^TCmdHead; //命令头
  TCmdHead = packed record
    FCmdID: Word;
    FUserID: Cardinal;
  end;

  {1: 注册命令}
  PCmdRegisterInfo = ^TCmdRegisterInfo; //注册命令格式
  TCmdRegisterInfo = packed record
    FCmdHead: TCmdHead;
    FClientHash: array[0..15] of Byte;
  end;
  PCmdReplyRegisterInfo = ^TCmdReplyRegisterInfo;
  TCmdReplyRegisterInfo = packed record //注册返回命令格式
    FCmdHead: TCmdHead;
    FReplySign: TReplySign;
    FRegisterID: Cardinal;
  end;

  {2: 登陆命令}
  PCmdLoginInfo = ^TCmdLoginInfo; //登录命令格式
  TCmdLoginInfo = packed record
    FCmdHead: TCmdHead;
    FClientVersion: Word;
    FLocalIP: Cardinal;
    FLocalPort: Word;
    FClientHash: array[0..15] of Byte;
  end;
  PCmdReplyLoginInfo = ^TCmdReplyLoginInfo; //登录返回命令格式
  TCmdReplyLoginInfo = packed record
    FCmdHead: TCmdHead;
    FReplySign: TReplySign;
    FPublicIP: Cardinal;
    FPublicPort: Word;
    FSafeSign: Cardinal;
  end;
  PCmdClientReLoginToServerInfo = ^TCmdClientReLoginToServerInfo; //服务器要求客户端重新登录
  TCmdClientReLoginToServerInfo = packed record
    FCmd: TCmdHead;
  end;

  {3: 退出命令}
  PCmdLogoutInfo = ^TCmdLogoutInfo;  //退出服务器命令格式，无返回
  TCmdLogoutInfo = packed record
    FCmdHead: TCmdHead;
    FSafeSign: Cardinal;
  end;

  {4: 心跳命令}
  PCmdHeartbeatInfo = ^TCmdHeartbeatInfo;
  TCmdHeartbeatInfo = packed record  //心跳包，接收与返回都为同一个包
    FCmdHead: TCmdHead;
    FNeedReply: Boolean;
  end;

  {5: 随机获取在线用户}
  TCmdGetRandomUsersInfo = packed record
    FCmdHead: TCmdHead;
  end;
  PUserNetInfo = ^TUserNetInfo;
  TUserNetInfo = packed record
    FUserID: Cardinal;
    FPublicIP: Cardinal;
    FLocalIP: Cardinal;
    FPublicPort: Word;
    FLocalPort: Word;
  end;
  PCmdReplyGetRandomUsersInfo = ^TCmdReplyGetRandomUsersInfo;
  TCmdReplyGetRandomUsersInfo = packed record
    FCmdHead: TCmdHead;
    FReplySign: TReplySign;
    FOnlineUserCount: Cardinal;
    FReplyCount: Byte;
    FUserInfo: TUserNetInfo; 
  end;


   //////////////////////////////////////////////////////////////////////////////////////////////////////
   ////                                                                                              ////
   ////                                        {6: NAT穿透命令}                                      ////
   ///                                                                                               ////
   //////////////////////////////////////////////////////////////////////////////////////////////////////
  //1: 向指定用户发送P2P HELLO 信息
  PCmdP2PHelloInfo = ^TCmdP2PHelloInfo;
  TCmdP2PHelloInfo = packed record
    FCmdHead: TCmdHead;
    FCallUserID: Cardinal;
    FSelfNetInfo: TUserNetInfo;
  end;
  //2：P2P HELLO 回应
  PCmdP2PReplyHelloInfo = ^TCmdP2PReplyHelloInfo;
  TCmdP2PReplyHelloInfo = packed record
    FCmdHead: TCmdHead;
    FCallUserID: Cardinal;
    FSelfNetInfo: TUserNetInfo;
  end;
  //3：请求服务器帮忙连接
  PCmdCallMeInfo = ^TCmdCallMeInfo;
  TCmdCallMeInfo = packed record
    FCmdHead: TCmdHead;
    FCallUserID: Cardinal;
    FMethod: Byte; //取值0，1；具体参见通信协议文档
  end;
  //4：服务器回复 CALL ME 命令
  PCmdReplyCallMeInfo = ^TCmdReplyCallMeInfo;
  TCmdReplyCallMeInfo = packed record
    FCmdHead: TCmdHead;
    FReplySign: TReplySign;
    FUserNetInfo: TUserNetInfo;
  end;
  //5: P2P的Call Friend 信息, 由服务器发送
  PCmdCallFriendInfo = ^TCmdCallFriendInfo;
  TCmdCallFriendInfo = packed record
    FCmdHead: TCmdHead;
    FUserNetInfo: TUserNetInfo;
  end;
  //6: 客户端通知P2P连接断开服务
  PCmdP2PDisconnectedInfo = ^TCmdP2PDisconnectedInfo;
  TCmdP2PDisconnectedInfo = packed record
    FCmdHead: TCmdHead;
    FNotifyUserID: Cardinal;
  end;


  {7: 客户端之间的字符信息}
  TCmdP2PStringInfo = packed record
    FCmdHead: TCmdHead;
    FLen: Word;
    FInfo: PChar;
  end;


   //////////////////////////////////////////////////////////////////////////////////////////////////////
   ////                                                                                              ////
   ////                                         {8: 服务器之间的信息交流}                            ////
   ///                                                                                               ////
   //////////////////////////////////////////////////////////////////////////////////////////////////////
  PCmdS2SHelloServerInfo = ^TCmdS2SHelloServerInfo;
  TCmdS2SHelloServerInfo = packed record
    FCmdHead: TCmdHead;
  end;
  PCmdS2SReplyHelloServerInfo = ^TCmdS2SReplyHelloServerInfo;
  TCmdS2SReplyHelloServerInfo = packed record
    FCmdHead: TCmdHead;
    FServerStyle: TServerStyle;
  end;
  PCmdS2SServerOnlineInfo = ^TCmdS2SServerOnlineInfo;
  TCmdS2SServerOnlineInfo = packed record
    FCmdHead: TCmdHead;
    FServerStyle: TServerStyle;
  end;

   //////////////////////////////////////////////////////////////////////////////////////////////////////
   ////                                                                                              ////
   ////                                          {9: 获取服务器列表}                                 ////
   ///                                                                                               ////
   //////////////////////////////////////////////////////////////////////////////////////////////////////
  PCmdGetServerAddrInfo = ^TCmdGetServerAddrInfo;
  TCmdGetServerAddrInfo = packed record
    FCmdHead: TCmdHead;
  end;
  TCmdServerInfo = packed record
    FServerStyle: TServerStyle;
    FServerIP: Cardinal;
    FServerPort: Word;
  end;
  PCmdReplyGetServerAddrInfo = ^TCmdReplyGetServerAddrInfo;
  TCmdReplyGetServerAddrInfo = packed record
    FCmdHead: TCmdHead;
    FReplySign: TReplySign;
    FServerCount: Word;
    FServerInfo: array[0..0] of TCmdServerInfo;
  end;


   //////////////////////////////////////////////////////////////////////////////////////////////////////
   ////                                                                                              ////
   ////                                          {10: 文件传输}                                      ////
   ///                                                                                               ////
   //////////////////////////////////////////////////////////////////////////////////////////////////////

  //1: 文件信息查询
  PCmdQueryFileInfo = ^TCmdQueryFileInfo;
  TCmdQueryFileInfo = packed record
    FCmdHead: TCmdHead;
    FHashStyle: THashStyle;
    FHash: array[0..15] of Byte;
  end;
  PCmdReplyQueryFileInfo = ^TCmdReplyQueryFileInfo;
  TCmdReplyQueryFileInfo = packed record //文件信息
    FCmdHead: TCmdHead;
    FHashStyle: THashStyle;
    FHash: array[0..15] of Byte; //要查找的Hash
    FReplySign: TReplySign;     //以下是返回的信息 如果失败则没有以下内容
    FFileSize: Int64;
    FFileSegmentSize: Integer;
    FFileHash: array[0..15] of Byte;
  end;
  //2: 文件下载进度查询
  PCmdQueryFileProgressInfo = ^TCmdQueryFileProgressInfo;
  TCmdQueryFileProgressInfo = TCmdQueryFileInfo;
  PCmdReplyQueryFileProgressInfo = ^TCmdReplyQueryFileProgressInfo;
  TCmdReplyQueryFileProgressInfo = packed record
    FCmdHead: TCmdHead;
    FHashStyle: THashStyle;
    FHash: array[0..15] of Byte; //要查找的Hash
    FReplySign: TReplySign;     //以下是返回的信息
    FTableLen: Integer;
    FTableBuffer: array[0..0] of Byte;
  end;
  //3: 请求文件数据
  TFileRequestInfo = packed record
    FSegmentIndex: Word;
    FBlockIndex: Word;
  end;
  PCmdRequestFileDataInfo = ^TCmdRequestFileDataInfo;
  TCmdRequestFileDataInfo = packed record
    FCmdHead: TCmdHead;
    FHashStyle: THashStyle;
    FHash: array[0..15] of Byte;
    FRequestCount: Word;
    FRequestInfo: array[0..0] of TFileRequestInfo;
  end;
  PCmdReplyRequestFileInfo = ^TCmdReplyRequestFileInfo;
  TCmdReplyRequestFileInfo = packed record //向请求者发送指定文件内容
    FCmdHead: TCmdHead;
    FHashStyle: THashStyle;
    FHash: array[0..15] of Byte;
    FReplySign: TReplySign;
    FSegmentIndex: Integer;
    FBlockIndex: Word;
    FBufferLen: Word;
    FBuffer: array[0..0] of Byte;
  end;
  //4: 请求文件HASH信息
  PCmdGetFileSegmentHashInfo = ^TCmdGetFileSegmentHashInfo;
  TCmdGetFileSegmentHashInfo = TCmdQueryFileInfo;
  PCmdReplyGetFileSegmentHashInfo = ^TCmdReplyGetFileSegmentHashInfo;
  TCmdReplyGetFileSegmentHashInfo = packed record
    FCmdHead: TCmdHead;
    FFileHash: array[0..15] of Byte;
    FHashCheckSegmentSize: Cardinal;
    FSegmentHashs: array[0..0] of Byte;
  end;

   //////////////////////////////////////////////////////////////////////////////////////////////////////
   ////                                                                                              ////
   ////                               {11: HASH服务器 文件搜索}                                      ////
   ///                                                                                               ////
   //////////////////////////////////////////////////////////////////////////////////////////////////////

   //1: 搜索文件
   PCmdSearchFileUserInfo = ^TCmdSearchFileUserInfo;
   TCmdSearchFileUserInfo = TCmdQueryFileInfo;
   PCmdReplySearchFileUserInfo = ^TCmdReplySearchFileUserInfo;
   TCmdReplySearchFileUserInfo = packed record
     FCmdHead: TCmdHead;
     FHashStyle: THashStyle;
     FHash: array[0..15] of Byte;
     FUserCount: Word;
     FUserIDs: array[0..0] of Cardinal;
   end;
   //2: 更新HASH信息
   PCmdUpdateFileHashTableInfo = ^TCmdUpdateFileHashTableInfo;
   TCmdUpdateFileHashTableInfo = packed record
     FCmdHead: TCmdHead;
     FHashCount: Word;
     FFileHash: array[0..15] of Byte;
     FWebHash: array[0..15] of Byte;
   end;
   PCmdReplyUpdateFileHashTableInfo = ^TCmdReplyUpdateFileHashTableInfo;
   TCmdReplyUpdateFileHashTableInfo = packed record
     FCmdHead: TCmdHead;
     FReplySize: TReplySign;
   end;
   //3：用户下线
   PCmdClientShutdownInfo = ^TCmdClientShutdownInfo;
   TCmdClientShutdownInfo = packed record
     FCmdHead: TCmdHead;
     FShutDownID: Cardinal;
   end;


function GetReplySinInfo(ASing: TReplySign): string;
function GetServerStyleInfo(AStyle: TServerStyle): string;

const
  {命令定义}

  {P <-> S}
  CtCmdSynchroPackage = 99; {同步包命令}
  CtCmd_Register = 100; //注册
  CtCmdReply_Register = 101; //注册返回
  CtCmd_Login = 110; //登录
  CtCmdReply_Login = 111; //登录返回
  CtCmd_Logout = 112; //退出
  CtCmd_Heartbeat = 113; //心跳
  CtCmdReply_Heartbeat = 114; //心跳返回
  CtCmd_GetRandomUsers = 120; //获取随机用户
  CtCmdReply_GetRandomUsers = 121; //返回随机用户
  CtCmd_ClientRelogin = 130; //重新登录
  CtCmd_GetServerAddr = 140; //请求服务器地址信息
  CtCmdReply_GetServerAddr = 141; //返回服务地址信息

  {P <-> P}
  CtCmdP2P_Hello = 1000;
  CtCmdP2P_ReplyHello = 1001;
  CtCmd_CallMe = 1002;
  CtCmdReply_CallMe = 1003;
  CtCmd_CallFriend = 1004;
  CtCmdP2P_Disconnected = 1099;
  CtCmdP2P_StringInfo = 1005;

  {S <-> S}
  CtCmdS2S_ServerOnline = 9000;
  CtCmdS2S_HelloServer = 9001;
  CtCmdS2SReply_HelloServer = 9002;
  

  {文件传输命令定义}
  CtCmd_QueryFileInfo = 2000; //查询文件信息
  CtCmdReply_QueryFileInfo = 2001;
  CtCmd_QueryFileProgress = 2010; //查询文件进度
  CtCmdReply_QueryFileProgress = 2011;
  CtCmd_RequestFileData = 2020; //请求文件数据
  CtCmdReply_RequestFileData = 2021;
  CtCmd_GetFileSegmentHash = 2030; //请求文件HASH信息
  CtCmdReply_GetFileSegmentHash = 2031;

  {文件搜索命令}
  CtCmd_SearchFileUser = 2100; //搜索拥有者信息
  CtCmdReply_SearchFileUser = 2101;
  CtCmd_UpdateFileHashTable = 2110; //更新共享信息
  CtCmdReply_UpdateFileHashTable = 2111;
  CtCmd_ClientShutDown = 2122; //客户端下线


  {ID定义}
  CtMinUserID = 9999; //最小的用户ID，不包含
  CtOnlineServerID = 999; //在线服务器
  CtFileShareServerID = 898; //文件共享服务器，多台服务器使用同一ID
  CtHashServerID = 878; //HASH服务器，多台服务器使用同一ID
  CtStatServerID = 868; //统计服务器

  {其它}
  CtCmdRegisterInfoSize = SizeOf(TCmdRegisterInfo);
  CtCmdReplyRegisterInfoSize = SizeOf(TCmdReplyRegisterInfo);
  CtCmdLoginInfoSize = SizeOf(TCmdLoginInfo);
  CtCmdReplyLoginInfoSize = SizeOf(TCmdReplyLoginInfo);
  CtCmdClientReLoginToServerInfoSize = SizeOf(TCmdClientReLoginToServerInfo);
  CtCmdLogoutInfoSize = SizeOf(TCmdLogoutInfo);
  CtCmdHeartbeatInfoSize = SizeOf(TCmdHeartbeatInfo);
  CtCmdGetRandomUsersInfoSize = SizeOf(TCmdGetRandomUsersInfo);
  CtCmdReplyGetRandomUsersInfoSize = SizeOf(TCmdReplyGetRandomUsersInfo);
  CtCmdGetServerAddrInfoSize = SizeOf(TCmdGetServerAddrInfo);
  CtCmdReplyGetServerAddrInfoSize = SizeOf(TCmdReplyGetServerAddrInfo);

  CtUserNetInfoSize = SizeOf(TUserNetInfo);

  {P2P NAT穿透}
  CtCmdP2PHelloInfoSize = SizeOf(TCmdP2PHelloInfo);
  CtCmdP2PReplyHelloInfoSize = SizeOf(TCmdP2PReplyHelloInfo);
  CtCmdCallMeInfoSize = SizeOf(TCmdCallMeInfo);
  CtCmdReplyCallMeInfoSize = SizeOf(TCmdReplyCallMeInfo);
  CtCmdCallFriendInfoSize = SizeOf(TCmdCallFriendInfo);
  CtCmdP2PDisconnectedInfoSize = SizeOf(TCmdP2PDisconnectedInfo);
  {END}

  {10: 文件传输}
  CtCmdQueryFileInfoSize = SizeOf(TCmdQueryFileInfo);
  CtCmdReplyQueryFileInfoSize = SizeOf(TCmdReplyQueryFileInfo);

  CtCmdQueryFileProgressInfoSize = SizeOf(TCmdQueryFileProgressInfo);
  CtCmdReplyQueryFileProgressInfoSize = SizeOf(TCmdReplyQueryFileProgressInfo);

  CtCmdRequestFileDataInfoSize = SizeOf(TCmdRequestFileDataInfo);
  CtCmdReplyRequestFileInfoSize = SizeOf(TCmdReplyRequestFileInfo);

  CtCmdGetFileSegmentHashInfoSize = SizeOf(TCmdGetFileSegmentHashInfo);
  CtCmdReplyGetFileSegmentHashInfoSize = SizeOf(TCmdReplyGetFileSegmentHashInfo);
  {end}

  {11: 文件搜索}
  CtCmdSearchFileUserInfoSize = SizeOf(TCmdSearchFileUserInfo);
  CtCmdReplySearchFileUserInfoSize = SizeOf(TCmdReplySearchFileUserInfo);
  CtCmdUpdateFileHashTableInfoSize = SizeOf(TCmdUpdateFileHashTableInfo);
  CtCmdReplyUpdateFileHashTableInfoSize = SizeOf(TCmdReplyUpdateFileHashTableInfo);
  CtCmdClientShutdownInfoSize = SizeOf(TCmdClientShutdownInfo);
  {end}  
  
  {服务器之间}
  CtCmdS2SHelloServerInfoSize = SizeOf(TCmdS2SHelloServerInfo);
  CtCmdS2SReplyHelloServerInfoSize = SizeOf(TCmdS2SReplyHelloServerInfo);
  CtCmdS2SServerOnlineInfoSize = SizeOf(TCmdS2SServerOnlineInfo);
  {END}

  CtSynchroHeaderSize = 4;
  CtMinPackageLen = SizeOf(TCmdHead);
  CtMinSearchRandomUserCount = 6; //在线数量允许下，最少返回的用户数量
  CtMaxSearchRandomUserCount = 10; //每次随机搜索时，最多返回的用户数量

  CtMaxReplyRegisterPackageSize = CtCmdReplyRegisterInfoSize + CtSynchroHeaderSize;
  CtMaxReplyLoginPackageSize = CtCmdReplyLoginInfoSize + CtSynchroHeaderSize;
  CtMaxReplyHeartbeatPackageSize = CtCmdHeartbeatInfoSize + CtSynchroHeaderSize;
  CtMinReplyGetRandomPackageSize = CtCmdReplyGetRandomUsersInfoSize - CtUserNetInfoSize;
  CtMaxReplyGetRandomPackageSize = CtCmdReplyGetRandomUsersInfoSize + (CtMaxSearchRandomUserCount - 1) * CtUserNetInfoSize;
  CtFileRequestInfoSize = SizeOf(TFileRequestInfo);

  CtMaxRequestFileBlockCount = (CtSinglePackageSize - (CtCmdRequestFileDataInfoSize - CtFileRequestInfoSize)) div CtFileRequestInfoSize ; //最多请求文件分块数量
  CtMaxRequestPackageSize = CtCombiPackageSize - CtCmdReplyRequestFileInfoSize;
  CtMaxSearchUserCount = (CtMaxCombiPackageSize - 20) div 4;

  CtMinP2PStringInfoSize = SizeOf(TCmdP2PStringInfo) + 1;

implementation


function GetReplySinInfo(ASing: TReplySign): string;
begin
  case ASing of
    rsSuccess:  Result := '成功';
    rsExistsID: Result := 'ID已经存在';
    rsOverdueVersion: Result := '已经过期的客户端版本';
    rsMustRegNewID: Result := '必须重新注册ID';
    rsNotFind: Result := '查到不到指定内容';
    rsError:    Result := '错误';
    else
      Result := '未知错误';
  end;
end;

function GetServerStyleInfo(AStyle: TServerStyle): string;
begin
  case AStyle of
    srvOnline:    Result := '在线服务器' ;
    srvFileShare: Result := '文件共享服务器' ;
    srvHash:      Result := 'HASH服务器' ;
    srvStat:      Result := '统计服务器' ;
    else
      Result := '未知服务器';
  end;
end;

end.
