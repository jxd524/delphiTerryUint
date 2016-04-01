{$ALIGN ON}
{$MINENUMSIZE 4}

{((((((((((((((((((((((((((((((((((((((O)))))))))))))))))))))))))))))))))))))))}
{                                                                              }
{                    DIRECTX 8 CONSTANTS, ENUMS AND INTERFACES                 }
{                                                                              }
{          Copyright (C) Microsoft Corporation.  All Rights Reserved.          }
{                                                                              }
{((((((((((((((((((((((((((((((((((((((O)))))))))))))))))))))))))))))))))))))))}

const
  AMTUNER_EVENT_CHANGED   = $1;

type
  TAM_MEDIA_TYPE = record
    majortype: TGUID;
    subtype: TGUID;
    bFixedSizeSamples: BOOL;
    bTemporalCompression: BOOL;
    lSampleSize: ULONG;
    FormatType: TGUID;
    pUnk: IUnknown;
    cbFormat: ULONG;
    pbFormat: Pointer;
  end;
  PAM_Media_Type = ^TAM_Media_Type;

  TICCompressProc = function(lInput: LPARAM; lFrame: DWORD; lpBits: Pointer; len: DWORD): DWORD; stdcall;

    TIcCompressFrames = record
     dwFlags: dword;
     lpbiOutput: pBitmapInfoHeader;     // output format
     lOutput: LPARAM;
     lpbiInput: pBitmapInfoHeader;      // format of frame to compress
     lInput: LPARAM;         // input identifier
     lStartFrame: LongInt;    // start frame
     lFrameCount:LongInt;    // # of frames
     lQuality :LongInt;       // quality
     lDataRate :LongInt;      // data rate
     lKeyRate :LongInt;       // key frame rate
     dwRate :dword;		// frame rate, as always
     dwScale :dword;
     dwOverheadPerFrame :dword;
     dwReserved2 :dword;
  end;

  TIcInfo = record
     dwSize: dword;                 // sizeof(ICINFO)
     fccType: dword;                // compressor type     'vidc' 'audc'
     fccHandler: dword;             // compressor sub-type 'rle ' 'jpeg' 'pcm '
     dwFlags: dword;                // flags LOWORD is type specific
     dwVersion: dword;              // version of the driver
     dwVersionICM: dword;           // version of the ICM used
     szName: array[0..15] of char;             // short name
     szDescription: array[0..127] of char;     // long name
     szDriver: array[0..127] of char;          // driver that contains compressor
  end;

const
  ED__BASE                                 = $1000;

(*  ED__DEVCAP_CAN_RECORD                   = ED__BASE+1;
  ED__DEVCAP_CAN_RECORD_STROBE            = ED__BASE+2;
  ED__DEVCAP_HAS_AUDIO                    = ED__BASE+3;
  ED__DEVCAP_HAS_VIDEO                    = ED__BASE+4;
  ED__DEVCAP_USES_FILES                   = ED__BASE+5;
  ED__DEVCAP_CAN_SAVE                     = ED__BASE+6;
  ED__DEVCAP_DEVICE_TYPE                  = ED__BASE+7;
  ED__DEVTYPE_VCR                         = ED__BASE+8;
  ED__DEVTYPE_LASERDISK                   = ED__BASE+9;
  ED__DEVTYPE_ATR                         = ED__BASE+10;
  ED__DEVTYPE_DDR                         = ED__BASE+11;
  ED__DEVTYPE_ROUTER                      = ED__BASE+12;
  ED__DEVTYPE_KEYER                       = ED__BASE+13;
  ED__DEVTYPE_MIXER_VIDEO                 = ED__BASE+14;
  ED__DEVTYPE_DVE                         = ED__BASE+15;
  ED__DEVTYPE_WIPEGEN                     = ED__BASE+16;
  ED__DEVTYPE_MIXER_AUDIO                 = ED__BASE+17;
  ED__DEVTYPE_CG                          = ED__BASE+18;
  ED__DEVTYPE_TBC                         = ED__BASE+19;
  ED__DEVTYPE_TCG                         = ED__BASE+20;
  ED__DEVTYPE_GPI                         = ED__BASE+21;
  ED__DEVTYPE_JOYSTICK                    = ED__BASE+22;
  ED__DEVTYPE_KEYBOARD                    = ED__BASE+23;
  ED__DEVCAP_EXTERNAL_DEVICE_ID           = ED__BASE+24; *)
  ED__DEVCAP_TIMECODE_READ                = ED__BASE+25;
(*  ED__DEVCAP_TIMECODE_WRITE               = ED__BASE+26;
  ED__DEVCAP_CTLTRK_READ                  = ED__BASE+27;
  ED__DEVCAP_INDEX_READ                   = ED__BASE+28;
  ED__DEVCAP_PREROLL                      = ED__BASE+29;
  ED__DEVCAP_POSTROLL                     = ED__BASE+30;
  ED__DEVCAP_SYNC_ACCURACY                = ED__BASE+31;
  ED__SYNCACC_PRECISE                     = ED__BASE+32;
  ED__SYNCACC_FRAME                       = ED__BASE+33;
  ED__SYNCACC_ROUGH                       = ED__BASE+34;
  ED__DEVCAP_NORMAL_RATE                  = ED__BASE+35;
  ED__RATE_24                             = ED__BASE+36;
  ED__RATE_25                             = ED__BASE+37;
  ED__RATE_2997                           = ED__BASE+38;
  ED__RATE_30                             = ED__BASE+39;
  ED__DEVCAP_CAN_PREVIEW                  = ED__BASE+40;
  ED__DEVCAP_CAN_MONITOR_SOURCES          = ED__BASE+41;
  ED__DEVCAP_CAN_TEST                     = ED__BASE+42;
  ED__DEVCAP_VIDEO_INPUTS                 = ED__BASE+43;
  ED__DEVCAP_AUDIO_INPUTS                 = ED__BASE+44;
  ED__DEVCAP_NEEDS_CALIBRATING            = ED__BASE+45;
  ED__DEVCAP_SEEK_TYPE                    = ED__BASE+46;
  ED__SEEK_PERFECT                        = ED__BASE+47;
  ED__SEEK_FAST                           = ED__BASE+48;
  ED__SEEK_SLOW                           = ED__BASE+49;
  ED__POWER_ON                            = ED__BASE+50;
  ED__POWER_OFF                           = ED__BASE+51;
  ED__POWER_STANDBY                       = ED__BASE+52;
  ED__ACTIVE                              = ED__BASE+53;
  ED__INACTIVE                            = ED__BASE+54;
  ED__ALL                                 = ED__BASE+55;
  ED__TEST                                = ED__BASE+56;
  ED__TRANSCAP_CAN_EJECT                  = ED__BASE+100;
  ED__TRANSCAP_CAN_BUMP_PLAY              = ED__BASE+101;
  ED__TRANSCAP_CAN_PLAY_BACKWARDS         = ED__BASE+102;
  ED__TRANSCAP_CAN_SET_EE                 = ED__BASE+103;
  ED__TRANSCAP_CAN_SET_PB                 = ED__BASE+104;
  ED__TRANSCAP_CAN_DELAY_VIDEO_IN         = ED__BASE+105;
  ED__TRANSCAP_CAN_DELAY_VIDEO_OUT        = ED__BASE+106;
  ED__TRANSCAP_CAN_DELAY_AUDIO_IN         = ED__BASE+107;
  ED__TRANSCAP_CAN_DELAY_AUDIO_OUT        = ED__BASE+108;
  ED__TRANSCAP_FWD_VARIABLE_MAX           = ED__BASE+109;
  ED__TRANSCAP_REV_VARIABLE_MAX           = ED__BASE+110;
  ED__TRANSCAP_NUM_AUDIO_TRACKS           = ED__BASE+111;
  ED__TRANSCAP_LTC_TRACK                  = ED__BASE+112;
  ED__TRANSCAP_NEEDS_TBC                  = ED__BASE+113;
  ED__TRANSCAP_NEEDS_CUEING               = ED__BASE+114;
  ED__TRANSCAP_CAN_INSERT                 = ED__BASE+115;
  ED__TRANSCAP_CAN_ASSEMBLE               = ED__BASE+116;
  ED__TRANSCAP_FIELD_STEP                 = ED__BASE+117;
  ED__TRANSCAP_CLOCK_INC_RATE             = ED__BASE+118;
  ED__TRANSCAP_CAN_DETECT_LENGTH          = ED__BASE+119;
  ED__TRANSCAP_CAN_FREEZE                 = ED__BASE+120;
  ED__TRANSCAP_HAS_TUNER                  = ED__BASE+121;
  ED__TRANSCAP_HAS_TIMER                  = ED__BASE+122;
  ED__TRANSCAP_HAS_CLOCK                  = ED__BASE+123;
  ED__MEDIA_SPIN_UP                       = ED__BASE+130;
  ED__MEDIA_SPIN_DOWN                     = ED__BASE+131;
  ED__MEDIA_UNLOAD                        = ED__BASE+132; *)
  ED__MODE_PLAY                           = ED__BASE+200;
  ED__MODE_STOP                           = ED__BASE+201;
  ED__MODE_FREEZE                         = ED__BASE+202;
  ED__MODE_THAW                           = ED__BASE+203;
  ED__MODE_FF                             = ED__BASE+204;
  ED__MODE_REW                            = ED__BASE+205;
  ED__MODE_RECORD                         = ED__BASE+206;
  ED__MODE_RECORD_STROBE                  = ED__BASE+207;
  ED__MODE_RECORD_FREEZE		  = ED__BASE+808;
  ED__MODE_STEP                           = ED__BASE+208;
  ED__MODE_STEP_FWD		          = ED__BASE+208;
  ED__MODE_STEP_REV		          = ED__BASE+809;
  ED__MODE_SHUTTLE                        = ED__BASE+209;
  ED__MODE_EDIT_CUE                       = ED__BASE+210;
  ED__MODE_VAR_SPEED                      = ED__BASE+211;
  ED__MODE_PERFORM                        = ED__BASE+212;
  ED__MODE_LINK_ON                        = ED__BASE+280;
  ED__MODE_LINK_OFF                       = ED__BASE+281;
(*  ED__TCG_TIMECODE_TYPE                   = ED__BASE+400;
  ED__TCG_SMPTE_LTC                       = ED__BASE+401;
  ED__TCG_SMPTE_VITC                      = ED__BASE+402;
  ED__TCG_MIDI_QF                         = ED__BASE+403;
  ED__TCG_MIDI_FULL                       = ED__BASE+404;
  ED__TCG_FRAMERATE                       = ED__BASE+405;
  ED__FORMAT_SMPTE_30                     = ED__BASE+406;
  ED__FORMAT_SMPTE_30DROP                 = ED__BASE+407;
  ED__FORMAT_SMPTE_25                     = ED__BASE+408;
  ED__FORMAT_SMPTE_24                     = ED__BASE+409;
  ED__TCG_SYNC_SOURCE                     = ED__BASE+410;
  ED__TCG_VIDEO                           = ED__BASE+411;
  ED__TCG_READER                          = ED__BASE+412;
  ED__TCG_FREE                            = ED__BASE+413;
  ED__TCG_REFERENCE_SOURCE                = ED__BASE+414;
  ED__DUMMY_1                             = ED__BASE+415;
  ED__TCR_SOURCE                          = ED__BASE+416;
  ED__TCR_LTC                             = ED__BASE+417;
  ED__TCR_VITC                            = ED__BASE+418;
  ED__TCR_CT                              = ED__BASE+419;
  ED__TCR_FTC			         = ED__BASE+420;
  ED__TCR_LAST_VALUE		         = ED__BASE+421;
  ED__TCD_SOURCE                          = ED__BASE+422;
  ED__TCR                                 = ED__BASE+423;
  ED__TCG                                 = ED__BASE+424;
  ED__TCD_SIZE                            = ED__BASE+425;
  ED__SMALL                               = ED__BASE+426;
  ED__MED                                 = ED__BASE+427;
  ED__LARGE                               = ED__BASE+428;
  ED__TCD_POSITION                        = ED__BASE+429;
  ED__TCD_INTENSITY                       = ED__BASE+436;
  ED__HIGH                                = ED__BASE+437;
  ED__LOW                                 = ED__BASE+438;
  ED__TCD_TRANSPARENCY                    = ED__BASE+439;
  ED__TCD_INVERT                          = ED__BASE+440;
  ED__MODE                                = ED__BASE+500;
  ED__ERROR                               = ED__BASE+501;
  ED__LOCAL                               = ED__BASE+502;
  ED__RECORD_INHIBIT                      = ED__BASE+503;
  ED__SERVO_LOCK                          = ED__BASE+504;
  ED__MEDIA_PRESENT                       = ED__BASE+505;
  ED__MEDIA_LENGTH                        = ED__BASE+506;
  ED__MEDIA_SIZE                          = ED__BASE+507;
  ED__MEDIA_TRACK_COUNT                   = ED__BASE+508;
  ED__MEDIA_TRACK_LENGTH                  = ED__BASE+509;
  ED__MEDIA_SIDE                          = ED__BASE+510;
  ED__MEDIA_TYPE                          = ED__BASE+511;
  ED__MEDIA_VHS                           = ED__BASE+512;
  ED__MEDIA_SVHS                          = ED__BASE+513;
  ED__MEDIA_HI8                           = ED__BASE+514;
  ED__MEDIA_UMATIC                        = ED__BASE+515;
  ED__MEDIA_DVC                           = ED__BASE+516;
  ED__MEDIA_1_INCH                        = ED__BASE+517;
  ED__MEDIA_D1                            = ED__BASE+518;
  ED__MEDIA_D2                            = ED__BASE+519;
  ED__MEDIA_D3                            = ED__BASE+520;
  ED__MEDIA_D5                            = ED__BASE+521;
  ED__MEDIA_DBETA                         = ED__BASE+522;
  ED__MEDIA_BETA                          = ED__BASE+523;
  ED__MEDIA_8MM                           = ED__BASE+524;
  ED__MEDIA_DDR                           = ED__BASE+525;
  ED__MEDIA_SX			         = ED__BASE+813;
  ED__MEDIA_OTHER                         = ED__BASE+526;
  ED__MEDIA_CLV                           = ED__BASE+527;
  ED__MEDIA_CAV                           = ED__BASE+528;
  ED__MEDIA_POSITION                      = ED__BASE+529;
  ED__LINK_MODE                           = ED__BASE+530;
  ED__TRANSBASIC_TIME_FORMAT              = ED__BASE+540;
  ED__FORMAT_MILLISECONDS                 = ED__BASE+541;
  ED__FORMAT_FRAMES                       = ED__BASE+542;
  ED__FORMAT_REFERENCE_TIME               = ED__BASE+543;
  ED__DUMMY_2                             = ED__BASE+544;
  ED__DUMMY_3                             = ED__BASE+545;
  ED__DUMMY_4                             = ED__BASE+546;
  ED__FORMAT_HMSF                         = ED__BASE+547;
  ED__FORMAT_TMSF                         = ED__BASE+548;
  ED__TRANSBASIC_TIME_REFERENCE           = ED__BASE+549;
  ED__TIMEREF_TIMECODE                    = ED__BASE+550;
  ED__TIMEREF_CONTROL_TRACK               = ED__BASE+551;
  ED__TIMEREF_INDEX                       = ED__BASE+552;
  ED__TRANSBASIC_SUPERIMPOSE              = ED__BASE+553;
  ED__TRANSBASIC_END_STOP_ACTION          = ED__BASE+554;
  ED__TRANSBASIC_RECORD_FORMAT            = ED__BASE+555;
  ED__RECORD_FORMAT_SP                    = ED__BASE+556;
  ED__RECORD_FORMAT_LP                    = ED__BASE+557;
  ED__RECORD_FORMAT_EP                    = ED__BASE+558;
  ED__TRANSBASIC_STEP_COUNT               = ED__BASE+559;
  ED__TRANSBASIC_STEP_UNIT                = ED__BASE+560;
  ED__STEP_FIELD                          = ED__BASE+561;
  ED__STEP_FRAME                          = ED__BASE+562;
  ED__STEP_3_2                            = ED__BASE+563;
  ED__TRANSBASIC_PREROLL                  = ED__BASE+564;
  ED__TRANSBASIC_RECPREROLL               = ED__BASE+565;
  ED__TRANSBASIC_POSTROLL                 = ED__BASE+566;
  ED__TRANSBASIC_EDIT_DELAY               = ED__BASE+567;
  ED__TRANSBASIC_PLAYTC_DELAY             = ED__BASE+568;
  ED__TRANSBASIC_RECTC_DELAY              = ED__BASE+569;
  ED__TRANSBASIC_EDIT_FIELD               = ED__BASE+570;
  ED__TRANSBASIC_FRAME_SERVO              = ED__BASE+571;
  ED__TRANSBASIC_CF_SERVO                 = ED__BASE+572;
  ED__TRANSBASIC_SERVO_REF                = ED__BASE+573;
  ED__REF_EXTERNAL                        = ED__BASE+574;
  ED__REF_INPUT                           = ED__BASE+575;
  ED__REF_INTERNAL                        = ED__BASE+576;
  ED__REF_AUTO                            = ED__BASE+577;
  ED__TRANSBASIC_WARN_GL                  = ED__BASE+578;
  ED__TRANSBASIC_SET_TRACKING             = ED__BASE+579;
  ED__TRACKING_PLUS                       = ED__BASE+580;
  ED__TRACKING_MINUS                      = ED__BASE+581;
  ED__TRACKING_RESET                      = ED__BASE+582;
  ED__TRANSBASIC_SET_FREEZE_TIMEOUT       = ED__BASE+583;
  ED__TRANSBASIC_VOLUME_NAME              = ED__BASE+584;
  ED__TRANSBASIC_BALLISTIC_1              = ED__BASE+585;
  ED__TRANSBASIC_BALLISTIC_2              = ED__BASE+586;
  ED__TRANSBASIC_BALLISTIC_3              = ED__BASE+587;
  ED__TRANSBASIC_BALLISTIC_4              = ED__BASE+588;
  ED__TRANSBASIC_BALLISTIC_5              = ED__BASE+589;
  ED__TRANSBASIC_BALLISTIC_6              = ED__BASE+590;
  ED__TRANSBASIC_BALLISTIC_7              = ED__BASE+591;
  ED__TRANSBASIC_BALLISTIC_8              = ED__BASE+592;
  ED__TRANSBASIC_BALLISTIC_9              = ED__BASE+593;
  ED__TRANSBASIC_BALLISTIC_10             = ED__BASE+594;
  ED__TRANSBASIC_BALLISTIC_11             = ED__BASE+595;
  ED__TRANSBASIC_BALLISTIC_12             = ED__BASE+596;
  ED__TRANSBASIC_BALLISTIC_13             = ED__BASE+597;
  ED__TRANSBASIC_BALLISTIC_14             = ED__BASE+598;
  ED__TRANSBASIC_BALLISTIC_15             = ED__BASE+599;
  ED__TRANSBASIC_BALLISTIC_16             = ED__BASE+600;
  ED__TRANSBASIC_BALLISTIC_17             = ED__BASE+601;
  ED__TRANSBASIC_BALLISTIC_18             = ED__BASE+602;
  ED__TRANSBASIC_BALLISTIC_19             = ED__BASE+603;
  ED__TRANSBASIC_BALLISTIC_20             = ED__BASE+604;
  ED__TRANSBASIC_SETCLOCK                 = ED__BASE+605;
  ED__TRANSBASIC_SET_COUNTER_FORMAT       = ED__BASE+606;
  ED__TRANSBASIC_SET_COUNTER_VALUE        = ED__BASE+607;
  ED__TRANSBASIC_SETTUNER_CH_UP           = ED__BASE+608;
  ED__TRANSBASIC_SETTUNER_CH_DN           = ED__BASE+609;
  ED__TRANSBASIC_SETTUNER_SK_UP           = ED__BASE+610;
  ED__TRANSBASIC_SETTUNER_SK_DN           = ED__BASE+611;
  ED__TRANSBASIC_SETTUNER_CH              = ED__BASE+612;
  ED__TRANSBASIC_SETTUNER_NUM             = ED__BASE+613;
  ED__TRANSBASIC_SETTIMER_EVENT           = ED__BASE+614;
  ED__TRANSBASIC_SETTIMER_STARTDAY        = ED__BASE+615;
  ED__TRANSBASIC_SETTIMER_STARTTIME       = ED__BASE+616;
  ED__TRANSBASIC_SETTIMER_STOPDAY         = ED__BASE+617;
  ED__TRANSBASIC_SETTIMER_STOPTIME        = ED__BASE+618;
  ED__TRANSVIDEO_SET_OUTPUT               = ED__BASE+630;
  ED__E2E                                 = ED__BASE+631;
  ED__PLAYBACK                            = ED__BASE+632;
  ED__OFF                                 = ED__BASE+633;
  ED__TRANSVIDEO_SET_SOURCE               = ED__BASE+634;
  ED__TRANSAUDIO_ENABLE_OUTPUT            = ED__BASE+640;
  ED__TRANSAUDIO_ENABLE_RECORD            = ED__BASE+642;
  ED__TRANSAUDIO_ENABLE_SELSYNC           = ED__BASE+643;
  ED__TRANSAUDIO_SET_SOURCE               = ED__BASE+644;
  ED__TRANSAUDIO_SET_MONITOR              = ED__BASE+645;
  ED__INVALID                             = ED__BASE+652;
  ED__EXECUTING                           = ED__BASE+653;
  ED__REGISTER                            = ED__BASE+654;
  ED__DELETE                              = ED__BASE+655;
  ED__EDIT_HEVENT                         = ED__BASE+656;
  ED__EDIT_TEST                           = ED__BASE+657;
  ED__EDIT_IMMEDIATE                      = ED__BASE+658;
  ED__EDIT_MODE                           = ED__BASE+659;
  ED__EDIT_MODE_ASSEMBLE                  = ED__BASE+660;
  ED__EDIT_MODE_INSERT                    = ED__BASE+661;
  ED__EDIT_MODE_CRASH_RECORD              = ED__BASE+662;
  ED__EDIT_MODE_BOOKMARK_TIME             = ED__BASE+663;
  ED__EDIT_MODE_BOOKMARK_CHAPTER          = ED__BASE+664;
  ED__EDIT_MASTER                         = ED__BASE+666;
  ED__EDIT_TRACK                          = ED__BASE+667;
  ED__EDIT_SRC_INPOINT                    = ED__BASE+668;
  ED__EDIT_SRC_OUTPOINT                   = ED__BASE+669;
  ED__EDIT_REC_INPOINT                    = ED__BASE+670;
  ED__EDIT_REC_OUTPOINT                   = ED__BASE+671;
  ED__EDIT_REHEARSE_MODE                  = ED__BASE+672;
  ED__EDIT_BVB                            = ED__BASE+673;
  ED__EDIT_VBV                            = ED__BASE+674;
  ED__EDIT_VVV                            = ED__BASE+675;
  ED__EDIT_PERFORM                        = ED__BASE+676;
  ED__EDIT_ABORT                          = ED__BASE+677;
  ED__EDIT_TIMEOUT                        = ED__BASE+678;
  ED__EDIT_SEEK                           = ED__BASE+679;
  ED__EDIT_SEEK_MODE                      = ED__BASE+680;
  ED__EDIT_SEEK_EDIT_IN                   = ED__BASE+681;
  ED__EDIT_SEEK_EDIT_OUT                  = ED__BASE+682;
  ED__EDIT_SEEK_PREROLL                   = ED__BASE+683;
  ED__EDIT_SEEK_PREROLL_CT                = ED__BASE+684;
  ED__EDIT_SEEK_BOOKMARK                  = ED__BASE+685;
  ED__EDIT_OFFSET                         = ED__BASE+686;
  ED__ERR_DEVICE_NOT_READY                = ED__BASE+700;
  ED__TRANSCAP_FWD_VARIABLE_MIN	         = ED__BASE+800;
  ED__TRANSCAP_REV_VARIABLE_MIN	         = ED__BASE+801;
  ED__TRANSCAP_FWD_SHUTTLE_MAX		 = ED__BASE+802;
  ED__TRANSCAP_FWD_SHUTTLE_MIN		 = ED__BASE+803;
  ED__TRANSCAP_REV_SHUTTLE_MAX		 = ED__BASE+804;
  ED__TRANSCAP_REV_SHUTTLE_MIN		 = ED__BASE+805;
  ED__TRANSCAP_MULTIPLE_EDITS		 = ED__BASE+806;
  ED__TRANSCAP_IS_MASTER			 = ED__BASE+807;
  ED__MODE_NOTIFY_ENABLE		         = ED__BASE+810;
  ED__MODE_NOTIFY_DISABLE		 = ED__BASE+811;
  ED__MODE_SHOT_SEARCH			 = ED__BASE+812;
  ED__TRANSCAP_HAS_DT			 = ED__BASE+814;
  ED__EDIT_PREREAD	                 = ED__BASE+815;
  ED__DEVTYPE_CAMERA                      = ED__BASE+900;
  ED__DEVTYPE_TUNER                       = ED__BASE+901;
  ED__DEVTYPE_DVHS                        = ED__BASE+902;
  ED__DEVTYPE_UNKNOWN                     = ED__BASE+903;
  ED__CAPABILITY_UNKNOWN                  = ED__BASE+910;
  ED__RAW_EXT_DEV_CMD                     = ED__BASE+920;
  ED__MEDIA_VHSC                          = ED__BASE+925;
  ED__MEDIA_UNKNOWN                       = ED__BASE+926;
  ED__MEDIA_NOT_PRESENT                   = ED__BASE+927;
  ED__CONTROL_HEVENT_GET                  = ED__BASE+928;
  ED__CONTROL_HEVENT_RELEASE              = ED__BASE+929;
  ED__NOTIFY_HEVENT_GET                   = ED__BASE+930;
  ED__NOTIFY_HEVENT_RELEASE               = ED__BASE+931;
  ED__MODE_CHANGE_NOTIFY                  = ED__BASE+932; *)
  ED__MODE_PLAY_FASTEST_FWD               = ED__BASE+933;
  ED__MODE_PLAY_SLOWEST_FWD               = ED__BASE+934;
  ED__MODE_PLAY_FASTEST_REV               = ED__BASE+935;
  ED__MODE_PLAY_SLOWEST_REV               = ED__BASE+936;
(*  ED__MODE_WIND                           = ED__BASE+937;
  ED__MODE_REW_FASTEST                    = ED__BASE+938;
  ED__MODE_REV_PLAY                       = ED__BASE+939;
  ED__DEV_REMOVED__HEVENT_GET              = ED__BASE+960;
  ED__DEV_REMOVED__HEVENT_RELEASE          = ED__BASE+961; *)
  ED__TRANSBASIC_INPUT_SIGNAL             = ED__BASE+940;
  ED__TRANSBASIC_OUTPUT_SIGNAL            = ED__BASE+941;
  ED__TRANSBASIC_SIGNAL_525_60_SD         = ED__BASE+942;
  ED__TRANSBASIC_SIGNAL_525_60_SDL        = ED__BASE+943;
  ED__TRANSBASIC_SIGNAL_625_50_SD         = ED__BASE+944;
  ED__TRANSBASIC_SIGNAL_625_50_SDL        = ED__BASE+945;
  ED__TRANSBASIC_SIGNAL_MPEG2TS           = ED__BASE+946;
  ED__DEVCAP_TIMECODE_SEEK                = ED__BASE+950;
  ED__DEVCAP_ATN_READ	                 = ED__BASE+951;
(*  ED__DEVCAP_ATN_SEEK	                 = ED__BASE+952;
  ED__DEVCAP_ATN_WRITE	                 = ED__BASE+953;
  ED__DEVCAP_RTC_READ	                 = ED__BASE+954;
  ED__DEVCAP_RTC_SEEK	                 = ED__BASE+955;
  ED__DEVCAP_RTC_WRITE	                 = ED__BASE+956;
  ED__TIMEREF_ATN                         = ED__BASE+958;

  ED__AUDIO_1                             = $0000001;
  ED__AUDIO_2                             = $0000002;
  ED__AUDIO_3                             = $0000004;
  ED__AUDIO_4                             = $0000008;
  ED__AUDIO_5                             = $0000010;
  ED__AUDIO_6                             = $0000020;
  ED__AUDIO_7                             = $0000040;
  ED__AUDIO_8                             = $0000080;
  ED__AUDIO_9                             = $0000100;
  ED__AUDIO_10                            = $0000200;
  ED__AUDIO_11                            = $0000400;
  ED__AUDIO_12                            = $0000800;
  ED__AUDIO_13                            = $0001000;
  ED__AUDIO_14                            = $0002000;
  ED__AUDIO_15                            = $0004000;
  ED__AUDIO_16                            = $0008000;
  ED__AUDIO_17                            = $0010000;
  ED__AUDIO_18                            = $0020000;
  ED__AUDIO_19                            = $0040000;
  ED__AUDIO_20                            = $0080000;
  ED__AUDIO_21                            = $0100000;
  ED__AUDIO_22                            = $0200000;
  ED__AUDIO_23                            = $0400000;
  ED__AUDIO_24                            = $0800000;
  ED__VIDEO                               = $2000000;
  ED__AUDIO_ALL                           = $10000000; *)


type
  TReference_Time = int64;
  PReference_Time = ^TReference_Time;

  pFilter_State = ^TFilter_State;
  TFilter_State = (
    State_Stopped,
    State_Paused,
    State_Running
  );

(*  TDVAudInfo = record
    bAudStyle: array[0..1] of Byte;
    bAudQu: array[0..1] of Byte;
    bNumAudPin: Byte;
    wAvgSamplesPerPinPerFrm: array[0..1] of WORD;
    wBlkMode: WORD;
    wDIFMode: WORD;
    wBlkDiv: WORD;
  end; *)

const
    AMPROPERTY_PIN_CATEGORY = 0;
    AMPROPERTY_PIN_MEDIUM   = 1;

const
    PhysConn_Video_Tuner	   =  1;
    PhysConn_Video_Composite	   =  2;
    PhysConn_Video_SVideo	   =  3;
    PhysConn_Video_RGB	           =  4;
    PhysConn_Video_YRYBY	   =  5;
    PhysConn_Video_SerialDigital   =  6;
    PhysConn_Video_ParallelDigital =  7;
    PhysConn_Video_SCSI	           =  8;
    PhysConn_Video_AUX	           =  9;
    PhysConn_Video_1394	           = 10;
    PhysConn_Video_USB	           = 11;
    PhysConn_Video_VideoDecoder	   = 12;
    PhysConn_Video_VideoEncoder	   = 13;
    PhysConn_Video_SCART	   = 14;
    PhysConn_Video_Black	   = 15;

    PhysConn_Audio_Tuner	   = $1000;
    PhysConn_Audio_Line	           = $1000 + 1;
    PhysConn_Audio_Mic	           = $1000 + 2;
    PhysConn_Audio_AESDigital	   = $1000 + 3;
    PhysConn_Audio_SPDIFDigital	   = $1000 + 4;
    PhysConn_Audio_SCSI	           = $1000 + 5;
    PhysConn_Audio_AUX	           = $1000 + 6;
    PhysConn_Audio_1394	           = $1000 + 7;
    PhysConn_Audio_USB	           = $1000 + 8;
    PhysConn_Audio_AudioDecoder	   = $1000 + 9;

const
   KSPROPERTY_SUPPORT_GET = 1;
   KSPROPERTY_SUPPORT_SET = 2;

type
  IKsPropertySet = interface(IUnknown)
    ['{31EFAC30-515C-11d0-A9AA-00AA0061BE93}']
    function Set_(const guidPropSet: TGUID; dwPropID: dword; pInstanceData: pointer; cbInstanceData: DWORD; pPropData: pointer; cbPropData: DWORD): HRESULT; stdcall;
    function Get(const guidPropSet: TGUID; dwPropID: dword; pInstanceData: pointer; cbInstanceData: DWORD; pPropData: pointer ; cbPropData: DWORD; out pcbReturned: DWORD): HRESULT; stdcall;
    function QuerySupported(const guidPropSet: TGUID; dwPropID: dword; out pTypeSupport: DWORD): HRESULT; stdcall;
  end;

type

(*  TMPType = (
    MPT_INT,
    MPT_FLOAT,
    MPT_BOOL,
    MPT_ENUM,
    MPT_MAX
  );

  TMPPARAMINFO = record
    mpType          : TMPType;
    mopCaps         : Single;
    mpdMinValue     : Single;
    mpdMaxValue     : Single;
    mpdNeutralValue : Single;
    szUnitText      : array[0..31] of WCHAR;
    szLabel         : array[0..31] of WCHAR;
  end;

  TMPENVELOPESEGMENT = record
    rtStart  : int64;
    rtEnd    : int64;
    valStart : Single;
    valEnd   : Single;
    iCurve   : LongWord;
    flags    : dword;
  end;

  IMediaParamInfo = interface(IUnknown)
    ['{6d6cbb60-a223-44aa-842f-a2f06750be6d}']
    function GetParamCount(out pdwParams: DWORD): HResult; stdcall;
    function GetParamInfo(dwParamIndex: DWORD; out pInfo: TMPPARAMINFO): HResult; stdcall;
    function GetParamText(dwParamIndex: DWORD; out ppwchText: PWideChar): HResult; stdcall;
    function GetNumTimeFormats(out pdwNumTimeFormats: DWORD): HResult; stdcall;
    function GetSupportedTimeFormat(dwFormatIndex: DWORD; out pguidTimeFormat: TGUID): HResult; stdcall;
    function GetCurrentTimeFormat(out pguidTimeFormat: TGUID; out pTimeData: DWORD): HResult; stdcall;
  end;

  IMediaParams = interface(IUnknown)
    ['{6d6cbb61-a223-44aa-842f-a2f06750be6e}']
    function GetParam(dwParamIndex: DWORD; out pValue: Single): HResult; stdcall;
    function SetParam(dwParamIndex: DWORD; value: Single): HResult; stdcall;
    function AddEnvelope(dwParamIndex, cSegments: DWORD; var pEnvelopeSegments: TMPENVELOPESEGMENT): HResult; stdcall;
    function FlushEnvelope(dwParamIndex: DWORD; refTimeStart, refTimeEnd: int64): HResult; stdcall;
    function SetTimeFormat(const guidTimeFormat: TGUID; mpTimeData: dword): HResult; stdcall;
  end;*)

  IMediaSample = interface(IUnknown)
    ['{56A8689A-0AD4-11CE-B03A-0020AF0BA770}']
    function GetPointer(out ppBuffer: PBYTE): HRESULT; stdcall;
    function GetSize: LongInt; stdcall;
    function GetTime(out pTimeStart, pTimeEnd: TReference_Time): HRESULT; stdcall;
    function SetTime(pTimeStart, pTimeEnd: PReference_Time): HRESULT; stdcall;
    function IsSyncPoint: HRESULT; stdcall;
    function SetSyncPoint(bIsSyncPoint: BOOL): HRESULT; stdcall;
    function IsPreroll: HRESULT; stdcall;
    function SetPreroll(bIsPreroll: BOOL): HRESULT; stdcall;
    function GetActualDataLength: LongInt; stdcall;
    function SetActualDataLength(lLen: LongInt): HRESULT; stdcall;
    function GetMediaType(out ppMediaType: PAM_Media_Type): HRESULT; stdcall;
    function SetMediaType(var pMediaType: TAM_Media_Type): HRESULT; stdcall;
    function IsDiscontinuity: HRESULT; stdcall;
    function SetDiscontinuity(bDiscontinuity: BOOL): HRESULT; stdcall;
    function GetMediaTime(out pTimeStart, pTimeEnd: TReference_Time): HRESULT; stdcall;
    function SetMediaTime(pTimeStart, pTimeEnd: PReference_Time): HRESULT; stdcall;
  end;

  TALLOCATOR_PROPERTIES = record
    cBuffers: LongInt;
    cbBuffer: LongInt;
    cbAlign: LongInt;
    cbPrefix: LongInt;
  end;

  pDIBData = ^TDIBData;
  TDIBData = record
    PaletteVersion: LongInt;
    Dib_Section: TDIBSECTION;
    Bitmap: hBitmap;
    hMapping: THandle;
    pBase: pByte;
  end;

  ISampleGrabberCB = interface(IUnknown)
    ['{0579154A-2B53-4994-B0D0-E773148EFF85}']
    function  SampleCB(SampleTime: Double; pSample: pointer): HResult; stdcall;
    function  BufferCB(SampleTime: Double; pBuffer: Pointer; BufferLen: LongInt): HResult; stdcall;
  end;

  IMediaControl = interface(IDispatch)
    ['{56A868B1-0AD4-11CE-B03A-0020AF0BA770}']
    function Run: HResult; stdcall;
    function Pause: HResult; stdcall;
    function Stop: HResult; stdcall;
    function GetState(msTimeout: dword; out pfs: TFilter_State): HResult; stdcall;
    function RenderFile(strFilename: WideString): HResult; stdcall;
    function AddSourceFilter(strFilename: WideString; out ppUnk: IDispatch): HResult; stdcall;
    function get_FilterCollection(out ppUnk: IDispatch): HResult; stdcall;
    function get_RegFilterCollection(out ppUnk: IDispatch): HResult; stdcall;
    function StopWhenReady: HResult; stdcall;
  end;

const
  CHARS_IN_GUID   = 39;

  MAX_PIN_NAME    = 128;
  MAX_FILTER_NAME = 128;

(*
const
  DMOCATEGORY_VIDEO_EFFECT         : TGUID = '{d990ee14-776c-4723-be46-3da2f56f10b9}';
  DMOCATEGORY_VIDEO_DECODER        : TGUID = '{4a69b442-28be-4991-969c-b500adf5d8a8}';
  DMOCATEGORY_VIDEO_ENCODER        : TGUID = '{33D9A760-90C8-11d0-BD43-00A0C911CE86}';

  DMOCATEGORY_AUDIO_DECODER        : TGUID = '{57f2db8b-e6bb-4513-9d43-dcd2a6593125}';
  DMOCATEGORY_AUDIO_ENCODER        : TGUID = '{33D9A761-90C8-11d0-BD43-00A0C911CE86}';
  DMOCATEGORY_AUDIO_EFFECT         : TGUID = '{f3602b3f-0592-48df-a4cd-674721e7ebeb}';
  DMOCATEGORY_AUDIO_CAPTURE_EFFECT : TGUID = '{f665aaba-3e09-4920-aa5f-219811148f09}';

  DMO_REGISTERF_IS_KEYED = 1;
  DMO_ENUMF_INCLUDE_KEYED = 1;

type
  PDMOPartialMediaType = ^TDMOPartialMediaType;
  TDMOPartialMediaType = record
    MajorType    : TGUID;
    SubType  : TGUID;
  end;

  IEnumDMO = interface(IUnknown)
    ['{2c3cd98a-2bfa-4a53-9c27-5249ba64ba0f}']
    function Next(cItemsToFetch: DWORD; out pCLSID: TGUID; out Names: PWideChar; pcItemsFetched: pDWORD): HResult; stdcall;
    function Skip(cItemsToSkip: DWORD): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppEnum: IEnumDMO): HResult; stdcall;
  end;*)

type
  TRefTime = double;

  HSEMAPHORE = LongInt;

  IBaseFilter = interface;

  TPin_Info = record
    pFilter: IBaseFilter;
    dir: TPin_Direction;
    achName: array[0..127] of WCHAR;
  end;

  IEnumMediaTypes = interface;

  IPin = interface(IUnknown)
    ['{56A86891-0AD4-11CE-B03A-0020AF0BA770}']
    function Connect(pReceivePin: IPin; const pmt: PAM_Media_Type): HRESULT; stdcall;
    function ReceiveConnection(pConnector: IPin; const pmt: TAM_Media_Type): HRESULT; stdcall;
    function Disconnect: HRESULT; stdcall;
    function ConnectedTo(out pPin: IPin): HRESULT; stdcall;
    function ConnectionMediaType(pmt: PAM_MEDIA_TYPE): HRESULT; stdcall;
    function QueryPinInfo(out pInfo: TPin_Info): HRESULT; stdcall;
    function QueryDirection(out pPinDir: TPin_Direction): HRESULT; stdcall;
    function QueryId(out Id: pWideChar): HRESULT; stdcall;
    function QueryAccept(pmt: PAM_Media_Type): HRESULT; stdcall;
    function EnumMediaTypes(out ppEnum: IEnumMediaTypes): HRESULT; stdcall;
    function QueryInternalConnections(out apPin: IPin; var nPin: ULONG): HRESULT; stdcall;
    function EndOfStream: HRESULT; stdcall;
    function BeginFlush: HRESULT; stdcall;
    function EndFlush: HRESULT; stdcall;
    function NewSegment(tStart, tStop: TReference_Time; dRate: double): HRESULT; stdcall;
  end;

  IEnumPins = interface(IUnknown)
    ['{56A86892-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cPins: ULONG; out ppPins: IPin; pcFetched: PULONG): HRESULT; stdcall;
    function Skip(cPins: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumPins): HRESULT; stdcall;
  end;

  IEnumMediaTypes = interface(IUnknown)
    ['{89C31040-846B-11CE-97D3-00AA0055595A}']
    function Next(cMediaTypes: ULONG; out ppMediaTypes: PAM_Media_Type;
      pcFetched: PULONG): HRESULT; stdcall;
    function Skip(cMediaTypes: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumMediaTypes): HRESULT; stdcall;
  end;

  IEnumFilters = interface;

  IFilterGraph = interface(IUnknown)
    ['{56A8689F-0AD4-11CE-B03A-0020AF0BA770}']
    function AddFilter(pFilter: IBaseFilter; pName: pWideChar): HRESULT; stdcall;
    function RemoveFilter(pFilter: IBaseFilter): HRESULT; stdcall;
    function EnumFilters(out ppEnum: IEnumFilters): HRESULT; stdcall;
    function FindFilterByName(pName: pWideChar; out ppFilter: IBaseFilter): HRESULT; stdcall;
    function ConnectDirect(ppinOut, ppinIn: IPin; pmt: PAM_Media_Type): HRESULT; stdcall;
    function Reconnect(ppin: IPin): HRESULT; stdcall;
    function Disconnect(ppin: IPin): HRESULT; stdcall;
    function SetDefaultSyncSource: HRESULT; stdcall;
  end;

  IEnumFilters = interface(IUnknown)
    ['{56A86893-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cFilters: ULONG; out ppFilter: IBaseFilter; pcFetched: PULONG): HRESULT; stdcall;
    function Skip(cFilters: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnum: IEnumFilters): HRESULT; stdcall;
  end;

  IReferenceClock = interface;

  IMediaFilter = interface(IPersist)
    ['{56A86899-0AD4-11CE-B03A-0020AF0BA770}']
    function Stop: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Run(tStart: TReference_Time): HRESULT; stdcall;
    function GetState(dwMilliSecsTimeout: dword; out State: TFilter_State): HRESULT; stdcall;
    function SetSyncSource(pClock: IReferenceClock): HRESULT; stdcall;
    function GetSyncSource(out pClock: IReferenceClock): HRESULT; stdcall;
  end;

  TFilterInfo = record
    achName: array[0..127] of WCHAR;
    pGraph: IFilterGraph;
  end;

  IBaseFilter = interface(IMediaFilter)
    ['{56A86895-0AD4-11CE-B03A-0020AF0BA770}']
    function EnumPins(out ppEnum: IEnumPins): HRESULT; stdcall;
    function FindPin(Id: pWideChar; out ppPin: IPin): HRESULT; stdcall;
    function QueryFilterInfo(out pInfo: TFilterInfo): HRESULT; stdcall;
    function JoinFilterGraph(pGraph: IFilterGraph; pName: pWideChar): HRESULT; stdcall;
    function QueryVendorInfo(out pVendorInfo: pWideChar): HRESULT; stdcall;
  end;

  IReferenceClock = interface(IUnknown)
    ['{56A86897-0AD4-11CE-B03A-0020AF0BA770}']
    function GetTime(out pTime: TReference_Time): HRESULT; stdcall;
    function AdviseTime(baseTime, streamTime: TReference_Time;
        hEvent: THandle; out pdwAdviseCookie: dword): HRESULT; stdcall;
    function AdvisePeriodic(startTime, periodTime: TReference_Time;
        hSemaphore: HSEMAPHORE; out pdwAdviseCookie: dword): HRESULT; stdcall;
    function Unadvise(dwAdviseCookie: dword): HRESULT; stdcall;
  end;

  IAMClockSlave = interface(IUnknown)
    ['{9FD52741-176D-4b36-8F51-CA8F933223BE}']
    function SetErrorTolerance(dwTolerance: DWORD): HResult; stdcall;
    function GetErrorTolerance(out dwTolerance: DWORD): HResult; stdcall;
  end;

  IAMOpenProgress = interface(IUnknown)
    ['{8E1C39A1-DE53-11cf-AA63-0080C744528D}']
    function QueryProgress(out pllTotal, pllCurrent: int64): HResult; stdcall;
    function AbortOperation: HResult; stdcall;
  end;

  PAM_Sample2_Properties = ^TAM_Sample2_Properties;
  TAM_Sample2_Properties = record
    cbData: dword;
    dwTypeSpecificFlags: dword;
    dwSampleFlags: dword;
    lActual: LongInt;
    tStart: TReference_Time;
    tStop: TReference_Time;
    dwStreamId: dword;
    pMediaType: PAM_Media_Type;
    pbBuffer: Pointer;
    cbBuffer: LongInt;
  end;

const
  AM_SEEKING_NoPositioning          = 0;
  AM_SEEKING_AbsolutePositioning    = $1;
  AM_SEEKING_RelativePositioning    = $2;
  AM_SEEKING_IncrementalPositioning = $3;
  AM_SEEKING_PositioningBitsMask    = $3;
  AM_SEEKING_SeekToKeyFrame         = $4;
  AM_SEEKING_ReturnTime             = $8;
  AM_SEEKING_Segment                = $10;
  AM_SEEKING_NoFlush                = $20;

  AM_SEEKING_CanSeekAbsolute        = $1;
  AM_SEEKING_CanSeekForwards        = $2;
  AM_SEEKING_CanSeekBackwards       = $4;
  AM_SEEKING_CanGetCurrentPos       = $8;
  AM_SEEKING_CanGetStopPos          = $10;
  AM_SEEKING_CanGetDuration         = $20;
  AM_SEEKING_CanPlayBackwards       = $40;
  AM_SEEKING_CanDoSegments          = $80;
  AM_SEEKING_Source                 = $100;

type
  IMediaSeeking = interface(IUnknown)
    ['{36B73880-C2C8-11CF-8B46-00805F6CEF60}']
    function GetCapabilities(out pCapabilities: dword): HRESULT; stdcall;
    function CheckCapabilities(pCapabilities: pdword): HRESULT; stdcall;
    function IsFormatSupported(const pFormat: TGUID): HRESULT; stdcall;
    function QueryPreferredFormat(out pFormat: TGUID): HRESULT; stdcall;
    function GetTimeFormat(out pFormat: TGUID): HRESULT; stdcall;
    function IsUsingTimeFormat(const pFormat: TGUID): HRESULT; stdcall;
    function SetTimeFormat(const pFormat: TGUID): HRESULT; stdcall;
    function GetDuration(out pDuration: int64): HRESULT; stdcall;
    function GetStopPosition(out pStop: int64): HRESULT; stdcall;
    function GetCurrentPosition(out pCurrent: int64): HRESULT; stdcall;
    function ConvertTimeFormat(out pTarget: int64; pTargetFormat: PGUID;
               Source: int64; pSourceFormat: PGUID): HRESULT; stdcall;
    function SetPositions(pCurrent: pint64; dwCurrentFlags: dword;
               pStop: pint64; dwStopFlags: dword): HRESULT; stdcall;
    function GetPositions(out pCurrent, pStop: int64): HRESULT; stdcall;
    function GetAvailable(out pEarliest, pLatest: int64): HRESULT; stdcall;
    function SetRate(dRate: double): HRESULT; stdcall;
    function GetRate(out pdRate: double): HRESULT; stdcall;
    function GetPreroll(out pllPreroll: int64): HRESULT; stdcall;
  end;

const
  AM_MEDIAEVENT_NONOTIFY = $01;

  REG_PINFLAG_B_ZERO     = $1;
  REG_PINFLAG_B_RENDERER = $2;
  REG_PINFLAG_B_MANY     = $4;
  REG_PINFLAG_B_OUTPUT   = $8;

const
  CK_NOCOLORKEY = $0;
  CK_INDEX      = $1;
  CK_RGB        = $2;

type
  pColorKey = ^TColorKey;
  TColorKey = record
    KeyType: dword;
    PaletteIndex: dword;
    LowColorValue: COLORREF;
    HighColorValue: COLORREF;
  end;

type
  HMonitor = THandle;

  IOverlayNotify = interface(IUnknown)
    ['{56A868A0-0AD4-11CE-B03A-0020AF0BA770}']
    function OnPaletteChange(dwColors: dword; const pPalette: TPALETTEENTRY): HRESULT; stdcall;
    function OnClipChange(const pSourceRect, pDestinationRect: TRect;
        const pRgnData: TRgnData): HRESULT; stdcall;
    function OnColorKeyChange(const pColorKey: TColorKey): HRESULT; stdcall;
    function OnPositionChange(const pSourceRect, pDestinationRect: TRect): HRESULT; stdcall;
  end;

  IOverlayNotify2 = interface(IOverlayNotify)
    ['{680EFA10-D535-11D1-87C8-00A0C9223196}']
    function OnDisplayChange(var hMonitor: HMONITOR ): HRESULT; stdcall;
  end;

  IOverlay = interface(IUnknown)
    ['{56A868A1-0AD4-11CE-B03A-0020AF0BA770}']
    function GetPalette(out pdwColors: dword; out ppPalette: PPALETTEENTRY): HRESULT; stdcall;
    function SetPalette(dwColors: dword; pPalette: pPaletteEntry): HRESULT; stdcall;
    function GetDefaultColorKey(out pColorKey: TColorKey): HRESULT; stdcall;
    function GetColorKey(out pColorKey: TColorKey): HRESULT; stdcall;
    function SetColorKey(ColorKey: pColorKey): HRESULT; stdcall;
    function GetWindowHandle(out pHwnd: HWND): HRESULT; stdcall;
    function GetClipList(out pSourceRect, pDestinationRect: TRect;
        out ppRgnData: PRgnData): HRESULT; stdcall;
    function GetVideoPosition(out pSourceRect, pDestinationRect: TRect): HRESULT; stdcall;
    function Advise(pOverlayNotify: IOverlayNotify; dwInterests: dword): HRESULT; stdcall;
    function Unadvise: HRESULT; stdcall;
  end;

  IMediaEventSink = interface(IUnknown)
    ['{56A868A2-0AD4-11CE-B03A-0020AF0BA770}']
    function Notify(EventCode, EventParam1, EventParam2: LongInt): HRESULT; stdcall;
  end;

  IFileSourceFilter = interface(IUnknown)
    ['{56A868A6-0AD4-11CE-B03A-0020AF0BA770}']
    function Load(pszFileName: pWideChar; const pmt: PAM_Media_Type): HRESULT; stdcall;
    function GetCurFile(out ppszFileName: pWideChar; out pmt: TAM_Media_Type): HRESULT; stdcall;
  end;

  IFileSinkFilter = interface(IUnknown)
    ['{A2104830-7C70-11CF-8BCE-00AA00A3F1A6}']
    function SetFileName(pszFileName: pWideChar; pmt: PAM_Media_Type): HRESULT; stdcall;
    function GetCurFile(out ppszFileName: pWideChar; out pmt: TAM_Media_Type): HRESULT; stdcall;
  end;

  IFileSinkFilter2 = interface(IFileSinkFilter)
    ['{00855B90-CE1B-11D0-BD4F-00A0C911CE86}']
    function SetMode(dwFlags: dword): HRESULT; stdcall;
    function GetMode(out pdwFlags: dword): HRESULT; stdcall;
  end;


  IGraphBuilder = interface(IFilterGraph)
    ['{56A868A9-0AD4-11CE-B03A-0020AF0BA770}']
    function Connect(ppinOut, ppinIn: IPin): HRESULT; stdcall;
    function Render(ppinOut: IPin): HRESULT; stdcall;
    function RenderFile(lpcwstrFile, lpcwstrPlayList: pWideChar): HRESULT; stdcall;
    function AddSourceFilter(lpcwstrFileName, lpcwstrFilterName: LPCWSTR;
        out ppFilter: IBaseFilter): HRESULT; stdcall;
    function SetLogFile(hFile: THandle): HRESULT; stdcall;
    function Abort: HRESULT; stdcall;
    function ShouldOperationContinue: HRESULT; stdcall;
  end;

  IGraphConfigCallback = interface(IUnknown)
    ['{ade0fd60-d19d-11d2-abf6-00a0c905f375}']
    function Reconfigure(var pvContext; dwFlags: dword): HRESULT; stdcall;
  end;

  IPinConnection = interface(IUnknown)
    ['{4a9a62d3-27d4-403d-91e9-89f540e55534}']
    function DynamicQueryAccept(var pmt: TAM_MEDIA_TYPE): HRESULT; stdcall;
    function NotifyEndOfStream(hNotifyEvent: THANDLE): HRESULT; stdcall;
    function IsEndPin: HRESULT; stdcall;
    function DynamicDisconnect: HRESULT; stdcall;
  end;

  IGraphConfig = interface(IUnknown)
    ['{03A1EB8E-32BF-4245-8502-114D08A9CB88}']
    function Reconnect(pOutputPin, pInputPin: IPin; pmtFirstConnection: PAM_MEDIA_TYPE;
             pUsingFilter: IBaseFilter; hAbortEvent: THANDLE; dwFlags: dword): HRESULT; stdcall;
    function Reconfigure(pCallback: IGraphConfigCallback; var pvContext;
             dwFlags: dword; hAbortEvent: THANDLE): HRESULT; stdcall;
    function AddFilterToCache(pFilter: IBaseFilter): HRESULT; stdcall;
    function EnumCacheFilter(out pEnum: IEnumFilters): HRESULT; stdcall;
    function RemoveFilterFromCache(pFilter: IBaseFilter): HRESULT; stdcall;
    function GetStartTime(out prtStart: TREFERENCE_TIME): HRESULT; stdcall;
    function PushThroughData(pOutputPin: IPin; pConnection: IPinConnection; hEventAbort: PHANDLE): HRESULT; stdcall;
    function SetFilterFlags(pFilter: IBaseFilter; dwFlags: dword): HRESULT; stdcall;
    function GetFilterFlags(pFilter: IBaseFilter; out pdwFlags: dword): HRESULT; stdcall;
    function RemoveFilterEx(pFilter: IBaseFilter; Flags: dword): HRESULT; stdcall;
  end;

const
  LOOK_UPSTREAM_ONLY: TGUID = (D1:$AC798BE0;D2:$98E3;D3:$11d1;D4:($B3,$F1,$00,$AA,$00,$37,$61,$C5));
  LOOK_DOWNSTREAM_ONLY: TGUID = (D1:$AC798BE1;D2:$98E3;D3:$11d1;D4:($B3,$F1,$00,$AA,$00,$37,$61,$C5));

type
  TDwordLong = record
     h: LongWord;
     l: LongWord;
  end;

  IAMCopyCaptureFileProgress = interface;

  ICaptureGraphBuilder = interface(IUnknown)
    ['{BF87B6E0-8C27-11D0-B3F0-00AA003761C5}']
    function SetFiltergraph(pfg: IGraphBuilder): HRESULT; stdcall;
    function GetFiltergraph(out ppfg: IGraphBuilder): HRESULT; stdcall;
    function SetOutputFileName(const pType: TGUID; lpstrFile: POLESTR; out ppf: IBaseFilter; out ppSink: IFileSinkFilter): HRESULT; stdcall;
    function FindInterface(pCategory: PGUID; pf: IBaseFilter; const riid: TGUID; out ppint): HRESULT; stdcall;
    function RenderStream(pCategory: PGUID; pSource: IUnknown; pfCompressor, pfRenderer: IBaseFilter): HRESULT; stdcall;
    function ControlStream(pCategory: PGUID; pFilter: IBaseFilter; pstart, pstop: PREFERENCE_TIME; wStartCookie, wStopCookie: WORD): HRESULT; stdcall;
    function AllocCapFile(lpstr: POLESTR; dwlSize: int64): HRESULT; stdcall;
    function CopyCaptureFile(lpwstrOld, lpwstrNew: POLESTR; fAllowEscAbort: LongInt; pCallback: IAMCopyCaptureFileProgress): HRESULT; stdcall;
  end;

  ICaptureGraphBuilder2 = interface(IUnknown)
    ['{93E5A4E0-2D50-11d2-ABFA-00A0C9C6E38D}']
    function SetFiltergraph(pfg: IGraphBuilder): HRESULT; stdcall;
    function GetFiltergraph(out Muxg: IGraphBuilder): HRESULT; stdcall;
    function SetOutputFileName(pType: pGUID; lpstrFile: pWideChar; out Mux: IBaseFilter; out ppSink: IFileSinkFilter): HRESULT; stdcall;
    function FindInterface(pCategory, pType: PGUID; pf: IBaseFilter; const riid: TGUID; out ppint): HRESULT; stdcall;
    function RenderStream(pCategory, pType: PGUID; pSource: IUnknown; pfCompressor, pfRenderer: IBaseFilter): HRESULT; stdcall;
    function ControlStream(pCategory, pType: PGUID; pFilter: IBaseFilter; pstart, pstop: PREFERENCE_TIME; wStartCookie, wStopCookie: WORD ): HRESULT; stdcall;
    function AllocCapFile(lpstr: pWideChar; dwlSize: TDwordLong): HRESULT; stdcall;
    function CopyCaptureFile(lpwstrOld, lpwstrNew: pWideChar; fAllowEscAbort: LongInt; pCallback: IAMCopyCaptureFileProgress): HRESULT; stdcall;
    function FindPin(pSource: IUnknown; pindir: TPIN_DIRECTION; pCategory, pType: pointer; fUnconnected: BOOL; num: LongInt; out ppPin: IPin): HRESULT; stdcall;
  end;


  IAMCopyCaptureFileProgress = interface(IUnknown)
    ['{670D1D20-A068-11D0-B3F0-00AA003761C5}']
    function Progress(iProgress: LongInt): HRESULT; stdcall;
  end;

const
  AM_RENDEREX_RENDERTOEXISTINGRENDERERS = $01;

type

 {$IFDEF BCB}

  IEnumMoniker = interface;
  IBindCtx = interface;

  PIMoniker = ^IMoniker;

  IEnumString = interface(IUnknown)
    ['{00000101-0000-0000-C000-000000000046}']
    function Next(celt: LongInt; out elt;
      pceltFetched: PLongInt): HResult; stdcall;
    function Skip(celt: LongInt): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumString): HResult; stdcall;
  end;

  IMoniker = interface(IPersistStream)
    ['{0000000F-0000-0000-C000-000000000046}']
    function BindToObject(const bc: IBindCtx; const mkToLeft: IMoniker;
      const iidResult: TIID; out vResult): HResult; stdcall;
    function BindToStorage(const bc: IBindCtx; const mkToLeft: IMoniker;
      const iid: TIID; out vObj): HResult; stdcall;
    function Reduce(const bc: IBindCtx; dwReduceHowFar: LongInt;
      mkToLeft: PIMoniker; out mkReduced: IMoniker): HResult; stdcall;
    function ComposeWith(const mkRight: IMoniker; fOnlyIfNotGeneric: BOOL;
      out mkComposite: IMoniker): HResult; stdcall;
    function Enum(fForward: BOOL; out enumMoniker: IEnumMoniker): HResult;
      stdcall;
    function IsEqual(const mkOtherMoniker: IMoniker): HResult; stdcall;
    function Hash(out dwHash: LongInt): HResult; stdcall;
    function IsRunning(const bc: IBindCtx; const mkToLeft: IMoniker;
      const mkNewlyRunning: IMoniker): HResult; stdcall;
    function GetTimeOfLastChange(const bc: IBindCtx; const mkToLeft: IMoniker;
      out filetime: TFileTime): HResult; stdcall;
    function Inverse(out mk: IMoniker): HResult; stdcall;
    function CommonPrefixWith(const mkOther: IMoniker;
      out mkPrefix: IMoniker): HResult; stdcall;
    function RelativePathTo(const mkOther: IMoniker;
      out mkRelPath: IMoniker): HResult; stdcall;
    function GetDisplayName(const bc: IBindCtx; const mkToLeft: IMoniker;
      out pszDisplayName: pWideChar): HResult; stdcall;
    function ParseDisplayName(const bc: IBindCtx; const mkToLeft: IMoniker;
      pszDisplayName: pWideChar; out chEaten: LongInt;
      out mkOut: IMoniker): HResult; stdcall;
    function IsSystemMoniker(out dwMksys: LongInt): HResult; stdcall;
  end;

  IEnumMoniker = interface(IUnknown)
    ['{00000102-0000-0000-C000-000000000046}']
    function Next(celt: LongInt; out elt;
      pceltFetched: PLongInt): HResult; stdcall;
    function Skip(celt: LongInt): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumMoniker): HResult; stdcall;
  end;

  IRunningObjectTable = interface(IUnknown)
    ['{00000010-0000-0000-C000-000000000046}']
    function Register(grfFlags: LongInt; const unkObject: IUnknown;
      const mkObjectName: IMoniker; out dwRegister: LongInt): HResult; stdcall;
    function Revoke(dwRegister: LongInt): HResult; stdcall;
    function IsRunning(const mkObjectName: IMoniker): HResult; stdcall;
    function GetObject(const mkObjectName: IMoniker;
      out unkObject: IUnknown): HResult; stdcall;
    function NoteChangeTime(dwRegister: LongInt;
      const filetime: TFileTime): HResult; stdcall;
    function GetTimeOfLastChange(const mkObjectName: IMoniker;
      out filetime: TFileTime): HResult; stdcall;
    function EnumRunning(out enumMoniker: IEnumMoniker): HResult; stdcall;
  end;

  PBindOpts = ^TBindOpts;

  tagBIND_OPTS = record
    cbStruct: LongInt;
    grfFlags: LongInt;
    grfMode: LongInt;
    dwTickCountDeadline: LongInt;
  end;
  TBindOpts = tagBIND_OPTS;

  BIND_OPTS = TBindOpts;

  IBindCtx = interface(IUnknown)
    ['{0000000E-0000-0000-C000-000000000046}']
    function RegisterObjectBound(const unk: IUnknown): HResult; stdcall;
    function RevokeObjectBound(const unk: IUnknown): HResult; stdcall;
    function ReleaseBoundObjects: HResult; stdcall;
    function SetBindOptions(const bindopts: TBindOpts): HResult; stdcall;
    function GetBindOptions(var bindopts: TBindOpts): HResult; stdcall;
    function GetRunningObjectTable(out rot: IRunningObjectTable): HResult;
      stdcall;
    function RegisterObjectParam(pszKey: pWideChar; const unk: IUnknown): HResult;
      stdcall;
    function GetObjectParam(pszKey: pWideChar; out unk: IUnknown): HResult;
      stdcall;
    function EnumObjectParam(out Enum: IEnumString): HResult; stdcall;
    function RevokeObjectParam(pszKey: pWideChar): HResult; stdcall;
  end;
  {$ENDIF BCB}

  PRegPinTypes = ^TRegPinTypes;
  TRegPinTypes = record
    clsMajorType: PGUID;
    clsMinorType: PGUID;
  end;

  PRegFilterPins = ^TRegFilterPins;
  TRegFilterPins = record
    strName: PWideChar;
    bRendered: BOOL;
    bOutput: BOOL;
    bZero: BOOL;
    bMany: BOOL;
    oFilter: PGUID;
    strConnectsToPin: PWideChar;
    nMediaTypes: LongWord;
    lpMediaType: PRegPinTypes;
  end;

  PRegPinMedium = ^TRegPinMedium;
  TRegPinMedium = record
    clsMedium: TGUID;
    dw1: dword;
    dw2: dword;
  end;

(*const
  REG_PINFLAG_B_ZERO     = $1;
  REG_PINFLAG_B_RENDERER = $2;
  REG_PINFLAG_B_MANY     = $4;
  REG_PINFLAG_B_OUTPUT   = $8;

type*)
  PRegFilterPins2 = ^TRegFilterPins2;
  TRegFilterPins2 = record
    dwFlags: dword;
    cInstances: UINT;
    nMediaTypes: UINT;
    lpMediaType: PRegPinTypes;
    lpMedium: PRegPinMedium;
    clsPinCategory: PGUID;
  end;

  TRegFilter2 = record
    dwVersion: dword;
    dwMerit: dword;
    case LongInt of
      0: (
        cPins: ULONG;
        cPins2: ULONG;
      );
      1: (
        rgPins: PRegFilterPins;
        rgPins2: PRegFilterPins2;
      );
  end;


  IFilterMapper2 = interface(IUnknown)
    ['{B79BB0B0-33C1-11D1-ABE1-00A0C905F375}']
    function CreateCategory(const clsidCategory: TGUID; dwCategoryMerit: dword;
        Description: PWideChar): HRESULT; stdcall;
    function UnregisterFilter(const pclsidCategory: TGUID;
        szInstance: PWideChar; const Filter: TGUID): HRESULT; stdcall;
    function RegisterFilter(const clsidFilter: TGUID; Name: PWideChar;
        ppMoniker: IMoniker; pclsidCategory: PGUID;
        szInstance: PWideChar; const prf2: TRegFilter2): HRESULT; stdcall;
    function EnumMatchingFilters(out ppEnum: IEnumMoniker; dwFlags: dword; bExactMatch: BOOL;
        dwMerit: dword; bInputNeeded: BOOL; cInputTypes: dword; pInputTypes: PGUID;
        pMedIn: PREGPINMEDIUM; pPinCategoryIn: PGUID; bRender, bOutputNeeded: BOOL;
        cOutputTypes: dword; pOutputTypes: PGUID; pMedOut: PRegPinMedium;
        pPinCategoryOut: PGUID): HRESULT; stdcall;
  end;


  IFilterGraph2 = interface(IGraphBuilder)
    ['{36B73882-C2C8-11CF-8B46-00805F6CEF60}']
    function AddSourceFilterForMoniker(pMoniker: IMoniker; pCtx: IBindCtx;
        lpcwstrFilterName: LPCWSTR; out ppFilter: IBaseFilter): HRESULT; stdcall;
    function ReconnectEx(ppin: IPin; pmt: PAM_Media_Type): HRESULT; stdcall;
    function RenderEx(pPinOut: IPin; dwFlags: dword; pvContext: Pdword): HRESULT; stdcall;
  end;

  IStreamBuilder = interface(IUnknown)
    ['{56A868BF-0AD4-11CE-B03A-0020AF0BA770}']
    function Render(ppinOut: IPin; pGraph: IGraphBuilder): HRESULT; stdcall;
    function Backout(ppinOut: IPin; pGraph: IGraphBuilder): HRESULT; stdcall;
  end;

const
  AM_STREAM_INFO_START_DEFINED   = $1;
  AM_STREAM_INFO_STOP_DEFINED    = $2;
  AM_STREAM_INFO_DISCARDING      = $4;
  AM_STREAM_INFO_STOP_SEND_EXTRA = $10;

type
  TQualityMessageType = (
    Famine,
    Flood
  );

  TQuality = record
    Typ: TQualityMessageType;
    Proportion: LongInt;
    Late: TReference_Time;
    TimeStamp: TReference_Time;
  end;

  IQualityControl = interface(IUnknown)
    ['{56A868A5-0AD4-11CE-B03A-0020AF0BA770}']
    function Notify(pSelf: IBaseFilter; q: TQuality): HRESULT; stdcall;
    function SetSink(piqc: IQualityControl): HRESULT; stdcall;
  end;



  TAM_Stream_Info = record
    tStart: TReference_Time;
    tStop: TReference_Time;
    dwStartCookie: dword;
    dwStopCookie: dword;
    dwFlags: dword;
  end;

  IAMAudioRendererStats = interface(IUnknown)
    ['{22320CB2-D41A-11d2-BF7C-D7CB9DF0BF93}']
    function GetStatParam(dwParam: DWORD; out pdwParam1, pdwParam2: DWORD): HResult; stdcall;
  end;

  IAMAudioInputMixer = interface(IUnknown)
    ['{54C39221-8380-11d0-B3F0-00AA003761C5}']
    function put_Enable(fEnable: BOOL): HRESULT; stdcall;
    function get_Enable(out pfEnable: BOOL): HRESULT; stdcall;
    function put_Mono(fMono: BOOL): HRESULT; stdcall;
    function get_Mono(out pfMono: BOOL): HRESULT; stdcall;
    function put_MixLevel(Level: double): HRESULT; stdcall;
    function get_MixLevel(out pLevel: double): HRESULT; stdcall;
    function put_Pan(Pan: double): HRESULT; stdcall;
    function get_Pan(out pPan: double): HRESULT; stdcall;
    function put_Loudness(fLoudness: BOOL): HRESULT; stdcall;
    function get_Loudness(out pfLoudness: BOOL): HRESULT; stdcall;
    function put_Treble(Treble: double): HRESULT; stdcall;
    function get_Treble(out pTreble: double): HRESULT; stdcall;
    function get_TrebleRange(out pRange: double): HRESULT; stdcall;
    function put_Bass(Bass: double): HRESULT; stdcall;
    function get_Bass(out pBass: double): HRESULT; stdcall;
    function get_BassRange(out pRange: double): HRESULT; stdcall;
  end;
  
  IAMStreamControl = interface(IUnknown)
    ['{36b73881-c2c8-11cf-8b46-00805f6cef60}']
    function StartAt(ptStart: PReference_Time; dwCookie: dword): HRESULT; stdcall;
    function StopAt(ptStop: PReference_Time; bSendExtra: BOOL;
        dwCookie: dword): HRESULT; stdcall;
    function GetInfo(out pInfo: TAM_Stream_Info): HRESULT; stdcall;
  end;

  PVideo_Stream_Config_Caps = ^TVideo_Stream_Config_Caps;
  TVideo_Stream_Config_Caps = record
    guid: TGUID;
    VideoStandard: ULONG;
    InputSize: TSize;
    MinCroppingSize: TSize;
    MaxCroppingSize: TSize;
    CropGranularityX: LongInt;
    CropGranularityY: LongInt;
    CropAlignX: LongInt;
    CropAlignY: LongInt;
    MinOutputSize: TSize;
    MaxOutputSize: TSize;
    OutputGranularityX: LongInt;
    OutputGranularityY: LongInt;
    StretchTapsX: LongInt;
    StretchTapsY: LongInt;
    ShrinkTapsX: LongInt;
    ShrinkTapsY: LongInt;
    MinFrameInterval: Int64;
    MaxFrameInterval: Int64;
    MinBitsPerSecond: LongInt;
    MaxBitsPerSecond: LongInt;
  end;

  TAudio_Stream_Config_Caps = record
    guid: TGUID;
    MinimumChannels: ULONG;
    MaximumChannels: ULONG;
    ChannelsGranularity: ULONG;
    MinimumBitsPerSample: ULONG;
    MaximumBitsPerSample: ULONG;
    BitsPerSampleGranularity: ULONG;
    MinimumSampleFrequency: ULONG;
    MaximumSampleFrequency: ULONG;
    SampleFrequencyGranularity: ULONG;
  end;

  IAMStreamConfig = interface(IUnknown)
    ['{C6E13340-30AC-11d0-A18C-00A0C9118956}']
    function SetFormat(pmt: PAM_Media_Type): HRESULT; stdcall;
    function GetFormat(out ppmt: PAM_Media_Type): HRESULT; stdcall;
    function GetNumberOfCapabilities(out piCount, piSize: LongInt): HRESULT; stdcall;
    function GetStreamCaps(iIndex: LongInt; out ppmt: PAM_Media_Type; pSCC: PVideo_Stream_Config_Caps): HRESULT; stdcall;
  end;

  IAMLatency = interface(IUnknown)
    ['{62EA93BA-EC62-11d2-B770-00C04FB6BD3D}']
    function GetLatency(var prtLatency: TREFERENCE_TIME): HRESULT; stdcall;
  end;

  IAMPushSource = interface(IAMLatency)
    ['{F185FE76-E64E-11d2-B76E-00C04FB6BD3D}']
    function GetPushSourceFlags(out pFlags: ULONG): HRESULT; stdcall;
    function SetPushSourceFlags(Flags: ULONG): HRESULT; stdcall;
    function SetStreamOffset(rtOffset: TREFERENCE_TIME): HRESULT; stdcall;
    function GetStreamOffset(out prtOffset: TREFERENCE_TIME): HRESULT; stdcall;
    function GetMaxStreamOffset(out prtMaxOffset: TREFERENCE_TIME): HRESULT; stdcall;
    function SetMaxStreamOffset(rtMaxOffset: TREFERENCE_TIME): HRESULT; stdcall;
  end;

  IAMGraphStreams = interface(IUnknown)
    ['{632105FA-072E-11d3-8AF9-00C04FB6BD3D}']
    function FindUpstreamInterface(pPin: IPin; const riid: TGUID; out ppvInterface;
             dwFlags: dword): HRESULT; stdcall;
    function SyncUsingStreamOffset(bUseStreamOffset: BOOL): HRESULT; stdcall;
    function SetMaxGraphLatency(rtMaxGraphLatency: TREFERENCE_TIME): HRESULT; stdcall;
  end;

  TInterleavingMode = (
    im_None,
    im_Capture,
    im_Full,
    im_none_buffered
  );

  IConfigInterleaving = interface(IUnknown)
    ['{BEE3D220-157B-11d0-BD23-00A0C911CE86}']
    function put_Mode(mode: TInterleavingMode): HRESULT; stdcall;
    function get_Mode(out pMode: TInterleavingMode): HRESULT; stdcall;
    function put_Interleaving(prtInterleave, prtPreroll: pointer): HRESULT; stdcall;
    function get_Interleaving(out prtInterleave, prtPreroll: TReference_Time): HRESULT; stdcall;
  end;

  IConfigAviMux = interface(IUnknown)
    ['{5ACD6AA0-F482-11ce-8B67-00AA00A3F1A6}']
    function SetMasterStream(iStream: LongInt): HRESULT; stdcall;
    function GetMasterStream(out pStream: LongInt): HRESULT; stdcall;
    function SetOutputCompatibilityIndex(fOldIndex: BOOL): HRESULT; stdcall;
    function GetOutputCompatibilityIndex(out pfOldIndex: BOOL): HRESULT; stdcall;
  end;

  ///////////////////////// ASF

  pWMWriterStatistics = ^TWMWriterStatistics;
  TWMWriterStatistics = packed record
    qwSampleCount        : Int64;
    qwByteCount          : Int64;

    qwDroppedSampleCount : Int64;
    qwDroppedByteCount   : Int64;

    dwCurrentBitrate     : LongWord;
    dwAverageBitrate     : LongWord;
    dwExpectedBitrate    : LongWord;

    //
    // Sample rates are given as 1000 * (samples / second).
    //
    dwCurrentSampleRate  : LongWord;
    dwAverageSampleRate  : LongWord;
    dwExpectedSampleRate : LongWord;
  end;

  PWMMediaType = ^TWMMediaType;
  TWMMediaType = packed record
    majortype            : TGUID;
    subtype              : TGUID;
    bFixedSizeSamples    : BOOL;
    bTemporalCompression : BOOL;
    lSampleSize          : dword;
    formattype           : TGUID;
    pUnk                 : IUnknown;
    cbFormat             : dword;
    pbFormat             : pByte;
  end;

  TWMTAttrDataType = (
    WMT_TYPE_DWORD,
    WMT_TYPE_STRING,
    WMT_TYPE_BINARY,
    WMT_TYPE_BOOL,
    WMT_TYPE_QWORD,
    WMT_TYPE_WORD,
    WMT_TYPE_GUID
  );

  TWMTStatus = (
    WMT_ERROR,
    WMT_OPENED,
    WMT_BUFFERING_START,
    WMT_BUFFERING_STOP,
    WMT_END_OF_FILE,
    WMT_END_OF_SEGMENT,
    WMT_END_OF_STREAMING,
    WMT_LOCATING,
    WMT_CONNECTING,
    WMT_NO_RIGHTS,
    WMT_MISSING_CODEC,
    WMT_STARTED,
    WMT_STOPPED,
    WMT_CLOSED,
    WMT_STRIDING,
    WMT_TIMER,
    WMT_INDEX_PROGRESS,
    WMT_SAVEAS_START,
    WMT_SAVEAS_STOP,
    WMT_NEW_SOURCEFLAGS,
    WMT_NEW_METADATA,
    WMT_BACKUPRESTORE_BEGIN,
    WMT_SOURCE_SWITCH,
    WMT_ACQUIRE_LICENSE,
    WMT_INDIVIDUALIZE,
    WMT_NEEDS_INDIVIDUALIZATION,
    WMT_NO_RIGHTS_EX,
    WMT_BACKUPRESTORE_END,
    WMT_BACKUPRESTORE_CONNECTING,
    WMT_BACKUPRESTORE_DISCONNECTING,
    WMT_ERROR_WITHURL,
    WMT_RESTRICTED_LICENSE,
    WMT_CLIENT_CONNECT,
    WMT_CLIENT_DISCONNECT,
    WMT_NATIVE_OUTPUT_PROPS_CHANGED,
    WMT_RECONNECT_START,
    WMT_RECONNECT_END,
    WMT_CLIENT_CONNECT_EX,
    WMT_CLIENT_DISCONNECT_EX,
    WMT_SET_FEC_SPAN,
    WMT_PREROLL_READY,
    WMT_PREROLL_COMPLETE,
    WMT_CLIENT_PROPERTIES,
    WMT_LICENSEURL_SIGNATURE_STATE
  );

  TWMTTransportType = (
   WMT_Transport_Type_Unreliable,
   WMT_Transport_Type_Reliable
  );

  TWMTNetProtocol = (
    WMT_PROTOCOL_HTTP
  );

  IWMStreamConfig = interface(IUnknown)
  ['{96406BDC-2B2B-11d3-B36B-00C04F6108FF}']
    function GetStreamType(out pguidStreamType: TGUID): HRESULT; stdcall;
    function GetStreamNumber(out pwStreamNum: Word): HRESULT; stdcall;
    function SetStreamNumber(wStreamNum: Word): HRESULT; stdcall;
    function GetStreamName(pwszStreamName: PWideChar; var pcchStreamName: Word): HRESULT; stdcall;
    function SetStreamName(pwszStreamName: PWideChar): HRESULT; stdcall;
    function GetConnectionName(pwszInputName: PWideChar; var pcchInputName: Word): HRESULT; stdcall;
    function SetConnectionName(pwszInputName: PWideChar): HRESULT; stdcall;
    function GetBitrate(out pdwBitrate: LongWord): HRESULT; stdcall;
    function SetBitrate(pdwBitrate: LongWord): HRESULT; stdcall;
    function GetBufferWindow(out pmsBufferWindow: LongWord): HRESULT; stdcall;
    function SetBufferWindow(msBufferWindow: LongWord): HRESULT; stdcall;
  end;

  IWMStreamConfig2 = interface(IWMStreamConfig)
  ['{7688D8CB-FC0D-43BD-9459-5A8DEC200CFA}']
    function GetTransportType (out pnTransportType: TWMTTransportType): HRESULT; stdcall;
    function SetTransportType(nTransportType: TWMTTransportType): HRESULT; stdcall;
    function AddDataUnitExtension(const guidExtensionSystemID: TGUID; cbExtensionDataSize: Word; pbExtensionSystemInfo: PBYTE; cbExtensionSystemInfo: LongWord): HRESULT; stdcall;
    function GetDataUnitExtensionCount(out pcDataUnitExtensions: Word): HRESULT; stdcall;
    function GetDataUnitExtension(wDataUnitExtensionNumber: Word;out pguidExtensionSystemID: TGUID; out pcbExtensionDataSize: Word;  pbExtensionSystemInfo: PBYTE; var pcbExtensionSystemInfo: LongWord): HRESULT; stdcall;
    function RemoveAllDataUnitExtensions: HRESULT; stdcall;
  end;

  IWMStreamList = interface(IUnknown)
  ['{96406BDD-2B2B-11d3-B36B-00C04F6108FF}']
    function GetStreams(pwStreamNumArray: PWORD; var pcStreams: PWORD): HRESULT; stdcall;
    function AddStream(wStreamNum: Word): HRESULT; stdcall;
    function RemoveStream(wStreamNum: Word): HRESULT; stdcall;
  end;


  IWMMutualExclusion = interface(IWMStreamList)
  ['{96406BDE-2B2B-11d3-B36B-00C04F6108FF}']
    function GetType(out pguidType: TGUID): HRESULT; stdcall;
    function SetType(guidType: TGUID): HRESULT; stdcall;
  end;

  IWMMutualExclusion2 = interface(IWMMutualExclusion)
  ['{0302B57D-89D1-4ba2-85C9-166F2C53EB91}']
    function GetName(pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function SetName(pwszName: PWideChar): HRESULT; stdcall;
    function GetRecordCount(out pwRecordCount: Word): HRESULT; stdcall;
    function AddRecord: HRESULT; stdcall;
    function RemoveRecord(wRecordNumber: Word ): HRESULT; stdcall;
    function GetRecordName(wRecordNumber: Word; pwszRecordName: PWideChar; var pcchRecordName: Word): HRESULT; stdcall;
    function SetRecordName(wRecordNumber: Word; pwszRecordName: PWideChar): HRESULT; stdcall;
    function GetStreamsForRecord(wRecordNumber: Word; pwStreamNumArray: PWORD; var pcStreams: Word): HRESULT; stdcall;
    function AddStreamForRecord(wRecordNumber, wStreamNumber: Word): HRESULT; stdcall;
    function RemoveStreamForRecord(wRecordNumber, wStreamNumber: Word): HRESULT; stdcall;
  end;

  IWMProfile = interface(IUnknown)
  ['{96406BDB-2B2B-11d3-B36B-00C04F6108FF}']
    function GetVersion(out pdwVersion: LongWord): HRESULT; stdcall;
    function GetName(pwszName: PWideChar; var pcchName: LongWord): HRESULT; stdcall;
    function SetName(pwszName: PWideChar): HRESULT; stdcall;
    function GetDescription(pwszDescription: PWideChar; var pcchDescription: LongWord): HRESULT; stdcall;
    function SetDescription(pwszDescription: PWideChar): HRESULT; stdcall;
    function GetStreamCount(out pcStreams: LongWord): HRESULT; stdcall;
    function GetStream(dwStreamIndex: LongWord; out ppConfig: IWMStreamConfig): HRESULT; stdcall;
    function GetStreamByNumber(wStreamNum: Word; out ppConfig: IWMStreamConfig): HRESULT; stdcall;
    function RemoveStream(pConfig: IWMStreamConfig): HRESULT; stdcall;
    function RemoveStreamByNumber(wStreamNum: Word): HRESULT; stdcall;
    function AddStream(pConfig: IWMStreamConfig): HRESULT; stdcall;
    function ReconfigStream(pConfig: IWMStreamConfig): HRESULT; stdcall;
    function CreateNewStream(const guidStreamType: TGUID; out ppConfig: IWMStreamConfig): HRESULT; stdcall;
    function GetMutualExclusionCount(out pcME: LongWord): HRESULT; stdcall;
    function GetMutualExclusion(dwMEIndex: LongWord; out ppME: IWMMutualExclusion): HRESULT; stdcall;
    function RemoveMutualExclusion(pME: IWMMutualExclusion): HRESULT; stdcall;
    function AddMutualExclusion(pME: IWMMutualExclusion): HRESULT; stdcall;
    function CreateNewMutualExclusion(out ppME: IWMMutualExclusion): HRESULT; stdcall;
  end;

  IWMProfile2 = interface(IWMProfile)
  ['{07E72D33-D94E-4be7-8843-60AE5FF7E5F5}']
    function GetProfileID(out pguidID: TGUID): HRESULT; stdcall;
  end;

  IConfigAsfWriter = interface(IUnknown)
  ['{45086030-F7E4-486a-B504-826BB5792A3B}']
    function ConfigureFilterUsingProfileId(dwProfileId: LongWord): HRESULT; stdcall;
    function GetCurrentProfileId(out pdwProfileId: LongWord): HRESULT; stdcall;
    function ConfigureFilterUsingProfileGuid(const guidProfile: TGUID): HRESULT; stdcall;
    function GetCurrentProfileGuid(out pProfileGuid: TGUID): HRESULT; stdcall;
    function ConfigureFilterUsingProfile(pProfile: IWMProfile): HRESULT; stdcall;
    function GetCurrentProfile(out ppProfile: IWMProfile): HRESULT; stdcall;
    function SetIndexMode(bIndexFile: BOOL): HRESULT; stdcall;
    function GetIndexMode(out pbIndexFile: BOOL): HRESULT; stdcall;
  end;

  IConfigAsfWriter2 = interface(IConfigAsfWriter)
  ['{7989CCAA-53F0-44f0-884A-F3B03F6AE066}']
    function StreamNumFromPin(pPin: IPin; out pwStreamNum: WORD): HRESULT; stdcall;
    function SetParam(AMASFWriterConfigParam: LongInt; dwParam1, dwParam2 {not used, must be 0}: LongWord): HRESULT; stdcall;
    function GetParam(dwParam: LongWord; out pdwParam1, pdwParam2 {not used, must be 0}: LongWord): HRESULT; stdcall;
    function ResetMultiPassState: HRESULT; stdcall;
  end;

  INSSBuffer = interface(IUnknown)
  ['{E1CD3524-03D7-11d2-9EED-006097D2D7CF}']
    function GetLength(out pdwLength: LongWord): HRESULT; stdcall;
    function SetLength(dwLength: LongWord): HRESULT; stdcall;
    function GetMaxLength(out pdwLength: LongWord): HRESULT; stdcall;
    function GetBuffer(out ppdwBuffer: PBYTE): HRESULT; stdcall;
    function GetBufferAndLength(out ppdwBuffer: PBYTE; out pdwLength: LongWord): HRESULT; stdcall;
  end;

  IWMWriterSink = interface(IUnknown)
    ['{96406BE4-2B2B-11D3-B36B-00C04F6108FF}']
    function OnHeader(pHeader: INSSBuffer): HResult; stdcall;
    function IsRealTime(out pfRealTime: BOOL): HResult; stdcall;
    function AllocateDataUnit(cbDataUnit: LongWord; out ppDataUnit: INSSBuffer): HResult; stdcall;
    function OnDataUnit(pDataUnit: INSSBuffer): HResult; stdcall;
    function OnEndWriting: HResult; stdcall;
  end;

  IWMWriterAdvanced = interface(IUnknown)
    ['{96406BE3-2B2B-11D3-B36B-00C04F6108FF}']
    function GetSinkCount(out pcSinks: LongWord): HResult; stdcall;
    function GetSink(dwSinkNum: LongWord; out ppSink: IWMWriterSink): HResult; stdcall;
    function AddSink(pSink: IWMWriterSink): HResult; stdcall;
    function RemoveSink(pSink: IWMWriterSink): HResult; stdcall;
    function WriteStreamSample(wStreamNum: Word; cnsSampleTime: Int64; msSampleSendTime: LongWord; cnsSampleDuration: Int64; dwFlags: LongWord; pSample: INSSBuffer): HResult; stdcall;    // The writer may be running in real-time. If so, it's interesting to
    function SetLiveSource(fIsLiveSource: BOOL): HResult; stdcall;
    function IsRealTime(out pfRealTime: BOOL): HResult; stdcall;
    function GetWriterTime(out pcnsCurrentTime: Int64): HResult; stdcall;
    function GetStatistics(wStreamNum: Word; out pStats: TWMWriterStatistics): HResult; stdcall;
    function SetSyncTolerance(msWindow: LongWord): HResult; stdcall;
    function GetSyncTolerance(out pmsWindow: LongWord): HResult; stdcall;
  end;

  IWMWriterAdvanced2 = interface(IWMWriterAdvanced)
    ['{962DC1EC-C046-4DB8-9CC7-26CEAE500817}']
    function GetInputSetting(dwInputNum: LongWord; pszName: PWideChar; out pType: TWMTAttrDataType; pValue: PByte; var pcbLength: Word): HResult; stdcall;
    function SetInputSetting(dwInputNum: LongWord; pszName: PWideChar; Type_: TWMTAttrDataType; pValue: PByte; cbLength: Word): HResult; stdcall;
  end;

  IWMMediaProps = interface(IUnknown)
  ['{96406BCE-2B2B-11d3-B36B-00C04F6108FF}']
    function GetType(out pguidType: TGUID): HRESULT; stdcall;
    function GetMediaType(pType: PWMMediaType; var pcbType: LongWord): HRESULT; stdcall;
    function SetMediaType(pType: PWMMediaType): HRESULT; stdcall;
  end;

  IWMInputMediaProps = interface(IWMMediaProps)
  ['{96406BD5-2B2B-11d3-B36B-00C04F6108FF}']
    function GetConnectionName(pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function GetGroupName(pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
  end;

  IWMWriter = interface(IUnknown)
  ['{96406BD4-2B2B-11d3-B36B-00C04F6108FF}']
    function SetProfileByID(const guidProfile: TGUID): HRESULT; stdcall;
    function SetProfile(pProfile: IWMProfile): HRESULT; stdcall;
    function SetOutputFilename(pwszFilename: PWideChar): HRESULT; stdcall;
    function GetInputCount(out pcInputs: LongWord): HRESULT; stdcall;
    function GetInputProps(dwInputNum: LongWord; out ppInput: IWMInputMediaProps): HRESULT; stdcall;
    function SetInputProps(dwInputNum: LongWord; pInput: IWMInputMediaProps): HRESULT; stdcall;
    function GetInputFormatCount(dwInputNumber: LongWord; out pcFormats: LongWord): HRESULT; stdcall;
    function GetInputFormat(dwInputNumber, dwFormatNumber: LongWord; out pProps: IWMInputMediaProps): HRESULT; stdcall;
    function BeginWriting: HRESULT; stdcall;
    function EndWriting: HRESULT; stdcall;
    function AllocateSample(dwSampleSize: LongWord; out ppSample: INSSBuffer): HRESULT; stdcall;
    function WriteSample(dwInputNum: LongWord; cnsSampleTime: Int64; dwFlags: LongWord; pSample: INSSBuffer): HRESULT; stdcall;
    function Flush: HRESULT; stdcall;
  end;


  IWMWriterNetworkSink = interface(IWMWriterSink)
    ['{96406BE7-2B2B-11D3-B36B-00C04F6108FF}']
    function SetMaximumClients(dwMaxClients: LongWord): HResult; stdcall;
    function GetMaximumClients(out pdwMaxClients: LongWord): HResult; stdcall;
    function SetNetworkProtocol(protocol: TWMTNetProtocol): HResult; stdcall;
    function GetNetworkProtocol(out pProtocol: TWMTNetProtocol): HResult; stdcall;
    function GetHostURL(pwszURL: PWideChar; var pcchURL: LongWord): HResult; stdcall;
    function Open(var pdwPortNum: LongWord): HResult; stdcall;
    function Disconnect: HResult; stdcall;
    function Close: HResult; stdcall;
  end;

  IWMProfileManager = interface(IUnknown)
  ['{d16679f2-6ca0-472d-8d31-2f5d55aee155}']
    function CreateEmptyProfile(dwVersion: LongWord; out ppProfile: IWMProfile): HRESULT; stdcall;
    function LoadProfileByID(const guidProfile: TGUID; out ppProfile: IWMProfile): HRESULT; stdcall;
    function LoadProfileByData(pwszProfile: PWideChar; out ppProfile: IWMProfile): HRESULT; stdcall;
    function SaveProfile(pIWMProfile: IWMProfile; pwszProfile: PWideChar; var pdwLength: LongWord): HRESULT; stdcall;
    function GetSystemProfileCount(out pcProfiles: LongWord): HRESULT; stdcall;
    function LoadSystemProfile(dwProfileIndex: LongWord; out ppProfile: IWMProfile): HRESULT; stdcall;
  end;

const
   WMT_VER_4_0	= $40000;
   WMT_VER_7_0	= $70000;
   WMT_VER_8_0	= $80000;
   WMT_VER_9_0	= $90000;

type
  IWMProfileManager2 = interface(IWMProfileManager)
  ['{7A924E51-73C1-494d-8019-23D37ED9B89A}']
    function GetSystemProfileVersion (out pdwVersion: LongWord): HRESULT; stdcall;
    function SetSystemProfileVersion (dwVersion: LongWord): HRESULT; stdcall;
  end;

  IWMWriterPushSink = interface(IWMWriterSink)
    ['{DC10E6A5-072C-467D-BF57-6330A9DDE12A}']
    function Connect(pwszURL: PWideChar; pwszTemplateURL: PWideChar; fAutoDestroy: BOOL): HResult; stdcall;
    function Disconnect: HResult; stdcall;
    function EndSession: HResult; stdcall;
  end;

  IWMStatusCallback = interface(IUnknown)
  ['{6d7cdc70-9888-11d3-8edc-00c04f6109cf}']
    function OnStatus(Status: TWMTStatus; hr: HRESULT; dwType: TWMTAttrDataType; pValue: PBYTE; pvContext: Pointer): HRESULT; stdcall;
  end;

  IWMRegisterCallback = interface(IUnknown)
    ['{CF4B1F99-4DE2-4E49-A363-252740D99BC1}']
    function Advise(pCallback: IWMStatusCallback; pvContext: Pointer): HResult; stdcall;
    function Unadvise(pCallback: IWMStatusCallback; pvContext: Pointer): HResult; stdcall;
  end;

const
   WMENC_FILTER_NONE	= 0;
   WMENC_FILTER_A	= $1;
   WMENC_FILTER_AV	= $11;
   WMENC_FILTER_AS	= $101;
   WMENC_FILTER_AVS	= $111;

type
   IWMEncProfileManager = interface(IDispatch)
    ['{731B9D9E-6CF4-4C37-A6A9-A89B880D36EC}']
    function WMEncProfileList(WMencMedia_Filter: LongInt; lcid: LongInt): HResult; stdcall;
    function WMEncProfileListEx(WMencMedia_Filter: LongInt; WMencMedia_newDefault: LongInt; lcid: LongInt): HResult; stdcall;
    function WMEncProfileEdit(bstrProfileName: pBSTR; WMencMedia_Filter: LongInt; lcid: LongInt): HResult; stdcall;
    function GetDetailsString(bstrProfileName: LongInt; lcid: LongInt; out bstrDetailsString: pBSTR): HResult; stdcall;
    function get_ProfileDirectory(out bstrDirectory: pBSTR): HResult; stdcall;
    function put_ProfileDirectory(bstrDirectory: pBSTR): HResult; stdcall;
    function get_LastCreatedProfile(out bstrProfile: pBSTR): HResult; stdcall;
    function get_LastEditedProfile(out bstrProfile: pBSTR): HResult; stdcall;
   end;

  IWMCodecInfo = interface(IUnknown)
    ['{A970F41E-34DE-4A98-B3BA-E4B3CA7528F0}']
    function GetCodecInfoCount(const guidType: TGUID; out pcCodecs: LongWord): HResult; stdcall;
    function GetCodecFormatCount(const guidType: TGUID; dwCodecIndex: LongWord; out pcFormat: LongWord): HResult; stdcall;
    function GetCodecFormat(const guidType: TGUID; dwCodecIndex: LongWord; dwFormatIndex: LongWord; out ppIStreamConfig: IWMStreamConfig): HResult; stdcall;
  end;

  IWMCodecInfo2 = interface(IWMCodecInfo)
    ['{AA65E273-B686-4056-91EC-DD768D4DF710}']
    function GetCodecName(const guidType: TGUID; dwCodecIndex: LongWord; wszName: PWideChar; var pcchName: LongWord): HResult; stdcall;
    function GetCodecFormatDesc(const guidType: TGUID; dwCodecIndex: LongWord; dwFormatIndex: LongWord; out ppIStreamConfig: IWMStreamConfig; wszDesc: PWideChar; var pcchDesc: LongWord): HResult; stdcall;
  end;

  IWMCodecInfo3 = interface(IWMCodecInfo2)
    ['{7E51F487-4D93-4F98-8AB4-27D0565ADC51}']
    function GetCodecFormatProp(const guidType: TGUID; dwCodecIndex: LongWord; dwFormatIndex: LongWord; pszName: PWideChar; out pType: TWMTAttrDataType; pValue: PByte; var pdwSize: LongWord): HResult; stdcall;
    function GetCodecProp(const guidType: TGUID; dwCodecIndex: LongWord; pszName: PWideChar; out pType: TWMTAttrDataType; pValue: PByte; var pdwSize: LongWord): HResult; stdcall;
    function SetCodecEnumerationSetting(const guidType: TGUID; dwCodecIndex: LongWord; pszName: PWideChar; Type_: TWMTAttrDataType; pValue: PByte; dwSize: LongWord): HResult; stdcall;
    function GetCodecEnumerationSetting(const guidType: TGUID; dwCodecIndex: LongWord; pszName: PWideChar; out pType: TWMTAttrDataType; pValue: PByte; var pdwSize: LongWord): HResult; stdcall;
  end;
(*
const
    WMENC_AUDIO	          = $01;
    WMENC_VIDEO	          = $02;
    WMENC_SCRIPT	  = $04;
    WMENC_FILETRANSFER	  = $08;

type
  IWMEncProfile = interface(IDispatch)
    ['{632B6078-BBC6-11D2-A329-006097C4E476}']
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_MediaCount(enumType: LongInt): Smallint; safecall;
    function Get_MultipleBitrate: WordBool; safecall;
    function Get_AudienceCollection: IDispatch; safecall;
    function Get_MaxPacketSize: Integer; safecall;
    procedure Set_MaxPacketSize(Value: Integer); safecall;
  end;

  IWMEncProfile2 = interface(IWMEncProfile)
    ['{C70E1CAC-32D2-4E22-A0FF-3A32E315D095}']
    function Get_ContentType: Integer; safecall;
    function Set_ContentType(Value: Integer): HResult; safecall;
    function LoadFromIWMProfile(pUnkProfile: IUnknown): HResult; safecall;
    function LoadFromFile(const bstrFileName: WideString): HResult; safecall;
    function LoadFromMemory(const bstrData: WideString): HResult; safecall;
    function SaveToIWMProfile: IUnknown; safecall;
    function SaveToFile(const bstrFileName: WideString): HResult; safecall;
    function SaveToMemory: WideString; safecall;
    function Clone(const pObj: IWMEncProfile2): HResult; safecall;
    function Clear: HResult; safecall;
    function Get_AudienceCount: Integer; safecall;
    //function Get_Audience(lIndex: Integer): IWMEncAudienceObj; safecall;
    function Get_Audience(lIndex: Integer): pointer; safecall;
    //function AddAudience(lBitrate: Integer): IWMEncAudienceObj; safecall;
    function AddAudience(lBitrate: Integer): pointer; safecall;
    function DeleteAudience(lIndex: Integer): HResult; safecall;
    function Get_ValidateMode: WordBool; safecall;
    function Set_ValidateMode(Value: WordBool): HResult; safecall;
    function Validate: HResult; safecall;
    function Get_CompatibilityMode: LongInt; safecall;
    function Set_CompatibilityMode(Value: LongInt): HResult; safecall;
    function Get_VBRMode(enumSrcType: LongInt; iRenderSite: Smallint): LongInt; safecall;
    function Set_VBRMode(enumSrcType: LongInt; iRenderSite: Smallint; Value: LongInt): HResult; safecall;
    function Set_ProfileName(const Value: WideString): HResult; safecall;
    function Get_ProfileName: WideString; safecall;
    function Set_ProfileDescription(const Value: WideString): HResult; safecall;
    function Get_ProfileDescription: WideString; safecall;
    function Get_AudioCodecCount: Integer; safecall;
    function EnumAudioCodec(lCodecIndex: Integer; out pvarName: OleVariant): Integer; safecall;
    function Get_AudioFormatCount(lCodecIndex: Integer): Integer; safecall;
    function EnumAudioFormat(lCodecIndex, lFormatIndex: Integer; out pvarName, pvarSamplingRate, pvarNChannels, pvarBitsPerSample: OleVariant): Integer; safecall;
    function Get_VideoCodecCount: Integer; safecall;
    function EnumVideoCodec(lCodecIndex: Integer; out pvarName: OleVariant): Integer; safecall;
    function GetCodecIndexFromFourCC(enumSrcType: LongInt; lFourCC: Integer): Integer; safecall;
    function GetCodecFourCCFromIndex(enumSrcType: LongInt; lIndex: Integer): Integer; safecall;
    function Get_LanguageCount(enumSrcType: LongInt; iRenderSite: Smallint): Integer; safecall;
    function Get_Language(enumSrcType: LongInt; iRenderSite: Smallint; lLanguageIndex: Integer): Integer; safecall;
    function AddLanguage(enumSrcType: LongInt; iRenderSite: Smallint; lcidLanguage: Integer): HResult; safecall;
    function RemoveLanguage(enumSrcType: LongInt; iRenderSite: Smallint; lcidLanguage: Integer): HResult; safecall;
    function Get_BroadcastMode: LongInt; safecall;
    function Set_BroadcastMode(Value: LongInt): HResult; safecall;
    function DetectCompatibility: LongInt; safecall;
    function Merge(const pWMEncProfileObj: IWMEncProfile2): HResult; safecall;
    function Compare(const pCompObj: IWMEncProfile2; out pfSubset: WordBool): WordBool; safecall;
    function Get_InterlaceMode(iRenderSiteIndex: Smallint): WordBool; safecall;
    function Set_InterlaceMode(iRenderSiteIndex: Smallint; Value: WordBool): HResult; safecall;
    function Get_NonSquarePixelMode(iRenderSiteIndex: Smallint): WordBool; safecall;
    function Set_NonSquarePixelMode(iRenderSiteIndex: Smallint; Value: WordBool): HResult; safecall;
    function Get_EnableTimecode(iRenderSiteIndex: Smallint): WordBool; safecall;
    function Set_EnableTimecode(iRenderSiteIndex: Smallint; Value: WordBool): HResult; safecall;
    function Get_MinPacketSize: Integer; safecall;
    function Set_MinPacketSize(Value: Integer): HResult; safecall;
  end;

  IWMEncProfileCollection = interface(IDispatch)
    ['{632B6077-BBC6-11D2-A329-006097C4E476}']
    function Get_length: Integer; safecall;
    function Get_Count: Integer; safecall;
    function Get_ProfileDirectory: WideString; safecall;
    function Set_ProfileDirectory(const Value: WideString): HResult; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Item(Index: Integer): IWMEncProfile; safecall;
    function Refresh: HResult; safecall;
  end;

  IWMEncoder = interface(IDispatch)
    ['{632B607F-BBC6-11D2-A329-006097C4E476}']
    function SetLocaleID(lLocaleID: Integer): HResult; safecall;
    function Load(const bstrFileName: WideString): HResult; safecall;
    function Save(const bstrFileName: WideString): HResult; safecall;
    function Start: HResult; safecall;
    function Stop: HResult; safecall;
    function Get_AutoStop: WordBool; safecall;
    function Set_AutoStop(Value: WordBool): HResult; safecall;
    function Pause: HResult; safecall;
    function PrepareToEncode(bPrepare: WordBool): HResult; safecall;
    function Reset: HResult; safecall;
    function Archive(enumArchiveType: LongInt; enumArchiveOp: LongInt): HResult; safecall;
    function SendScript(iIndex: Smallint; const bstrType, bstrData: WideString): HResult; safecall;
    function Get_EnableAutoArchive: WordBool; safecall;
    function Set_EnableAutoArchive(Value: WordBool): HResult; safecall;
    function Get_RecordingLevel(iIndex: Smallint): Integer; safecall;
    function Set_RecordingLevel(iIndex: Smallint; Value: Integer): HResult; safecall;
    function Get_RecordingMute(iIndex: Smallint): WordBool; safecall;
    function Set_RecordingMute(iIndex: Smallint; Value: WordBool): HResult; safecall;
    function GetAudioLevel(AudioLevelUnits: LongInt; iIndex: Smallint; var plDuration: Integer; out pdLMin, pdLAvg, pdLMax, pdRMin, pdRAvg, pdRMax: Double): HResult; safecall;
    //function Get_SourceGroupCollection: IWMEncSourceGroupCollection; safecall;
    //function Get_SourcePluginInfoManager: IWMEncSourcePluginInfoManager; safecall;
    //function Get_TransformPluginInfoManager: IWMEncTransformPluginInfoManager; safecall;
    function Get_SourceGroupCollection: pointer; safecall;
    function Get_SourcePluginInfoManager: pointer; safecall;
    function Get_TransformPluginInfoManager: pointer; safecall;
    //function Get_DisplayInfo: IWMEncDisplayInfo; safecall;
    function Get_DisplayInfo: pointer; safecall;
    //function Get_Attributes: IWMEncAttributes; safecall;
    function Get_Attributes: pointer; safecall;
    //function Get_Broadcast: IWMEncBroadcast; safecall;
    function Get_Broadcast: pointer; safecall;
    //function Get_File_: IWMEncFile; safecall;
    function Get_File_: pointer; safecall;
    //function Get_Statistics: IWMEncStatistics; safecall;
    function Get_Statistics: pointer; safecall;
    function Get_ProfileCollection: IWMEncProfileCollection; safecall;
    function Get_RunState: LongInt; safecall;
    function Get_ErrorState: Integer; safecall;
    function Get_ArchiveState(enumType: LongInt): LongInt; safecall;
    function Get_Name: WideString; safecall;
    function Set_Name(const Value: WideString): HResult; safecall;
    function Get_RemoteAdmin: WordBool; safecall;
    function Set_RemoteAdmin(Value: WordBool): HResult; safecall;
    function GenerateBroadcastInfo(const bstrFileName: WideString): HResult; safecall;
    function Get_IndexerState: LongInt; safecall;
    function Get_AutoIndex: WordBool; safecall;
    function Set_AutoIndex(Value: WordBool): HResult; safecall;
    function Indexer(enumIndexerOp: LongInt): HResult; safecall;
  end;

const
  WMMEDIATYPE_Video               : TGUID = '{73646976-0000-0010-8000-00AA00389B71}';
*)

type
   pWmtStreamSelection = ^TWmtStreamSelection;
   TWmtStreamSelection  = (WMT_OFF, WMT_CLEANPOINT_ONLY, WMT_ON);

  IWMReaderCallback = interface(IWMStatusCallback)
  ['{96406BD8-2B2B-11d3-B36B-00C04F6108FF}']
    function OnSample(dwOutputNum: LongWord; cnsSampleTime, cnsSampleDuration: Int64; dwFlags: LongWord; pSample: INSSBuffer; pvContext: Pointer): HRESULT; stdcall;
  end;

  IWMOutputMediaProps = interface(IWMMediaProps)
  ['{96406BD7-2B2B-11d3-B36B-00C04F6108FF}']
    function GetStreamGroupName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function GetConnectionName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
  end;

  IWMReader = interface(IUnknown)
  ['{96406BD6-2B2B-11d3-B36B-00C04F6108FF}']
    function Open(pwszURL: PWideChar; pCallback: IWMReaderCallback; pvContext: Pointer): HRESULT; stdcall;
    function Close: HRESULT; stdcall;
    function GetOutputCount(out pcOutputs: LongWord): HRESULT; stdcall;
    function GetOutputProps(dwOutputNum: LongWord; out ppOutput: IWMOutputMediaProps): HRESULT; stdcall;
    function SetOutputProps(dwOutputNum: LongWord; pOutput: IWMOutputMediaProps): HRESULT; stdcall;
    function GetOutputFormatCount(dwOutputNumber: LongWord; out pcFormats: LongWord): HRESULT; stdcall;
    function GetOutputFormat(dwOutputNumber, dwFormatNumber: LongWord; out ppProps: IWMOutputMediaProps): HRESULT; stdcall;
    function Start(cnsStart, cnsDuration: Int64; fRate: Single; pvContext: Pointer): HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
  end;

  IWMSyncReader = interface(IUnknown)
  ['{9397F121-7705-4dc9-B049-98B698188414}']
    function Open(pwszFilename: PWideChar): HRESULT; stdcall;
    function Close: HRESULT; stdcall;
    function SetRange(cnsStartTime, cnsDuration: Int64): HRESULT; stdcall;
    function SetRangeByFrame(wStreamNum: Word; qwFrameNumber, cFramesToRead: Int64 ): HRESULT; stdcall;
    function GetNextSample(wStreamNum: Word; out ppSample: INSSBuffer;
      out pcnsSampleTime: Int64; out pcnsDuration: Int64;
      out pdwFlags: LongWord; out pdwOutputNum: LongWord;
      out pwStreamNum: Word): HRESULT; stdcall;
    function SetStreamsSelected(cStreamCount: Word; pwStreamNumbers: PWORD; pSelections: PWMTStreamSelection): HRESULT; stdcall;
    function GetStreamSelected(wStreamNum: Word; out pSelection: TWMTStreamSelection): HRESULT; stdcall;
    function SetReadStreamSamples(wStreamNum: Word; fCompressed: BOOL): HRESULT; stdcall;
    function GetReadStreamSamples(wStreamNum: Word; out pfCompressed: BOOL): HRESULT; stdcall;
    function GetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar; out pType: TWMTAttrDataType; {out} pValue: PBYTE; var pcbLength: Word): HRESULT; stdcall;
    function SetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar; Type_: TWMTAttrDataType; {in} pValue: PBYTE; cbLength: Word): HRESULT; stdcall;
    function GetOutputCount(out pcOutputs: LongWord): HRESULT; stdcall;
    function GetOutputProps(dwOutputNum: LongWord; out ppOutput: pointer): HRESULT; stdcall;
    function SetOutputProps(dwOutputNum: LongWord; pOutput: pointer): HRESULT; stdcall;
    function GetOutputFormatCount(dwOutputNum: LongWord; out pcFormats: LongWord): HRESULT; stdcall;
    function GetOutputFormat(dwOutputNum, dwFormatNum: LongWord; out ppProps: pointer): HRESULT; stdcall;
    function GetOutputNumberForStream(wStreamNum: Word; out pdwOutputNum: LongWord): HRESULT; stdcall;
    function GetStreamNumberForOutput(dwOutputNum: LongWord; out pwStreamNum: Word): HRESULT; stdcall;
    function GetMaxOutputSampleSize(dwOutput: LongWord; out pcbMax: LongWord): HRESULT; stdcall;
    function GetMaxStreamSampleSize(wStream: Word; out pcbMax: LongWord): HRESULT; stdcall;
    function OpenStream(pStream: IStream): HRESULT; stdcall;
  end;
  ///////////////////////// ASF



const
   CompressionCaps_CanQuality    = $1;
   CompressionCaps_CanCrunch     = $2;
   CompressionCaps_CanKeyFrame   = $4;
   CompressionCaps_CanBFrame     = $8;
   CompressionCaps_CanWindow     = $10;

   AMTUNER_SUBCHAN_NO_TUNE	= -2;
   AMTUNER_SUBCHAN_DEFAULT	= -1;

   AMTUNER_HASNOSIGNALSTRENGTH	= -1;
   AMTUNER_NOSIGNAL	        = 0;
   AMTUNER_SIGNALPRESENT       = 1;

type

  IAMTunerNotification = interface;

  IAMTuner = interface(IUnknown)
    ['{211A8761-03AC-11d1-8D13-00AA00BD8339}']
    function put_Channel(lChannel, lVideoSubChannel, lAudioSubChannel: LongInt): HRESULT; stdcall;
    function get_Channel(out lChannel, lVideoSubChannel, lAudioSubChannel: LongInt): HRESULT; stdcall;
    function ChannelMinMax(out lChannelMin, lChannelMax: LongInt): HRESULT; stdcall;
    function put_CountryCode(lCountryCode: LongInt): HRESULT; stdcall;
    function get_CountryCode(out lCountryCode: LongInt): HRESULT; stdcall;
    function put_TuningSpace(lTuningSpace: LongInt): HRESULT; stdcall;
    function get_TuningSpace(out lTuningSpace: LongInt): HRESULT; stdcall;
    function Logon(hCurrentUser: THandle): HRESULT; stdcall;
    function Logout: HRESULT; stdcall;
    function SignalPresent(out plSignalStrength: LongInt): HRESULT; stdcall;
    function put_Mode(lMode: dword): HRESULT; stdcall;
    function get_Mode(out plMode: dword): HRESULT; stdcall;
    function GetAvailableModes(out plModes: LongInt): HRESULT; stdcall;
    function RegisterNotificationCallBack(pNotify: IAMTunerNotification;
        lEvents: LongInt): HRESULT; stdcall;
    function UnRegisterNotificationCallBack(pNotify: IAMTunerNotification): HRESULT; stdcall;
  end;

  IAMTunerNotification = interface(IUnknown)
    ['{211A8760-03AC-11d1-8D13-00AA00BD8339}']
    function OnEvent(Event: LongInt): HRESULT; stdcall;
  end;

  IAMTVTuner = interface(IAMTuner)
    ['{211A8766-03AC-11d1-8D13-00AA00BD8339}']
    function get_AvailableTVFormats(out lAnalogVideoStandard: LongInt): HRESULT; stdcall;
    function get_TVFormat(out plAnalogVideoStandard: LongInt): HRESULT; stdcall;
    function AutoTune(lChannel: LongInt; out plFoundSignal: LongInt): HRESULT; stdcall;
    function StoreAutoTune: HRESULT; stdcall;
    function get_NumInputConnections(out plNumInputConnections: LongInt): HRESULT; stdcall;
    function put_InputType(lIndex: LongInt; InputType: LongInt): HRESULT; stdcall;
    function get_InputType(lIndex: LongInt; out InputType: LongInt): HRESULT; stdcall;
    function put_ConnectInput(lIndex: LongInt): HRESULT; stdcall;
    function get_ConnectInput(out plIndex: LongInt): HRESULT; stdcall;
    function get_VideoFrequency(out lFreq: LongInt): HRESULT; stdcall;
    function get_AudioFrequency(out lFreq: LongInt): HRESULT; stdcall;
  end;

type
  IAMCrossbar = interface(IUnknown)
    ['{C6E13380-30AC-11d0-A18C-00A0C9118956}']
    function get_PinCounts(out OutputPinCount, InputPinCount: LongInt): HRESULT; stdcall;
    function CanRoute(OutputPinIndex, InputPinIndex: LongInt): HRESULT; stdcall;
    function Route(OutputPinIndex, InputPinIndex: LongInt): HRESULT; stdcall;
    function get_IsRoutedTo(OutputPinIndex: LongInt;
      out InputPinIndex: LongInt): HRESULT; stdcall;
    function get_CrossbarPinInfo(IsInputPin: BOOL; PinIndex: LongInt;
      out PinIndexRelated : LongInt; out PhysicalType: LongInt): HRESULT; stdcall;
  end;

const
    VideoControlFlag_FlipHorizontal        = $0001;
    VideoControlFlag_FlipVertical          = $0002;
    VideoControlFlag_ExternalTriggerEnable = $0004;
    VideoControlFlag_Trigger               = $0008;

type
  IAMVideoControl = interface(IUnknown)
    ['{6a2e0670-28e4-11d0-a18c-00a0c9118956}']
    function GetCaps(pPin: IPin; out pCapsFlags: Longint): HRESULT; stdcall;
    function SetMode(pPin: IPin; Mode: Longint): HRESULT; stdcall;
    function GetMode(pPin: IPin; out Mode: Longint): HRESULT; stdcall;
    function GetCurrentActualFrameRate(pPin: IPin; out ActualFrameRate: Int64): HRESULT; stdcall;
    function GetMaxAvailableFrameRate(pPin: IPin; iIndex: Longint; Dimensions: TSize; out MaxAvailableFrameRate: Int64): HRESULT; stdcall;
    function GetFrameRateList(pPin: IPin; iIndex: Longint; Dimensions: TSize; out ListSize: Longint; out FrameRates: pointer): HRESULT; stdcall;
  end;

  TAMTVAudioEventType = (
    AMTVAUDIO_EVENT_CHANGED
  );

  IAMTVAudio = interface(IUnknown)
    ['{83EC1C30-23D1-11d1-99E6-00A0C9560266}']
    function GetHardwareSupportedTVAudioModes(out plModes: LongInt): HRESULT; stdcall;
    function GetAvailableTVAudioModes(out plModes: LongInt): HRESULT; stdcall;
    function get_TVAudioMode(out plMode: LongInt): HRESULT; stdcall;
    function put_TVAudioMode(lMode: LongInt): HRESULT; stdcall;
    function RegisterNotificationCallBack(pNotify: IAMTunerNotification;
        lEvents: LongInt): HRESULT; stdcall;
    function UnRegisterNotificationCallBack(pNotify: IAMTunerNotification): HRESULT; stdcall;
  end;

  IAMTVAudioNotification = interface(IUnknown)
    ['{83EC1C33-23D1-11D1-99E6-00A0C9560266}']
    function OnEvent(Event: TAMTVAudioEventType): HRESULT; stdcall;
  end;


  IAMVideoCompression = interface(IUnknown)
    ['{C6E13343-30AC-11d0-A18C-00A0C9118956}']
    function put_KeyFrameRate(KeyFrameRate: LongInt): HRESULT; stdcall;
    function get_KeyFrameRate(out pKeyFrameRate: LongInt): HRESULT; stdcall;
    function put_PFramesPerKeyFrame(PFramesPerKeyFrame: LongInt): HRESULT; stdcall;
    function get_PFramesPerKeyFrame(out pPFramesPerKeyFrame: LongInt): HRESULT; stdcall;
    function put_Quality(Quality: double): HRESULT; stdcall;
    function get_Quality(out pQuality: double): HRESULT; stdcall;
    function put_WindowSize(WindowSize: int64): HRESULT; stdcall;
    function get_WindowSize(out pWindowSize: int64): HRESULT; stdcall;
    function GetInfo(pszVersion: PWideChar; var pcbVersion: LongInt;
        pszDescription: PWideChar; var pcbDescription: LongInt;
        out pDefaultKeyFrameRate, pDefaultPFramesPerKey: LongInt;
        out pDefaultQuality: double; out pCapabilities: LongInt): HRESULT; stdcall;
    function OverrideKeyFrame(FrameNumber: LongInt): HRESULT; stdcall;
    function OverrideFrameSize(FrameNumber, Size: LongInt): HRESULT; stdcall;
  end;

type
  IAMDroppedFrames = interface(IUnknown)
    ['{C6E13344-30AC-11d0-A18C-00A0C9118956}']
    function GetNumDropped(out plDropped: LongInt): HRESULT; stdcall;
    function GetNumNotDropped(out plNotDropped: LongInt): HRESULT; stdcall;
    function GetDroppedInfo(lSize: LongInt; out plArray: LongInt;
        out plNumCopied: LongInt): HRESULT; stdcall;
    function GetAverageFrameSize(out plAverageSize: LongInt): HRESULT; stdcall;
  end;

const
  
	OLDWM_CAP_START                    = WM_USER;

(*	OLDWM_CAP_GET_CAPSTREAMPTR         = (OLDWM_CAP_START+  1);
	OLDWM_CAP_SET_CALLBACK_ERROR       = (OLDWM_CAP_START+  2);
	OLDWM_CAP_SET_CALLBACK_STATUS      = (OLDWM_CAP_START+  3);
	OLDWM_CAP_SET_CALLBACK_YIELD       = (OLDWM_CAP_START+  4);
	OLDWM_CAP_SET_CALLBACK_FRAME       = (OLDWM_CAP_START+  5);
	OLDWM_CAP_SET_CALLBACK_VIDEOSTREAM = (OLDWM_CAP_START+  6);
	OLDWM_CAP_SET_CALLBACK_WAVESTREAM  = (OLDWM_CAP_START+  7);
	OLDWM_CAP_GET_USER_DATA            = (OLDWM_CAP_START+  8);
	OLDWM_CAP_SET_USER_DATA            = (OLDWM_CAP_START+  9);

	OLDWM_CAP_DRIVER_CONNECT           = (OLDWM_CAP_START+  10);
	OLDWM_CAP_DRIVER_DISCONNECT        = (OLDWM_CAP_START+  11);
	OLDWM_CAP_DRIVER_GET_NAME          = (OLDWM_CAP_START+  12);
	OLDWM_CAP_DRIVER_GET_VERSION       = (OLDWM_CAP_START+  13);
	OLDWM_CAP_DRIVER_GET_CAPS          = (OLDWM_CAP_START+  14);

	OLDWM_CAP_FILE_SET_CAPTURE_FILE    = (OLDWM_CAP_START+  20);
	OLDWM_CAP_FILE_GET_CAPTURE_FILE    = (OLDWM_CAP_START+  21);
	OLDWM_CAP_FILE_ALLOCATE            = (OLDWM_CAP_START+  22);
	OLDWM_CAP_FILE_SAVEAS              = (OLDWM_CAP_START+  23);
	OLDWM_CAP_FILE_SET_INFOCHUNK       = (OLDWM_CAP_START+  24);
	OLDWM_CAP_FILE_SAVEDIB             = (OLDWM_CAP_START+  25);

	OLDWM_CAP_EDIT_COPY                = (OLDWM_CAP_START+  30);

	OLDWM_CAP_SET_AUDIOFORMAT          = (OLDWM_CAP_START+  35);
	OLDWM_CAP_GET_AUDIOFORMAT          = (OLDWM_CAP_START+  36);

	OLDWM_CAP_DLG_VIDEOFORMAT          = (OLDWM_CAP_START+  41);
	OLDWM_CAP_DLG_VIDEOSOURCE          = (OLDWM_CAP_START+  42);
	OLDWM_CAP_DLG_VIDEODISPLAY         = (OLDWM_CAP_START+  43);
	OLDWM_CAP_GET_VIDEOFORMAT          = (OLDWM_CAP_START+  44); *)
	OLDWM_CAP_SET_VIDEOFORMAT          = (OLDWM_CAP_START+  45);
(*	OLDWM_CAP_DLG_VIDEOCOMPRESSION     = (OLDWM_CAP_START+  46);

	OLDWM_CAP_SET_PREVIEW              = (OLDWM_CAP_START+  50);
 	OLDWM_CAP_SET_OVERLAY              = (OLDWM_CAP_START+  51);
	OLDWM_CAP_SET_PREVIEWRATE          = (OLDWM_CAP_START+  52);
	OLDWM_CAP_SET_SCALE                = (OLDWM_CAP_START+  53);
	OLDWM_CAP_GET_STATUS               = (OLDWM_CAP_START+  54);
	OLDWM_CAP_SET_SCROLL               = (OLDWM_CAP_START+  55);

	OLDWM_CAP_GRAB_FRAME               = (OLDWM_CAP_START+  60);
	OLDWM_CAP_GRAB_FRAME_NOSTOP        = (OLDWM_CAP_START+  61);

	OLDWM_CAP_SEQUENCE                 = (OLDWM_CAP_START+  62);
	OLDWM_CAP_SEQUENCE_NOFILE          = (OLDWM_CAP_START+  63);
	OLDWM_CAP_SET_SEQUENCE_SETUP       = (OLDWM_CAP_START+  64);
	OLDWM_CAP_GET_SEQUENCE_SETUP       = (OLDWM_CAP_START+  65);
	OLDWM_CAP_SET_MCI_DEVICE           = (OLDWM_CAP_START+  66);
	OLDWM_CAP_GET_MCI_DEVICE           = (OLDWM_CAP_START+  67);
	OLDWM_CAP_STOP                     = (OLDWM_CAP_START+  68);
	OLDWM_CAP_ABORT                    = (OLDWM_CAP_START+  69);

	OLDWM_CAP_SINGLE_FRAME_OPEN        = (OLDWM_CAP_START+  70);
	OLDWM_CAP_SINGLE_FRAME_CLOSE       = (OLDWM_CAP_START+  71);
	OLDWM_CAP_SINGLE_FRAME             = (OLDWM_CAP_START+  72);

	OLDWM_CAP_PAL_OPEN                 = (OLDWM_CAP_START+  80);
	OLDWM_CAP_PAL_SAVE                 = (OLDWM_CAP_START+  81);
	OLDWM_CAP_PAL_PASTE                = (OLDWM_CAP_START+  82);
	OLDWM_CAP_PAL_AUTOCREATE           = (OLDWM_CAP_START+  83);
	OLDWM_CAP_PAL_MANUALCREATE         = (OLDWM_CAP_START+  84);

		// Following added post VFW 1.1
	OLDWM_CAP_SET_CALLBACK_CAPCONTROL  = (OLDWM_CAP_START+  85);

	// Defines end of the message range
	OLDWM_CAP_END                      = OLDWM_CAP_SET_CALLBACK_CAPCONTROL; *)

(* VFW__E_INVALIDMEDIATYPE                   = $80040200;
   VFW__E_INVALIDSUBTYPE                     = $80040201;
   VFW__E_NEED_OWNER                         = $80040202;
   VFW__E_ENUM_OUT_OF_SYNC                   = $80040203;
   VFW__E_ALREADY_CONNECTED                  = $80040204;
   VFW__E_FILTER_ACTIVE                      = $80040205;
   VFW__E_NO_TYPES                           = $80040206;
   VFW__E_NO_ACCEPTABLE_TYPES                = $80040207;
   VFW__E_INVALID_DIRECTION                  = $80040208;
   VFW__E_NOT_CONNECTED                      = $80040209;
   VFW__E_NO_ALLOCATOR                       = $8004020A;
   VFW__E_RUNTIME_ERROR                      = $8004020B;
   VFW__E_BUFFER_NOTSET                      = $8004020C;
   VFW__E_BUFFER_OVERFLOW                    = $8004020D;
   VFW__E_BADALIGN                           = $8004020E;
   VFW__E_ALREADY_COMMITTED                  = $8004020F;
   VFW__E_BUFFERS_OUTSTANDING                = $80040210;
   VFW__E_NOT_COMMITTED                      = $80040211;
   VFW__E_SIZENOTSET                         = $80040212;
   VFW__E_NO_CLOCK                           = $80040213;
   VFW__E_NO_SINK                            = $80040214;
   VFW__E_NO_INTERFACE                       = $80040215;
   VFW__E_NOT_FOUND                          = $80040216;
   VFW__E_CANNOT_CONNECT                     = $80040217;
   VFW__E_CANNOT_RENDER                      = $80040218;
   VFW__E_CHANGING_FORMAT                    = $80040219;
   VFW__E_NO_COLOR_KEY_SET                   = $8004021A;
   VFW__E_NOT_OVERLAY_CONNECTION             = $8004021B;
   VFW__E_NOT_SAMPLE_CONNECTION              = $8004021C;
   VFW__E_PALETTE_SET                        = $8004021D;
   VFW__E_COLOR_KEY_SET                      = $8004021E;
   VFW__E_NO_COLOR_KEY_FOUND                 = $8004021F;
   VFW__E_NO_PALETTE_AVAILABLE               = $80040220;
   VFW__E_NO_DISPLAY_PALETTE                 = $80040221;
   VFW__E_TOO_MANY_COLORS                    = $80040222;
   VFW__E_STATE_CHANGED                      = $80040223;
   VFW__E_NOT_STOPPED                        = $80040224;
   VFW__E_NOT_PAUSED                         = $80040225;
   VFW__E_NOT_RUNNING                        = $80040226;
   VFW__E_WRONG_STATE                        = $80040227;
   VFW__E_START_TIME_AFTER_END               = $80040228;
   VFW__E_INVALID_RECT                       = $80040229;
   VFW__E_TYPE_NOT_ACCEPTED                  = $8004022A;
   VFW__E_SAMPLE_REJECTED                    = $8004022B;
   VFW__E_SAMPLE_REJECTED_EOS                = $8004022C;
   VFW__E_DUPLICATE_NAME                     = $8004022D;
   VFW__S_DUPLICATE_NAME                     = $0004022D;
   VFW__E_TIMEOUT                            = $8004022E;
   VFW__E_INVALID_FILE_FORMAT                = $8004022F;
   VFW__E_ENUM_OUT_OF_RANGE                  = $80040230;
   VFW__E_CIRCULAR_GRAPH                     = $80040231;
   VFW__E_NOT_ALLOWED_TO_SAVE                = $80040232;
   VFW__E_TIME_ALREADY_PASSED                = $80040233;
   VFW__E_ALREADY_CANCELLED                  = $80040234;
   VFW__E_CORRUPT_GRAPH_FILE                 = $80040235;
   VFW__E_ADVISE_ALREADY_SET                 = $80040236;
   VFW__S_STATE_INTERMEDIATE                 = $00040237;
   VFW__E_NO_MODEX_AVAILABLE                 = $80040238;
   VFW__E_NO_ADVISE_SET                      = $80040239;
   VFW__E_NO_FULLSCREEN                      = $8004023A;
   VFW__E_IN_FULLSCREEN_MODE                 = $8004023B;
   VFW__E_UNKNOWN_FILE_TYPE                  = $80040240;
   VFW__E_CANNOT_LOAD_SOURCE_FILTER          = $80040241; *)
   VFW__S_PARTIAL_RENDER                     = $00040242;
(* VFW__E_FILE_TOO_SHORT                     = $80040243;
   VFW__E_INVALID_FILE_VERSION               = $80040244;
   VFW__S_SOME_DATA_IGNORED                  = $00040245;
   VFW__S_CONNECTIONS_DEFERRED               = $00040246;
   VFW__E_INVALID_CLSID                      = $80040247;
   VFW__E_INVALID_MEDIA_TYPE                 = $80040248;
   VFW__E_BAD_KEY                            = $800403F2;
   VFW__S_NO_MORE_ITEMS                      = $00040103;
   VFW__E_SAMPLE_TIME_NOT_SET                = $80040249;
   VFW__S_RESOURCE_NOT_NEEDED                = $00040250;
   VFW__E_MEDIA_TIME_NOT_SET                 = $80040251;
   VFW__E_NO_TIME_FORMAT_SET                 = $80040252;
   VFW__E_MONO_AUDIO_HW                      = $80040253;
   VFW__S_MEDIA_TYPE_IGNORED                 = $00040254;
   VFW__E_NO_AUDIO_HARDWARE                  = $80040256;
   VFW__S_VIDEO_NOT_RENDERED                 = $00040257;
   VFW__S_AUDIO_NOT_RENDERED                 = $00040258;
   VFW__E_RPZA                               = $80040259;
   VFW__S_RPZA                               = $0004025A;
   VFW__E_PROCESSOR_NOT_SUITABLE             = $8004025B;
   VFW__E_UNSUPPORTED_AUDIO                  = $8004025C;
   VFW__E_UNSUPPORTED_VIDEO                  = $8004025D;
   VFW__E_MPEG_NOT_CONSTRAINED               = $8004025E;
   VFW__E_NOT_IN_GRAPH                       = $8004025F;
   VFW__S_ESTIMATED                          = $00040260;
   VFW__E_NO_TIME_FORMAT                     = $80040261;
   VFW__E_READ_ONLY                          = $80040262;
   VFW__S_RESERVED                           = $00040263;
   VFW__E_BUFFER_UNDERFLOW                   = $80040264;
   VFW__E_UNSUPPORTED_STREAM                 = $80040265;
   VFW__E_NO_TRANSPORT                       = $80040266;
   VFW__S_STREAM_OFF                         = $00040267;
   VFW__S_CANT_CUE                           = $00040268;
   VFW__E_BAD_VIDEOCD                        = $80040269;
   VFW__S_NO_STOP_TIME                       = $00040270;
   VFW__E_OUT_OF_VIDEO_MEMORY                = $80040271;
   VFW__E_VP_NEGOTIATION_FAILED              = $80040272; *)
   VFW__E_DDRAW_CAPS_NOT_SUITABLE            = $80040273;
(* VFW__E_NO_VP_HARDWARE                     = $80040274;
   VFW__E_NO_CAPTURE_HARDWARE                = $80040275;
   VFW__E_DVD_OPERATION_INHIBITED            = $80040276;
   VFW__E_DVD_INVALIDDOMAIN                  = $80040277;
   VFW__E_DVD_GRAPHNOTREADY                  = $80040279;
   VFW__E_DVD_RENDERFAIL                     = $8004027A;
   VFW__E_DVD_DECNOTENOUGH                   = $8004027B;
   VFW__E_DDRAW_VERSION_NOT_SUITABLE         = $8004027C;
   VFW__E_COPYPROT_FAILED                    = $8004027D;
   VFW__S_NOPREVIEWPIN                       = $0004027E;
   VFW__E_TIME_EXPIRED                       = $8004027F;
   VFW__S_DVD_NON_ONE_SEQUENTIAL             = $00040280;
   VFW__E_DVD_WRONG_SPEED                    = $80040281;
   VFW__E_DVD_MENU_DOES_NOT_EXIST            = $80040282;
   VFW__E_DVD_CMD_CANCELLED                  = $80040283;
   VFW__E_DVD_STATE_WRONG_VERSION            = $80040284;
   VFW__E_DVD_STATE_CORRUPT                  = $80040285;
   VFW__E_DVD_STATE_WRONG_DISC               = $80040286;
   VFW__E_DVD_INCOMPATIBLE_REGION            = $80040287;
   VFW__E_DVD_NO_ATTRIBUTES                  = $80040288;
   VFW__E_DVD_NO_GOUP_PGC                    = $80040289;
   VFW__E_DVD_LOW_PARENTAL_LEVEL             = $8004028A;
   VFW__E_DVD_NOT_IN_KARAOKE_MODE            = $8004028B;
   VFW__S_DVD_CHANNEL_CONTENTS_NOT_AVAILABLE = $0004028C;
   VFW__S_DVD_NOT_ACCURATE                   = $0004028D;
   VFW__E_FRAME_STEP_UNSUPPORTED             = $8004028E;
   VFW__E_DVD_STREAM_DISABLED                = $8004028F;
   VFW__E_DVD_TITLE_UNKNOWN                  = $80040290;
   VFW__E_DVD_INVALID_DISC                   = $80040291;
   VFW__E_DVD_NO_RESUME_INFORMATION          = $80040292;
   VFW__E_PIN_ALREADY_BLOCKED_ON_THIS_THREAD = $80040293;
   VFW__E_PIN_ALREADY_BLOCKED                = $80040294;
   VFW__E_CERTIFICATION_FAILURE              = $80040295; *)
   VFW__E_VMR_NOT_IN_MIXER_MODE              = $80040296;
   VFW__E_VMR_NO_AP_SUPPLIED                 = $80040297;
   VFW__E_VMR_NO_DEINTERLACE_HW              = $80040298;
   VFW__E_VMR_NO_PROCAMP_HW                  = $80040299;

   ICM__USER                    = (DRV_USER+$0000) ;

   ICM__RESERVED                = (DRV_USER+$1000) ;

   ICM__GETSTATE                = (ICM__RESERVED+0) ;
   ICM__SETSTATE                = (ICM__RESERVED+1) ;
   ICM__GETINFO                 = (ICM__RESERVED+2) ;

   ICM__COMPRESS_FRAMES_INFO    = (ICM__USER+70) ;

(*   ICM__CONFIGURE               = (ICM__RESERVED+10);
   ICM__ABOUT                   = (ICM__RESERVED+11);

   ICM__GETDEFAULTQUALITY       = (ICM__RESERVED+30);
   ICM__GETQUALITY              = (ICM__RESERVED+31);
   ICM__SETQUALITY              = (ICM__RESERVED+32);

   ICM__SET                     = (ICM__RESERVED+40);
   ICM__GET                     = (ICM__RESERVED+41);

   ICM__FRAMERATE               = $526D7246; // FOURCC('F','r','m','R')
   ICM__KEYFRAMERATE            = $5279654B; // FOURCC('K','e','y','R')

   ICM__COMPRESS_GET_FORMAT     = (ICM__USER+4)  ;
   ICM__COMPRESS_GET_SIZE       = (ICM__USER+5)  ;
   ICM__COMPRESS_QUERY          = (ICM__USER+6)  ;
   ICM__COMPRESS_BEGIN          = (ICM__USER+7)  ;
   ICM__COMPRESS                = (ICM__USER+8)  ;
   ICM__COMPRESS_END            = (ICM__USER+9)  ;

   ICM__DECOMPRESS_GET_FORMAT   = (ICM__USER+10) ;
   ICM__DECOMPRESS_QUERY        = (ICM__USER+11) ;
   ICM__DECOMPRESS_BEGIN        = (ICM__USER+12) ;
   ICM__DECOMPRESS              = (ICM__USER+13) ;
   ICM__DECOMPRESS_END          = (ICM__USER+14) ;
   ICM__DECOMPRESS_SET_PALETTE  = (ICM__USER+29) ;
   ICM__DECOMPRESS_GET_PALETTE  = (ICM__USER+30) ;

   ICM__DRAW_QUERY              = (ICM__USER+31) ;
   ICM__DRAW_BEGIN              = (ICM__USER+15) ;
   ICM__DRAW_GET_PALETTE        = (ICM__USER+16) ;
   ICM__DRAW_START              = (ICM__USER+18) ;
   ICM__DRAW_STOP               = (ICM__USER+19) ;
   ICM__DRAW_END                = (ICM__USER+21) ;
   ICM__DRAW_GETTIME            = (ICM__USER+32) ;
   ICM__DRAW                    = (ICM__USER+33) ;
   ICM__DRAW_WINDOW             = (ICM__USER+34) ;
   ICM__DRAW_SETTIME            = (ICM__USER+35) ;
   ICM__DRAW_REALIZE            = (ICM__USER+36) ;
   ICM__DRAW_FLUSH              = (ICM__USER+37) ;
   ICM__DRAW_RENDERBUFFER       = (ICM__USER+38) ;

   ICM__DRAW_START_PLAY         = (ICM__USER+39) ;
   ICM__DRAW_STOP_PLAY          = (ICM__USER+40) ;

   ICM__DRAW_SUGGESTFORMAT      = (ICM__USER+50) ;
   ICM__DRAW_CHANGEPALETTE      = (ICM__USER+51) ;

   ICM__GETBUFFERSWANTED        = (ICM__USER+41) ;

   ICM__GETDEFAULTKEYFRAMERATE  = (ICM__USER+42) ;

   ICM__DECOMPRESSEX_BEGIN      = (ICM__USER+60) ;
   ICM__DECOMPRESSEX_QUERY      = (ICM__USER+61) ;
   ICM__DECOMPRESSEX            = (ICM__USER+62) ;
   ICM__DECOMPRESSEX_END        = (ICM__USER+63) ;

   ICM__COMPRESS_FRAMES_INFO    = (ICM__USER+70) ;
   ICM__SET_STATUS_PROC         = (ICM__USER+72) ;
*)

   VfwCompressDialog_Config      = 1;
   VfwCompressDialog_About       = 2;
   VfwCompressDialog_QueryConfig = 4;
   VfwCompressDialog_QueryAbout  = 8;

   VfwCaptureDialog_Source       = 1;
   VfwCaptureDialog_Format       = 2;
   VfwCaptureDialog_Display      = 4;

type
  IAMVfwCaptureDialogs = interface(IUnknown)
    ['{D8D715A0-6E5E-11D0-B3F0-00AA003761C5}']
    function HasDialog(iDialog: LongInt): HRESULT; stdcall;
    function ShowDialog(iDialog: LongInt; hwnd: HWND): HRESULT; stdcall;
    function SendDriverMessage(iDialog: dword; uMsg: dword; dw1, dw2: dword): HRESULT; stdcall;
  end;

  IAMVfwCompressDialogs = interface(IUnknown)
    ['{D8D715A3-6E5E-11D0-B3F0-00AA003761C5}']
    function ShowDialog(iDialog: LongInt; hwnd: HWND): HRESULT; stdcall;
    function GetState(pState: pointer; var pcbState: LongInt): HRESULT; stdcall;
    function SetState(pState: pointer; cbState: LongInt): HRESULT; stdcall;
    function SendDriverMessage(uMsg: LongInt; dw1, dw2: LongInt): HRESULT; stdcall;
  end;

type

  IAMAnalogVideoDecoder = interface(IUnknown)
    ['{C6E13350-30AC-11d0-A18C-00A0C9118956}']
    function get_AvailableTVFormats(out lAnalogVideoStandard: LongInt): HRESULT; stdcall;
    function put_TVFormat(lAnalogVideoStandard: LongInt): HRESULT; stdcall;
    function get_TVFormat(out plAnalogVideoStandard: LongInt): HRESULT; stdcall;
    function get_HorizontalLocked(out plLocked: LongInt): HRESULT; stdcall;
    function put_VCRHorizontalLocking(lVCRHorizontalLocking: LongInt): HRESULT; stdcall;
    function get_VCRHorizontalLocking(out plVCRHorizontalLocking: LongInt): HRESULT; stdcall;
    function get_NumberOfLines(out plNumberOfLines: LongInt): HRESULT; stdcall;
    function put_OutputEnable(lOutputEnable: LongInt): HRESULT; stdcall;
    function get_OutputEnable(out plOutputEnable: LongInt): HRESULT; stdcall;
  end;

  TTimeCode = record
    wFrameRate: Word;
    wFrameFract: Word;
    dwFrames: dword;
  end;

  TTimeCode_Sample = record
    qwTick: Int64;
    timecode: TTimeCode;
    dwUser: dword;
    dwFlags: dword;
  end;

  IAMTimecodeDisplay = interface(IUnknown)
    ['{9B496CE2-811B-11CF-8C77-00AA006B6814}']
    function GetTCDisplayEnable(out pState: LongInt): HRESULT; stdcall;
    function SetTCDisplayEnable(State: LongInt): HRESULT; stdcall;
    function GetTCDisplay(Param: LongInt; out pValue: LongInt): HRESULT; stdcall;
    function SetTCDisplay(Param, Value: LongInt): HRESULT; stdcall;
  end;

  IAMTimecodeReader = interface(IUnknown)
    ['{9B496CE1-811B-11CF-8C77-00AA006B6814}']
    function GetTCRMode(Param: LongInt; out pValue: LongInt): HRESULT; stdcall;
    function SetTCRMode(Param: LongInt; Value: LongInt): HRESULT; stdcall;
    function put_VITCLine(Line: LongInt): HRESULT; stdcall;
    function get_VITCLine(out pLine: LongInt): HRESULT; stdcall;
    function GetTimecode(out pTimecodeSample: TTimeCode_Sample): HRESULT; stdcall;
  end;

  IAMExtDevice = interface(IUnknown)
    ['{B5730A90-1A2C-11CF-8C23-00AA006B6814}']
    function GetCapability(Capability: LongInt; out pValue: LongInt; out pdblValue: double): HRESULT; stdcall;
    function get_ExternalDeviceID(out ppszData: pWideChar): HRESULT; stdcall;
    function get_ExternalDeviceVersion(out ppszData: pWideChar): HRESULT; stdcall;
    function put_DevicePower(PowerMode: LongInt): HRESULT; stdcall;
    function get_DevicePower(out pPowerMode: LongInt): HRESULT; stdcall;
    function Calibrate(hEvent: THandle; Mode: LongInt; out pStatus: LongInt): HRESULT; stdcall;
    function put_DevicePort(DevicePort: LongInt): HRESULT; stdcall;
    function get_DevicePort(out pDevicePort: LongInt): HRESULT; stdcall;
  end;

  IAMExtTransport = interface(IUnknown)
    ['{A03CD5F0-3045-11CF-8C44-00AA006B6814}']
    function GetCapability(Capability: LongInt; out pValue: LongInt; out pdblValue: double): HRESULT; stdcall;
    function put_MediaState(State: LongInt): HRESULT; stdcall;
    function get_MediaState(out pState: LongInt): HRESULT; stdcall;
    function put_LocalControl(State: LongInt): HRESULT; stdcall;
    function get_LocalControl(out pState: LongInt): HRESULT; stdcall;
    function GetStatus(StatusItem: LongInt; out pValue: LongInt): HRESULT; stdcall;
    function GetTransportBasicParameters(Param: LongInt; var pValue: LongInt; ppszData: pointer): HRESULT; stdcall;
    function SetTransportBasicParameters(Param: LongInt; Value: LongInt; pszData: PWideChar): HRESULT; stdcall;
    function GetTransportVideoParameters(Param: LongInt; out pValue: LongInt): HRESULT; stdcall;
    function SetTransportVideoParameters(Param: LongInt; Value: LongInt): HRESULT; stdcall;
    function GetTransportAudioParameters(Param: LongInt; out pValue: LongInt): HRESULT; stdcall;
    function SetTransportAudioParameters(Param: LongInt; Value: LongInt): HRESULT; stdcall;
    function put_Mode(Mode: LongInt): HRESULT; stdcall;
    function get_Mode(out pMode: LongInt): HRESULT; stdcall;
    function put_Rate(dblRate: double): HRESULT; stdcall;
    function get_Rate(out pdblRate: double): HRESULT; stdcall;
    function GetChase(out pEnabled, pOffset: LongInt; var phEvent: THandle): HRESULT; stdcall;
    function SetChase(Enable, Offset: LongInt; hEvent: THandle): HRESULT; stdcall;
    function GetBump(out pSpeed, pDuration: LongInt): HRESULT; stdcall;
    function SetBump(Speed, Duration: LongInt): HRESULT; stdcall;
    function get_AntiClogControl(out pEnabled: LongInt): HRESULT; stdcall;
    function put_AntiClogControl(Enable: LongInt): HRESULT; stdcall;
    function GetEditPropertySet(EditID: LongInt; out pState: LongInt): HRESULT; stdcall;
    function SetEditPropertySet(var pEditID: LongInt; State: LongInt): HRESULT; stdcall;
    function GetEditProperty(EditID, Param: LongInt; out pValue: LongInt): HRESULT; stdcall;
    function SetEditProperty(EditID, Param, Value: LongInt): HRESULT; stdcall;
    function get_EditStart(out pValue: LongInt): HRESULT; stdcall;
    function put_EditStart(Value: LongInt): HRESULT; stdcall;
  end;

const
  AM_Flags_Auto = 1;
  AM_Flags_Manual = 2;

type

  IAMCameraControl = interface(IUnknown)
    ['{C6E13370-30AC-11d0-A18C-00A0C9118956}']
    //function GetRange(Property_: longint; out pMin, pMax, pSteppingDelta, pDefault, pCapsFlag: longint): HRESULT; stdcall;
    function GetRange(Property_: LongInt; out pMin, pMax, pSteppingDelta, pDefault, pCapsFlags: longint): HRESULT; stdcall;
    function Set_(Property_: LongInt; lValue: LongInt; Flags: LongInt): HRESULT; stdcall;
    function Get(Property_: LongInt; out lValue, Flags: longint): HRESULT; stdcall;
  end;

  IAMVideoProcAmp = interface(IUnknown)
    ['{C6E13360-30AC-11d0-A18C-00A0C9118956}']
    function GetRange(Property_:LongInt; out pMin, pMax, pSteppingDelta, pDefault, pCapsFlags: longint): HRESULT; stdcall;
    function Set_(Property_: LongInt; lValue: LongInt; Flags: LongInt): HRESULT; stdcall;
    function Get(Property_: LongInt; out lValue, Flags: longint): HRESULT; stdcall;
  end;

const
 	DVDECODERRESOLUTION_720x480	= 1000;
	DVDECODERRESOLUTION_360x240	= 1001;
	DVDECODERRESOLUTION_180x120	= 1002;
	DVDECODERRESOLUTION_88x60	   = 1003;

 	DVRESOLUTION_FULL	        = 1000;
	DVRESOLUTION_HALF	        = 1001;
	DVRESOLUTION_QUARTER	     = 1002;
	DVRESOLUTION_DC	        = 1003;

type
  IIPDVDec = interface(IUnknown)
    ['{b8e8bd60-0bfe-11d0-af91-00aa00b67a42}']
    function get_IPDisplay(out displayPix : LongInt): HRESULT; stdcall;
    function put_IPDisplay(displayPix: LongInt): HRESULT; stdcall;
  end;

  IDVRGB219 = interface(IUnknown)
    ['{58473A19-2BC8-4663-8012-25F81BABDDD1}']
    function SetRGB219(bState: BOOL): HRESULT; stdcall;
  end;

  IDVSplitter = interface(IUnknown)
    ['{92a3a302-da7c-4a1f-ba7e-1802bb5d2d02}']
    function DiscardAlternateVideoFrames(nDiscard: LongInt): HRESULT; stdcall;
  end;

  ICreateDevEnum = interface(IUnknown)
    ['{29840822-5B84-11D0-BD3B-00A0C911CE86}']
    function CreateClassEnumerator(const clsidDeviceClass: TGUID; out ppEnumMoniker: IEnumMoniker; dwFlags: dword): HRESULT; stdcall;
  end;

type
  OAEVENT = LongInt;
  OAHWND = LongInt;

  IMediaEvent = interface(IDispatch)
    ['{56A868B6-0AD4-11CE-B03A-0020AF0BA770}']
    function GetEventHandle(out hEvent: OAEVENT): HRESULT; stdcall;
    function GetEvent(out lEventCode: LongInt; out lParam1, lParam2: LongInt;
        msTimeout: dword): HRESULT; stdcall;
    function WaitForCompletion(msTimeout: dword; out pEvCode: LongInt):
        HRESULT; stdcall;
    function CancelDefaultHandling(lEvCode: LongInt): HRESULT; stdcall;
    function RestoreDefaultHandling(lEvCode: LongInt): HRESULT; stdcall;
    function FreeEventParams(lEvCode: LongInt; lParam1, lParam2: LongInt):
        HRESULT; stdcall;
  end;

  IMediaEventEx = interface(IMediaEvent)
    ['{56A868C0-0AD4-11CE-B03A-0020AF0BA770}']
    function SetNotifyWindow(hwnd: OAHWND; lMsg: LongInt;
        lInstanceData: LongInt): HRESULT; stdcall;
    function SetNotifyFlags(lNoNotifyFlags: LongInt): HRESULT; stdcall;
    function GetNotifyFlags(out lplNoNotifyFlags): HRESULT; stdcall; //LongInt
  end;

(*  TAMVPMode = (
    AMVP_MODE_WEAVE,
    AMVP_MODE_BOBINTERLEAVED,
    AMVP_MODE_BOBNONINTERLEAVED,
    AMVP_MODE_SKIPEVEN,
    AMVP_MODE_SKIPODD
  );

  IVPBaseNotify = interface(IUnknown)
    function RenegotiateVPParameters: HResult; stdcall;
  end;

  IVPNotify = interface(IVPBaseNotify)
    ['{C76794A1-D6C5-11D0-9E69-00C04FD7C15B}']
    function SetDeinterlaceMode(mode: TAMVPMode): HResult; stdcall;
    function GetDeinterlaceMode(out pMode: TAMVPMode): HResult; stdcall;
  end;

  IVPNotify2 = interface(IVPNotify)
    ['{EBF47183-8764-11d1-9E69-00C04FD7C15B}']
    function SetVPSyncMaster(bVPSyncMaster: BOOL): HResult; stdcall;
    function GetVPSyncMaster(OUT pbVPSyncMaster: BOOL): HResult; stdcall;
  end;*)

const
  AMDDS_NONE    = $00;        // No use for DCI/DirectDraw
  AMDDS_DCIPS   = $01;        // Use DCI primary surface
  AMDDS_PS      = $02;        // Use DirectDraw primary
  AMDDS_RGBOVR  = $04;        // RGB overlay surfaces
  AMDDS_YUVOVR  = $08;        // YUV overlay surfaces
  AMDDS_RGBOFF  = $10;        // RGB offscreen surfaces
  AMDDS_YUVOFF  = $20;        // YUV offscreen surfaces
  AMDDS_RGBFLP  = $40;        // RGB flipping surfaces
  AMDDS_YUVFLP  = $80;        // YUV flipping surfaces
  AMDDS_ALL     = $FF;        // ALL the previous flags
  AMDDS_DEFAULT = AMDDS_ALL;   // Use all available surfaces

  AMDDS_YUV = AMDDS_YUVOFF or AMDDS_YUVOVR or AMDDS_YUVFLP;
  AMDDS_RGB = AMDDS_RGBOFF or AMDDS_RGBOVR or AMDDS_RGBFLP;
  AMDDS_PRIMARY = AMDDS_DCIPS or AMDDS_PS;

type
   (*
  IDirectDrawVideo = interface(IUnknown)
    ['{36D39EB0-DD75-11CE-BF0E-00AA0055595A}']
    // IDirectDrawVideo methods
    function GetSwitches(out pSwitches: DWORD): HRESULT; stdcall;
    function SetSwitches(pSwitches: DWORD): HRESULT; stdcall;
    function GetCaps(out pCaps: TDDCaps): HRESULT; stdcall;
    function GetEmulatedCaps(out pCaps: TDDCaps): HRESULT; stdcall;
    function GetSurfaceDesc(out pSurfaceDesc: TDDSurfaceDesc): HRESULT; stdcall;
    function GetFourCCCodes(out pCount, pCodes: DWORD): HRESULT; stdcall;
    function SetDirectDraw(pDirectDraw: IDirectDraw): HRESULT; stdcall;
    function GetDirectDraw(out ppDirectDraw: IDirectDraw): HRESULT; stdcall;
    function GetSurfaceType(out pSurfaceType: DWORD): HRESULT; stdcall;
    function SetDefault: HRESULT; stdcall;
    function UseScanLine(UseScanLine: LongBool): HRESULT; stdcall;
    function CanUseScanLine(var UseScanLine: LongBool): HRESULT; stdcall;
    function UseOverlayStretch(UseOverlayStretch: LongBool): HRESULT; stdcall;
    function CanUseOverlayStretch(var UseOverlayStretch: LongBool): HRESULT;
        stdcall;
    function UseWhenFullScreen(UseWhenFullScreen: LongBool): HRESULT; stdcall;
    function WillUseFullScreen(var UseWhenFullScreen: LongBool): HRESULT;
        stdcall;
  end;
  *)

  IDirectDrawVideo = interface(IUnknown)
    ['{36D39EB0-DD75-11CE-BF0E-00AA0055595A}']
    // IDirectDrawVideo methods
    function GetSwitches(out pSwitches: DWORD): HRESULT; stdcall;
    function SetSwitches(pSwitches: DWORD): HRESULT; stdcall;
    function GetCaps(out pCaps: pointer): HRESULT; stdcall;
    function GetEmulatedCaps(out pCaps: pointer): HRESULT; stdcall;
    function GetSurfaceDesc(out pSurfaceDesc: pointer): HRESULT; stdcall;
    function GetFourCCCodes(out pCount, pCodes: DWORD): HRESULT; stdcall;
    function SetDirectDraw(pDirectDraw: pointer): HRESULT; stdcall;
    function GetDirectDraw(out ppDirectDraw: pointer): HRESULT; stdcall;
    function GetSurfaceType(out pSurfaceType: DWORD): HRESULT; stdcall;
    function SetDefault: HRESULT; stdcall;
    function UseScanLine(UseScanLine: LongBool): HRESULT; stdcall;
    function CanUseScanLine(var UseScanLine: LongBool): HRESULT; stdcall;
    function UseOverlayStretch(UseOverlayStretch: LongBool): HRESULT; stdcall;
    function CanUseOverlayStretch(var UseOverlayStretch: LongBool): HRESULT;
        stdcall;
    function UseWhenFullScreen(UseWhenFullScreen: LongBool): HRESULT; stdcall;
    function WillUseFullScreen(var UseWhenFullScreen: LongBool): HRESULT;
        stdcall;
  end;

  IVideoWindow = interface(IDispatch)
    ['{56A868B4-0AD4-11CE-B03A-0020AF0BA770}']
    function put_Caption(strCaption: WideString): HResult; stdcall;
    function get_Caption(out strCaption: WideString): HResult; stdcall;
    function put_WindowStyle(WindowStyle: LongInt): HResult; stdcall;
    function get_WindowStyle(out WindowStyle: LongInt): HResult; stdcall;
    function put_WindowStyleEx(WindowStyleEx: LongInt): HResult; stdcall;
    function get_WindowStyleEx(out WindowStyleEx: LongInt): HResult; stdcall;
    function put_AutoShow(AutoShow: LongBool): HResult; stdcall;
    function get_AutoShow(out AutoShow: LongBool): HResult; stdcall;
    function put_WindowState(WindowState: LongInt): HResult; stdcall;
    function get_WindowState(out WindowState: LongInt): HResult; stdcall;
    function put_BackgroundPalette(BackgroundPalette: LongInt): HResult; stdcall;
    function get_BackgroundPalette(out pBackgroundPalette: LongInt): HResult; stdcall;
    function put_Visible(Visible: LongBool): HResult; stdcall;
    function get_Visible(out pVisible: LongBool): HResult; stdcall;
    function put_Left(Left: LongInt): HResult; stdcall;
    function get_Left(out pLeft: LongInt): HResult; stdcall;
    function put_Width(Width: LongInt): HResult; stdcall;
    function get_Width(out pWidth: LongInt): HResult; stdcall;
    function put_Top(Top: LongInt): HResult; stdcall;
    function get_Top(out pTop: LongInt): HResult; stdcall;
    function put_Height(Height: LongInt): HResult; stdcall;
    function get_Height(out pHeight: LongInt): HResult; stdcall;
    function put_Owner(Owner: OAHWND): HResult; stdcall;
    function get_Owner(out Owner: OAHWND): HResult; stdcall;
    function put_MessageDrain(Drain: OAHWND): HResult; stdcall;
    function get_MessageDrain(out Drain: OAHWND): HResult; stdcall;
    function get_BorderColor(out Color: LongInt): HResult; stdcall;
    function put_BorderColor(Color: LongInt): HResult; stdcall;
    function get_FullScreenMode(out FullScreenMode: LongBool): HResult; stdcall;
    function put_FullScreenMode(FullScreenMode: LongBool): HResult; stdcall;
    function SetWindowForeground(Focus: LongInt): HResult; stdcall;
    function NotifyOwnerMessage(hwnd: LongInt; uMsg, wParam, lParam: LongInt): HResult; stdcall;
    function SetWindowPosition(Left, Top, Width, Height: LongInt): HResult; stdcall;
    function GetWindowPosition(out pLeft, pTop, pWidth, pHeight: LongInt): HResult; stdcall;
    function GetMinIdealImageSize(out pWidth, pHeight: LongInt): HResult; stdcall;
    function GetMaxIdealImageSize(out pWidth, pHeight: LongInt): HResult; stdcall;
    function GetRestorePosition(out pLeft, pTop, pWidth, pHeight: LongInt): HResult; stdcall;
    function HideCursor(HideCursor: LongBool): HResult; stdcall;
    function IsCursorHidden(out CursorHidden: LongBool): HResult; stdcall;
  end;

  IVideoFrameStep = interface(IUnknown)
    ['{e46a9787-2b71-444d-a4b5-1fab7b708d6a}']
    function Step(dwFrames: dword; pStepObject: IUnKnown): HResult; stdcall;
    function CanStep(bMultiple: LongInt; pStepObject: IUnknown): HResult; stdcall;
    function CancelStep: HResult; stdcall;
  end;

  IBasicAudio = interface(IDispatch)
    ['{56A868B3-0AD4-11CE-B03A-0020AF0BA770}']
    function put_Volume(lVolume: Longint): HResult; stdcall;
    function get_Volume(out plVolume: Longint): HResult; stdcall;
    function put_Balance(lBalance: Longint): HResult; stdcall;
    function get_Balance(out plBalance: Longint): HResult; stdcall;
  end;

  IBasicVideo = interface(IDispatch)
    ['{56A868B5-0AD4-11CE-B03A-0020AF0BA770}']
    function get_AvgTimePerFrame(out pAvgTimePerFrame: TRefTime): HResult; stdcall;
    function get_BitRate(out pBitRate: LongInt): HResult; stdcall;
    function get_BitErrorRate(out pBitErrorRate: LongInt): HResult; stdcall;
    function get_VideoWidth(out pVideoWidth: LongInt): HResult; stdcall;
    function get_VideoHeight(out pVideoHeight: LongInt): HResult; stdcall;
    function put_SourceLeft(SourceLeft: LongInt): HResult; stdcall;
    function get_SourceLeft(out pSourceLeft: LongInt): HResult; stdcall;
    function put_SourceWidth(SourceWidth: LongInt): HResult; stdcall;
    function get_SourceWidth(out pSourceWidth: LongInt): HResult; stdcall;
    function put_SourceTop(SourceTop: LongInt): HResult; stdcall;
    function get_SourceTop(out pSourceTop: LongInt): HResult; stdcall;
    function put_SourceHeight(SourceHeight: LongInt): HResult; stdcall;
    function get_SourceHeight(out pSourceHeight: LongInt): HResult; stdcall;
    function put_DestinationLeft(DestinationLeft: LongInt): HResult; stdcall;
    function get_DestinationLeft(out pDestinationLeft: LongInt): HResult; stdcall;
    function put_DestinationWidth(DestinationWidth: LongInt): HResult; stdcall;
    function get_DestinationWidth(out pDestinationWidth: LongInt): HResult; stdcall;
    function put_DestinationTop(DestinationTop: LongInt): HResult; stdcall;
    function get_DestinationTop(out pDestinationTop: LongInt): HResult; stdcall;
    function put_DestinationHeight(DestinationHeight: LongInt): HResult; stdcall;
    function get_DestinationHeight(out pDestinationHeight: LongInt): HResult; stdcall;
    function SetSourcePosition(Left, Top, Width, Height: LongInt): HResult; stdcall;
    function GetSourcePosition(out pLeft, pTop, pWidth, pHeight: LongInt): HResult; stdcall;
    function SetDefaultSourcePosition: HResult; stdcall;
    function SetDestinationPosition(Left, Top, Width, Height: LongInt): HResult; stdcall;
    function GetDestinationPosition(out pLeft, pTop, pWidth, pHeight: LongInt): HResult; stdcall;
    function SetDefaultDestinationPosition: HResult; stdcall;
    function GetVideoSize(out pWidth, Height: LongInt): HResult; stdcall;
    function GetVideoPaletteEntries(StartIndex, Entries: LongInt;
        out pRetrieved: LongInt; out pPalette): HResult; stdcall;
    function GetCurrentImage(var BufferSize: LongInt; var pDIBImage): HResult; stdcall;
    function IsUsingDefaultSource: HResult; stdcall;
    function IsUsingDefaultDestination: HResult; stdcall;
  end;

  IBasicVideo2 = interface(IBasicVideo)
    ['{329bb360-f6ea-11d1-9038-00a0c9697298}']
    function GetPreferredAspectRatio(out plAspectX, plAspectY: LongInt): HRESULT; stdcall;
  end;

type
  PDDSCaps = ^TDDSCaps;
  TDDSCaps = packed record
    dwCaps: dword;
  end;

  PDDColorKey = ^TDDColorKey;
  TDDColorKey = packed record
    dwColorSpaceLowValue: dword;
    dwColorSpaceHighValue: dword;
  end;

  PDDPixelFormat_DX6 = ^TDDPixelFormat_DX6;
  TDDPixelFormat_DX6 = packed record
    dwSize: dword;
    dwFlags: dword;
    dwFourCC: dword;
    case LongInt of
      1: (
          dwRGBBitCount : dword;
          dwRBitMask : dword;
          dwGBitMask : dword;
          dwBBitMask : dword;
          dwRGBAlphaBitMask : dword;
          );
      2: (
          dwYUVBitCount : dword;
          dwYBitMask : dword;  
          dwUBitMask : dword;
          dwVBitMask : dword;
          dwYUVAlphaBitMask : dword;
          );
      3: (
          dwZBufferBitDepth : dword;
          dwStencilBitDepth : dword;
          dwZBitMask : dword;
          dwStencilBitMask : dword;
          dwLuminanceAlphaBitMask : dword;
          );
      4: (
          dwAlphaBitDepth : dword;
          dwLuminanceBitMask : dword;
          dwBumpDvBitMask : dword;
          dwBumpLuminanceBitMask : dword;
          dwRGBZBitMask : dword;
          );
      5: (
           dwLuminanceBitCount : dword;
           dwBumpDuBitMask : dword;
           Fill1, Fill2    : dword;
           dwYUVZBitMask   : dword;
         );
      6: ( dwBumpBitCount  : dword;
         );
  end;

  PDDPixelFormat = PDDPixelFormat_DX6;

  TDDSurfaceDesc = packed record
    dwSize: dword;
    dwFlags: dword;
    dwHeight: dword;
    dwWidth: dword;
    case LongInt of
    0: (
      dwLinearSize : dword;
     );
    1: (
      lPitch: LongInt;
      dwBackBufferCount: dword;
      case LongInt of
      0: (
        dwMipMapCount: dword;
        dwAlphaBitDepth: dword;
        dwReserved: dword;
        lpSurface: Pointer;
        ddckCKDestOverlay: TDDColorKey;
        ddckCKDestBlt: TDDColorKey;
        ddckCKSrcOverlay: TDDColorKey;
        ddckCKSrcBlt: TDDColorKey;
        ddpfPixelFormat: TDDPixelFormat_DX6;
        ddsCaps: TDDSCaps;
       );
      1: (
        dwZBufferBitDepth: dword;
       );
      2: (
        dwRefreshRate: dword;
       );
     );
  end;

  PDDSurfaceDesc = ^TDDSurfaceDesc;

  IQualProp = interface(IUnknown)
    ['{1BD0ECB0-F8E2-11CE-AAC6-0020AF0B99A3}']
    function get_FramesDroppedInRenderer(var pcFrames: LongInt): HRESULT;
        stdcall;
    function get_FramesDrawn(out pcFrames: LongInt): HRESULT; stdcall;
    function get_AvgFrameRate(out piAvgFrameRate: LongInt): HRESULT; stdcall;
    function get_Jitter(out iJitter: LongInt): HRESULT; stdcall;
    function get_AvgSyncOffset(out piAvg: LongInt): HRESULT; stdcall;
    function get_DevSyncOffset(out piDev: LongInt): HRESULT; stdcall;
  end;

  IFullScreenVideo = interface(IUnknown)
    ['{DD1D7110-7836-11CF-BF47-00AA0055595A}']
    // IFullScreenVideo methods
    function CountModes(out pModes: LongInt): HRESULT; stdcall;
    function GetModeInfo(Mode: LongInt; out pWidth, pHeight, pDepth: LongInt):
        HRESULT; stdcall;
    function GetCurrentMode(out pMode: LongInt): HRESULT; stdcall;
    function IsModeAvailable(Mode: LongInt): HRESULT; stdcall;
    function IsModeEnabled(Mode: LongInt): HRESULT; stdcall;
    function SetEnabled(Mode: LongInt; bEnabled: LongInt): HRESULT; stdcall;
    function GetClipFactor(out pClipFactor: LongInt): HRESULT; stdcall;
    function SetClipFactor(ClipFactor: LongInt): HRESULT; stdcall;
    function SetMessageDrain(hwnd: HWND): HRESULT; stdcall;
    function GetMessageDrain(out hwnd: HWND): HRESULT; stdcall;
    function SetMonitor(Monitor: LongInt): HRESULT; stdcall;
    function GetMonitor(out Monitor: LongInt): HRESULT; stdcall;
    function HideOnDeactivate(Hide: LongBool): HRESULT; stdcall;
    function IsHideOnDeactivate: HRESULT; stdcall;
    function SetCaption(strCaption: TBStr): HRESULT; stdcall;
    function GetCaption(out pstrCaption: TBStr): HRESULT; stdcall;
    function SetDefault: HRESULT; stdcall;
  end;

  IFullScreenVideoEx = interface(IFullScreenVideo)
    ['{53479470-F1DD-11CF-BC42-00AA00AC74F6}']
    function SetAcceleratorTable(hwnd: HWND; hAccel: HACCEL): HRESULT; stdcall;
    function GetAcceleratorTable(var hwnd: HWND; var hAccel: HACCEL): HRESULT;
        stdcall;
    function KeepPixelAspectRatio(KeepAspect: LongBool): HRESULT; stdcall;
    function IsKeepPixelAspectRatio(var pKeepAspect: LongBool): HRESULT; stdcall;
  end;

  IBaseVideoMixer = interface(IUnknown)
    ['{61DED640-E912-11CE-A099-00AA00479A58}']
    function SetLeadPin(iPin: LongInt): HRESULT; stdcall;
    function GetLeadPin(out iPin: LongInt): HRESULT; stdcall;
    function GetInputPinCount(out piPinCount: LongInt): HRESULT; stdcall;
    function IsUsingClock(out pbValue: LongInt): HRESULT; stdcall;
    function SetUsingClock(bValue: LongInt): HRESULT; stdcall;
    function GetClockPeriod(out pbValue: LongInt): HRESULT; stdcall;
    function SetClockPeriod(bValue: LongInt): HRESULT; stdcall;
  end;

  IMpeg2Demultiplexer = interface(IUnknown)
    ['{436eee9c-264f-4242-90e1-4e330c107512}']
    function CreateOutputPin(pMediaType: PAM_MEDIA_TYPE; pszPinName: PWideChar; out ppIPin: IPin): HResult; stdcall;
    function SetOutputPinMediaType(pszPinName: PWideChar; var pMediaType: TAM_MEDIA_TYPE): HResult; stdcall;
    function DeleteOutputPin(pszPinName: PWideChar): HResult; stdcall;
  end;

const
   MPEG2_PROGRAM_ELEMENTARY_STREAM = 1;

type

  PStreamIDMap = ^TStreamIDMap;
  TStreamIDMap = record
    stream_id             : ULONG;
    dwMediaSampleContent  : DWORD;
    ulSubstreamFilterValue: ULONG;
    iDataOffset           : integer;
  end;

  IEnumStreamIdMap = interface(IUnknown)
    ['{945C1566-6202-46fc-96C7-D87F289C6534}']
    function Next(cRequest: ULONG; StreamIdMap: PStreamIDMap; out pcReceived: ULONG): HResult; stdcall;
    function Skip(cRecords: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppIEnumStreamIdMap: IEnumStreamIdMap): HResult; stdcall;
  end;

  IMPEG2StreamIdMap = interface(IUnknown)
    ['{D0E04C47-25B8-4369-925A-362A01D95444}']
    function MapStreamId(ulStreamId: ULONG; MediaSampleContent: ULONG; ulSubstreamFilterValue: ULONG; iDataOffset: ULONG): HResult; stdcall;
    function UnmapStreamId(culStreamId: ULONG; var pulStreamId: ULONG): HResult; stdcall;
    function EnumStreamIdMap(out ppIEnumStreamIdMap: IEnumStreamIdMap): HResult; stdcall;
  end;

  IMPEG2PIDMap = interface(IUnknown)
    ['{afb6c2a1-2c41-11d3-8a60-0000f81e0e4a}']
    function MapPID(culPID: ULONG; var pulPID: ULONG; MediaSampleContent: ULONG): HResult; stdcall;
    function UnmapPID(culPID: ULONG; var pulPID: ULONG): HResult; stdcall;
    function EnumPIDMap(out pIEnumPIDMap: pointer): HResult; stdcall;
  end;



const
     PROPSETID_HCW_ENCODE_CONFIG_PROPERTIES: TGUID = '{432A0DA4-806A-43a0-B426-4F2A234AA6B8}';

	HCW_ECP_SYSTEM_StreamType	=   0; // Type of Stream

	HCW_ECP_VIDEO_Horizontal_Res= 100; // Horizontal Resolution (Vertical is implied by StreamType and VideoFormat)
	HCW_ECP_VIDEO_GOP_OpenClosed= 101; // Closed or Open GOP
	HCW_ECP_VIDEO_BitRate		= 102; // BitRate and Mode(CBR/VBR)
//	HCW_ECP_VIDEO_GOP_Size		= 103; // GOP Size Details (FUTURE)

	HCW_ECP_AUDIO_BitRate		= 200; // BitRate and Layer
	HCW_ECP_AUDIO_SampleRate	= 201; // SampleRate
	HCW_ECP_AUDIO_Output		= 202; // Output Mode (Mono; Stereo; Joint; Dual)
	HCW_ECP_AUDIO_CRC			= 203; // CRC On/Off

	HCW_ECP_DETECT_Macrovision	= 300; // GET ONLY
									   // Macrovision Detection (Yes; No; Don't Know)
	HCW_ECP_DETECT_MVision_Level    = 301; // GET ONLY
									   // Macrovision Level Detection (ie. Level1, Level2, Level3)
type
    HCW_Sys_StreamType = dword;
const
	HCW_Sys_StreamType_Unknown			=   0; //Unknown (error?) StreamType

	HCW_Sys_StreamType_Program			= 101; //Program Stream (SEE NOTE ABOVE)
	HCW_Sys_StreamType_Program_DVD		= 102; //Program Stream (DVD Compliant)
	HCW_Sys_StreamType_Program_DVD_MC	= 103; //Program Stream (Meets MS MediaCenter Requirements -- Not Strictly DVD-Compliant)
	HCW_Sys_StreamType_Program_SVCD		= 104; //Program Stream (SVCD Compliant)

	HCW_Sys_StreamType_MPEG1			= 201; //MPGE1 Stream
	HCW_Sys_StreamType_MPEG1_VCD		= 202; //MPEG1 Stream (VCD Compliant)

type
   HCW_Vid_HRes = dword;
const
	HCW_Vid_HRes_Unknown			= 0;

	HCW_Vid_HRes_FullD1				= 1;
	HCW_Vid_HRes_720				= 1;

	HCW_Vid_HRes_TwoThirdsD1		= 2;
	HCW_Vid_HRes_480				= 2;

	HCW_Vid_HRes_HalfD1				= 3;
	HCW_Vid_HRes_352				= 3;

	HCW_Vid_HRes_320				= 4;

	HCW_Vid_HRes_640				= 5;

	HCW_Vid_GOP_OC_Open		= 0; //Open GOPS
	HCW_Vid_GOP_OC_Closed	= 1; //Closed GOPS

	HCW_Vid_EncMode_CBR		= 0; //Constant Bit Rate
	HCW_Vid_EncMode_VBR		= 1; //Variable Bit Rate

type
   HCW_Vid_BitRate = record
      dwSize: dword;
      dwReserved: dword;
      Mode: LongInt;
      dwBitRate: dword;
      dwBitRatePeak: dword;
   end;

type
   HCW_Aud_BitRate_Layer2 = dword;
const
	HCW_Aud_BitRate_Layer2_32		=  1;
	HCW_Aud_BitRate_Layer2_48		=  2;
	HCW_Aud_BitRate_Layer2_56		=  3;
	HCW_Aud_BitRate_Layer2_64		=  4;
	HCW_Aud_BitRate_Layer2_80		=  5;
	HCW_Aud_BitRate_Layer2_96		=  6;
	HCW_Aud_BitRate_Layer2_112		=  7;
	HCW_Aud_BitRate_Layer2_128		=  8;
	HCW_Aud_BitRate_Layer2_160		=  9;
	HCW_Aud_BitRate_Layer2_192		=  10;
	HCW_Aud_BitRate_Layer2_224		=  11;
	HCW_Aud_BitRate_Layer2_256		=  12;
	HCW_Aud_BitRate_Layer2_320		=  13;
	HCW_Aud_BitRate_Layer2_384		=  14;

	HCW_Aud_Layer_2		=  2;

type
   HCW_Aud_BitRate = record
      dwSize: dword;
      dwReserved: dword;
      Layer: longint;
      dwBitRate: dword;
   end;

type
   HCW_Aud_SampleRate = dword;
const
	HCW_Aud_SampleRate_32			= 0;
	HCW_Aud_SampleRate_44			= 1; //44.1
	HCW_Aud_SampleRate_48			= 2;

type
   HCW_Aud_Output = dword;
const
	HCW_Aud_Output_Stereo		= 0;
	HCW_Aud_Output_JointStereo	= 1;
	HCW_Aud_Output_DualChannel	= 2;
	HCW_Aud_Output_Mono			= 3;

type
   HCW_Aud_CRC = dword;
const
	HCW_Aud_CRC_Off	= 0;
	HCW_Aud_CRC_On	= 1;

type
   HCW_Detect_Macrovision = dword;
const
	HCW_Detect_Macrovision_No		= 0; //Macrovision Detection - NOT Detected
	HCW_Detect_Macrovision_Yes		= 1; //Macrovision Detection - YES Detected
	HCW_Detect_Macrovision_DontKnow	= 2; //Macrovision Detection - Can't Determine Right Now

type
   HCW_Detect_MVision_Level = dword;
const
   HCW_Detect_MVision_Level_None    = 0;                              //no macrovision detected
   HCW_Detect_MVision_Level_1       = 1;                              //Level1 detected
   HCW_Detect_MVision_Level_2       = 2;                              //Level2 detected
   HCW_Detect_MVision_Level_3       = 3;                              //Level3 detected


const
    	AMOVERFX_NOFX	          = 0;
	AMOVERFX_MIRRORLEFTRIGHT  = $2;
	AMOVERFX_MIRRORUPDOWN	  = $4;
	AMOVERFX_DEINTERLACE	  = $8;
type

  IAMOverlayFX = interface(IUnknown)
    ['{62fae250-7e65-4460-bfc9-6398b322073c}']
    function QueryOverlayFXCaps(out lpdwOverlayFXCaps: dword): HRESULT; stdcall;
    function SetOverlayFX(dwOverlayFX: dword): HRESULT; stdcall;
    function GetOverlayFX(out lpdwOverlayFX: dword): HRESULT; stdcall;
  end;

const
  iMAXBITS        = 8;
  iEGA_COLORS     = 16;
  iMASK_COLORS    = 3;
  iPALETTE        = 8;
  iPALETTE_COLORS = 256;
  iTRUECOLOR      = 16;
  iRED            = 0;
  iGREEN          = 1;
  iBLUE           = 2;

  AMINTERLACE_IsInterlaced            = $00000001;  // if 0, other interlace bits are irrelevent
  AMINTERLACE_1FieldPerSample         = $00000002;  // else 2 fields per media sample
  AMINTERLACE_Field1First             = $00000004;  // else Field 2 is first;  top field in PAL is field 1, top field in NTSC is field 2?
  AMINTERLACE_UNUSED                  = $00000008;  //
  AMINTERLACE_FieldPatternMask        = $00000030;  // use this mask with AMINTERLACE_FieldPat*
  AMINTERLACE_FieldPatField1Only      = $00000000;  // stream never contains a Field2
  AMINTERLACE_FieldPatField2Only      = $00000010;  // stream never contains a Field1
  AMINTERLACE_FieldPatBothRegular     = $00000020;  // There will be a Field2 for every Field1 (required for Weave?)
  AMINTERLACE_FieldPatBothIrregular   = $00000030;  // Random pattern of Field1s and Field2s
  AMINTERLACE_DisplayModeMask         = $000000c0;
  AMINTERLACE_DisplayModeBobOnly      = $00000000;
  AMINTERLACE_DisplayModeWeaveOnly    = $00000040;
  AMINTERLACE_DisplayModeBobOrWeave   = $00000080;

type
  TTrueColorInfo = record
    dwBitMasks: array[0..iMASK_COLORS-1] of dword;
    bmiColors: array[0..iPALETTE_COLORS-1] of TRGBQuad;
  end;

  PVideoInfoHeader2 = ^TVideoInfoHeader2;
  TVideoInfoHeader2 = record // size = 112
    rcSource: TRect;
    rcTarget: TRect;
    dwBitRate: dword;
    dwBitErrorRate: dword;
    AvgTimePerFrame: TReference_Time;
    dwInterlaceFlags: dword;
    dwCopyProtectFlags: dword;
    dwPictAspectRatioX: dword;
    dwPictAspectRatioY: dword;
    dwControlFlags: dword;
    dwReserved2: dword;
    bmiHeader: TBitmapInfoHeader;
   end;

  PVideoInfoHeader = ^TVideoInfoHeader;
  TVideoInfoHeader = record
    rcSource: TRect;                   // The bit we really want to use
    rcTarget: TRect;                   // Where the video should go
    dwBitRate: dword;                  // Approximate bit data rate
    dwBitErrorRate: dword;             // Bit error rate for this stream
    AvgTimePerFrame: TReference_Time;  // Average time per frame (100ns units)

    bmiHeader: TBitmapInfoHeader;
  end;

  PVideoInfo = ^TVideoInfo;
  TVideoInfo = record
    rcSource: TRect;
    rcTarget: TRect;
    dwBitRate: dword;
    dwBitErrorRate: dword;
    AvgTimePerFrame: TReference_Time;

    bmiHeader: TBitmapInfoHeader;

    case LongInt of
    0: (
      bmiColors: array[0..iPALETTE_COLORS-1] of TRGBQuad
      );
    1: (
      dwBitMasks: array[0..iMASK_COLORS-1] of dword
      );
    2: (
      TrueColorInfo: TTrueColorInfo
      );
  end;

  pMpeg1VideoInfo = ^TMpeg1VideoInfo;
  TMpeg1VideoInfo = record
    hdr: TVideoInfoHeader;
    dwStartTimeCode: dword;
    cbSequenceHeader: dword;
    bSequenceHeader: array[0..0] of Byte;
  end;

  pMPEG2VideoInfo = ^TMPEG2VideoInfo;
  TMPEG2VideoInfo = record // size 136
     hdr: TVideoInfoHeader2;
     dwStartTimeCode: dword;
     cbSequenceHeader: dword;
     dwProfile: dword;
     dwLevel: dword;
     dwFlags: dword;
     dwSequenceHeader: dword;
  end;

  pMPEG1WAVEFORMAT = ^TMPEG1WAVEFORMAT;
  TMPEG1WAVEFORMAT = record
    wfx: TWaveFormatEx;
    fwHeadLayer: Word;
    dwHeadBitrate: DWORD;
    fwHeadMode: Word;
    fwHeadModeExt: Word;
    wHeadEmphasis: Word;
    fwHeadFlags: Word;
    dwPTSLow: DWORD;
    dwPTSHigh: DWORD;
  end;

type
   TRendererMode = (rmPreview, rmCapture, rmPlayback);

const
  MERIT_PREFERRED       = $800000;
  MERIT_NORMAL          = $600000;
  MERIT_UNLIKELY        = $400000;
  MERIT_DO_NOT_USE      = $200000;
  MERIT_SW_COMPRESSOR   = $100000;
  MERIT_HW_COMPRESSOR   = $100050;

  EC_SYSTEMBASE                        = $00;
  EC_USER                              = $8000;
  EC_COMPLETE                          = $01;
  EC_USERABORT                         = $02;
  EC_ERRORABORT                        = $03;
  EC_TIME                              = $04;
  EC_REPAINT                           = $05;
  EC_STREAM_ERROR_STOPPED              = $06;
  EC_STREAM_ERROR_STILLPLAYING         = $07;
  EC_ERROR_STILLPLAYING                = $08;
  EC_PALETTE_CHANGED                   = $09;
  EC_VIDEO_SIZE_CHANGED                = $0A;
  EC_QUALITY_CHANGE                    = $0B;
  EC_SHUTTING_DOWN                     = $0C;
  EC_CLOCK_CHANGED                     = $0D;
  EC_PAUSED                            = $0E;
  EC_OPENING_FILE                      = $10;
  EC_BUFFERING_DATA                    = $11;
  EC_FULLSCREEN_LOST                   = $12;
  EC_ACTIVATE                          = $13;
  EC_NEED_RESTART                      = $14;
  EC_WINDOW_DESTROYED                  = $15;
  EC_DISPLAY_CHANGED                   = $16;
  EC_STARVATION                        = $17;
  EC_OLE_EVENT                         = $18;
  EC_NOTIFY_WINDOW                     = $19;
  EC_STREAM_CONTROL_STOPPED            = $1A;
  EC_STREAM_CONTROL_STARTED            = $1B;
  EC_END_OF_SEGMENT                    = $1C;
  EC_SEGMENT_STARTED                   = $1D;
  EC_LENGTH_CHANGED                    = $1E;
  EC_DEVICE_LOST                       = $1f;
  EC_STEP_COMPLETE                     = $24;
  EC_SKIP_FRAMES                       = $25;
  EC_TIMECODE_AVAILABLE		       = $30;
  EC_EXTDEVICE_MODE_CHANGE	       = $31;
  EC_GRAPH_CHANGED                     = $50;
  EC_CLOCK_UNSET                       = $51;
  EC_VMR_RENDERDEVICE_SET              = $53;
  EC_VMR_SURFACE_FLIPPED               = $54;
  EC_VMR_RECONNECTION_FAILED           = $55;
  EC_PREPROCESS_COMPLETE               = $56;
  EC_CODECAPI_EVENT                    = $57;
  EC_WMT_EVENT_BASE                    = $0251;
  EC_WMT_INDEX_EVENT = EC_WMT_EVENT_BASE;

const

//  CLSID_CTVTunerFilter: TGUID = (D1:$266EEE40;D2:$6C63;D3:$11cf;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
    CLSID_ASFWriter: TGUID = (D1:$7c23220e;D2:$55bb;D3:$11d3;D4:($8b,$16,$00,$c0,$4f,$b6,$bd,$3d));
    CLSID_WMEncProfileManager: TGUID = '{A8D3AD02-7508-4004-B2E9-AD33F087F43C}';
    CLSID_Colour: TGUID = (D1:$1643e180; D2:$90f5; D3:$11ce;D4:($97, $d5, $00, $aa, $00, $55, $59, $5a));
//  CLSID_AviSplitter: TGUID = (D1:$1B544C20;D2:$FD0B;D3:$11CE;D4:($8C,$63,$00,$AA,$00,$44,$B5,$1E));
    CLSID_CaptureGraphBuilder2: TGUID = '{BF87B6E1-8C27-11d0-B3F0-00AA003761C5}';
    CLSID_CaptureGraphBuilder: TGUID = '{BF87B6E0-8C27-11D0-B3F0-00AA003761C5}';
//  CLSID_CaptureProperties: TGUID = (D1:$1B544C22;D2:$FD0B;D3:$11CE;D4:($8C,$63,$00,$AA,$00,$44,$B5,$1F)); // INDIALOGS
    CLSID_FilterGraph: TGUID = (D1:$E436EBB3;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
//  CLSID_FilterMapper2: TGUID = (D1:$CDA42200;D2:$BD88;D3:$11D0;D4:($BD,$4E,$00,$A0,$C9,$11,$CE,$86));
    CLSID_Proxy: TGUID = (D1:$17CCA71B;D2:$ECD7;D3:$11D0;D4:($B9,$08,$00,$A0,$C9,$22,$31,$96));
    CLSID_VfwCapture: TGUID = (D1:$1B544C22;D2:$FD0B;D3:$11CE;D4:($8C,$63,$00,$AA,$00,$44,$B5,$1E));
    CLSID_VideoRenderer: TGUID = (D1:$70E102B0;D2:$5556;D3:$11CE;D4:($97,$C0,$00,$AA,$00,$55,$59,$5A));
//  CLSID_VideoPortManager: TGUID = (D1:$6f26a6cd;D2:$967b;D3:$47fd; D4:($87,$4a,$7a,$ed,$2c,$9d,$25,$a2));
//    CLSID_PinnacleVideoRenderer: TGUID = (D1:$D7C14634;D2:$2549;D3:$4F76;D4:($93,$FC,$E5,$B7,$B7,$57,$FE,$48));

//  CLSID_PinnacleMJPEGCompressor: TGUID = '{90CF2AA9-D1B9-DC47-BDE0-E00ACEF9460F}';
//  CLSID_PinnacleMJPEGCompressor: TGUID = '{AD25688D-FE9A-4FF3-884C-FA9591377A5C}';
//  CLSID_PinnacleMJPEGRenderer: TGUID = '{1A1A44C6-16DF-41C1-A479-E3761069EEED}';
//  CLSID_PinnacleVideoRenderer: TGUID = '{11C012C1-5563-11D3-8D66-00AA00A01893}';
//  CLSID_PinnacleMJPEGRenderer: TGUID = '{C6441A1A-DF16-C141-A479-E3761069EEED}';

//  CLSID_PinnacleVideoRendererDlg: TGUID = (D1:$1A1A44C6;D2:$16DF;D3:$41C1;D4:($A4,$79,$E3,$76,$10,$69,$EE,$ED));//(D1:$11C012C1;D2:$5563;D3:$11D3;D4:($8D,$66,$00,$AA,$00,$A0,$18,$93));
//  CLSID_VBISurfaces: TGUID = (D1:$814b9800;D2:$1c88;D3:$11d1;D4:($ba, $d9, $0, $60, $97, $44, $11, $1a));
    CLSID_VMR7: TGUID = (D1:$B87BEB7B; D2:$8D29; D3:$423f; D4:($AE, $4D, $65, $82, $C1, $01, $75, $AC));
    CLSID_VMR9 : TGUID = (D1:$51b4abf3; D2:$748f; D3:$4e3b; D4:($a2, $76,$c8,$28,$33,$0e,$92,$6a));
    CLSID_VideoRendererDefault: TGUID = (D1:$6BC1CFFA; D2:$8FC1; D3:$4261; D4:($AC, $22, $CF, $B4, $CC, $38, $DB, $50));
    CLSID_AudioRender: TGUID = (D1:$E30629D1;D2:$27E5;D3:$11CE;D4:($87,$5D,$00,$60,$8C,$B7,$80,$66));
    CLSID_DSoundRender: TGUID = '{79376820-07D0-11cf-A24D-0020AFD79767}';
    CLSID_AVIDec: TGUID = (D1:$CF49D4E0;D2:$1115;D3:$11CE;D4:($B0,$3A,$00,$20,$AF,$0B,$A7,$70));
    CLSID_WMAsfReader: TGUID = '{187463A0-5BB7-11d3-ACBE-0080C75E246E}';
    CLSID_WindowsMediaSourceFilter: TGUID = '{6B6D0800-9ADA-11D0-A520-00A0D10129C0}';
    CLSID_DVVideoCodec: TGUID = (D1:$B1B77C00;D2:$C3E4;D3:$11CF;D4:($AF,$79,$00,$AA,$00,$B6,$7A,$42));
    CLSID_DVVideoEnc: TGUID = (D1:$13AA3650;D2:$BB6F;D3:$11D0;D4:($AF,$B9,$00,$AA,$00,$B6,$7A,$42));
    CLSID_DVMux: TGUID = '{129D7E40-C10D-11d0-AFB9-00AA00B67A42}';
    CLSID_DVSplitter: TGUID = (D1:$4EB31670;D2:$9FC6;D3:$11CF;D4:($AF,$6E,$00,$AA,$00,$B6,$7A,$42));
    CLSID_OverlayMixer: TGUID = (D1:$CD8743A1;D2:$3736;D3:$11D0;D4:($9E,$69,$00,$C0,$4F,$D7,$C1,$5B));
    CLSID_OverlayMixer2: TGUID = (D1:$A0025E90;D2:$E45B;D3:$11D1;D4:($AB,$E9,$00,$A0,$C9,$05,$F3,$75));
    CLSID_SystemDeviceEnum: TGUID = (D1:$62BE5D10;D2:$60EB;D3:$11D0;D4:($BD,$3B,$00,$A0,$C9,$11,$CE,$86));
    CLSID_WMEncoder: TGUID = '{632B606A-BBC6-11D2-A329-006097C4E476}';
    CLSID_WMEncProfile2: TGUID = '{A5AC04E7-3E13-48CE-A43F-9FBA59DB1544}';
    CLSID_LegacyAmFilterCategory: TGUID = '{083863F1-70DE-11d0-BD40-00A0C911CE86}';
    CLSID_WDMStreamEncoders: TGUID = '{19689BF6-C384-48FD-AD51-90E58C79F70B}';
    CLSID_VideoInputDeviceCategory: TGUID = (D1:$860BB310;D2:$5D01;D3:$11D0;D4:($BD,$3B,$00,$A0,$C9,$11,$CE,$86));
    CLSID_AudioRendererCategory: TGUID = '{E0F158E1-CB04-11d0-BD4E-00A0C911CE86}';
    CLSID_VideoCompressorCategory: TGUID = (D1:$33D9A760;D2:$90C8;D3:$11D0;D4:($BD,$43,$00,$A0,$C9,$11,$CE,$86));
    KSCATEGORY_MULTIPLEXER : TGUID = (D1:$7a5de1d3; D2:$01a1; D3:$452c; D4:($b4,$81,$4f,$a2,$b9,$62,$71,$e8));
    KSCATEGORY_CROSSBAR    : TGUID = (D1:$a799a801; D2:$a46d; D3:$11d0; D4:($a1, $8c, $00, $a0, $24, $01, $dc, $d4));
    KSCATEGORY_TVTUNER     : TGUID = (D1:$a799a800; D2:$a46d; D3:$11d0; D4:($a1, $8c, $00, $a0, $24, $01, $dc, $d4));
    KSCATEGORY_TVAUDIO     : TGUID = (D1:$a799a802; D2:$a46d; D3:$11d0; D4:($a1, $8c, $00, $a0, $24, $01, $dc, $d4));
    CLSID_AudioCompressorCategory: TGUID = (D1:$33D9A761;D2:$90C8;D3:$11D0;D4:($BD,$43,$00,$A0,$C9,$11,$CE,$86));
    CLSID_AudioInputDeviceCategory: TGUID = (D1:$33D9A762;D2:$90C8;D3:$11D0;D4:($BD,$43,$00,$A0,$C9,$11,$CE,$86));
    CLSID_ActiveMovieCategories: TGUID = (D1:$DA4E3DA0;D2:$D07D;D3:$11D0;D4:($BD,$50,$00,$A0,$C9,$11,$CE,$86));
//  CLSID_DvdGraphBuilder: TGUID = (D1:$FCC152B7;D2:$F372;D3:$11D0;D4:($8E,$00,$00,$C0,$4F,$D7,$C0,$8B));
//  CLSID_DVDNavigator: TGUID = (D1:$9B8C4620;D2:$2C1A;D3:$11D0;D4:($84,$93,$00,$A0,$24,$38,$AD,$48));
//  CLSID_DVDState: TGUID = (D1:$f963c5cf;D2:$a659;D3:$4a93;D4:($96,$38,$ca,$f3,$cd,$27,$7d,$13));
    CLSID_SmartTee: TGUID = (D1:$cc58e280;D2:$8aa1;D3:$11d1;D4:($b3,$f1,$00,$aa,$00,$37,$61,$c5));
    CLSID_InfTee: TGUID = (D1:$F8388A40;D2:$D5BB;D3:$11D0;D4:($BE,$5A,$00,$80,$C7,$06,$56,$8E));
    CLSID_Dm: TGUID = (D1:$afb6c280;D2:$2c41;D3:$11d3;D4:($8a,$60,$00,$00,$f8,$1e,$0e,$4a));
    CLSID_SnTS  : TGUID = '{FBE18D61-BF69-11D2-BAAA-00A024D857B2}';
    CLSID_SnVDTN: TGUID = '{81C81A03-D68F-4598-9827-051D9B5CB891}';
    CLSID_SnVD  : TGUID = '{30146000-87BF-11D1-BE74-C94E44925F69}';
    CLSID_SnADTN: TGUID = '{08091097-9634-4D6B-BE98-75FEEE60729E}';
    CLSID_SnAD  : TGUID = '{E1F79440-812F-11D1-BFA2-00A024EC9DA6}';

  (*  CLSID_Hauppauge_WinTV_PVR_PCI_II_Encoder  : TGUID = '{17CCA71B-ECD7-11D0-B908-00A0C9223196}}';
    CLSID_Hauppauge_WinTV_PVR_PCI_II_Capture  : TGUID = '{17CCA71B-ECD7-11D0-B908-00A0C9223196}';
    CLSID_Hauppauge_WinTV_PVR_PCI_II_TV_Tuner : TGUID = '{266EEE40-6C63-11cf-8A03-00AA006ECB65}';
    CLSID_Hauppauge_WinTV_PVR_PCI_II_Crossbar : TGUID = '{71F96460-78F3-11d0-A18C-00A0C9118956}';
    CLSID_Hauppauge_WinTV_PVR_PCI_II_TVAudio  : TGUID = '{71F96462-78F3-11d0-A18C-00A0C9118956}';
    CLSID_Hauppauge_WinTV_PVR_PCI_II_: TGUID = '';
    CLSID_Hauppauge_WinTV_PVR_PCI_II_: TGUID = '';
    CLSID_Hauppauge_WinTV_PVR_PCI_II_: TGUID = '';
    CLSID_Hauppauge_WinTV_PVR_PCI_II_: TGUID = '';*)
//  CLSID_SystemClock: TGUID = (D1:$E436EBB1;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));

    CLSID_MediaDet : TGUID = '{65BD0711-24D2-4FF7-9324-ED2E5D3ABAFA}';
    CLSID_SampleGrabber : TGUID = '{C1F400A0-3F08-11D3-9F0B-006008039E37}';
    CLSID_NullRenderer : TGUID = '{C1F400A4-3F08-11D3-9F0B-006008039E37}';
//    CLSID_AVIDecompressor : TGUID = '{CF49D4E0-1115-11CE-B03A-0020AF0BA770}';

  CLSID_DSCALER_Deinterlacer: TGUID = (D1:$463D645D; D2:$48F7; D3:$11D4; D4:($84,$64,$00,$08,$C7,$82,$A2,$57));
//  CLSID_LAME_Mpeg3Encoder: TGUID = (D1:$B8D27088; D2:$DF5F; D3:$4B7C; D4:($98,$DC,$0E,$91,$A1,$69,$62,$86));

//  GUID_NULL: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));
//  MEDIATYPE_NULL: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));
//  MEDIASUBTYPE_NULL: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));

    MEDIATYPE_Video: TGUID = (D1:$73646976;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIATYPE_Audio: TGUID = (D1:$73647561;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIATYPE_AnalogVideo: TGUID = (D1:$0482DDE1;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIATYPE_Text: TGUID = (D1:$73747874;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIATYPE_Midi: TGUID = (D1:$7364696D;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIATYPE_Stream: TGUID = (D1:$E436EB83;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIATYPE_Interleaved: TGUID = (D1:$73766169;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIATYPE_MPEG1SystemStream: TGUID = (D1:$E436EB82;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIATYPE_AnalogAudio: TGUID = (D1:$0482DEE1;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIATYPE_File: TGUID = (D1:$656C6966;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIATYPE_ScriptCommand: TGUID = (D1:$73636D64;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIATYPE_AUXLine21Data: TGUID = (D1:$670AEA80;D2:$3A82;D3:$11D0;D4:($B7,$9B,$00,$AA,$00,$37,$67,$A7));
//  MEDIATYPE_Timecode: TGUID = (D1:$0482DEE3;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIATYPE_LMRT : TGUID = (D1:$74726c6d;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
//  MEDIATYPE_URL_STREAM: TGUID = (D1:$736c7275;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));

    MEDIASUBTYPE_CLPL: TGUID = (D1:$4C504C43;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
    MEDIASUBTYPE_YUYV: TGUID = (D1:$56595559;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
    MEDIASUBTYPE_IYUV: TGUID = (D1:$56555949;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
    MEDIASUBTYPE_YVU9: TGUID = (D1:$39555659;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_Y411: TGUID = (D1:$31313459;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_Y41P: TGUID = (D1:$50313459;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_YUY2: TGUID = (D1:$32595559;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_Y422: TGUID = '{32323459-0000-0010-8000-00AA00389B71}';
    MEDIASUBTYPE_YVYU: TGUID = (D1:$55595659;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_I420: TGUID = (D1:$30323449;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
    MEDIASUBTYPE_UYVY: TGUID = (D1:$59565955;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_Y211: TGUID = (D1:$31313259;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_YV12: TGUID = (D1:$32315659;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_CLJR: TGUID = (D1:$524A4C43;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_IF09: TGUID = (D1:$39304649;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_CPLA: TGUID = (D1:$414C5043;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_MJPG: TGUID = (D1:$47504A4D;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_TVMJ: TGUID = (D1:$4A4D5654;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_WAKE: TGUID = (D1:$454B4157;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_CFCC: TGUID = (D1:$43434643;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_IJPG: TGUID = (D1:$47504A49;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_Plum: TGUID = (D1:$6D756C50;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));

    MEDIASUBTYPE_dvsc: TGUID = (D1:$53435644;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_dvsd: TGUID = (D1:$44535644;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_dvsd2: TGUID = (D1:$64737664;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_dvhd: TGUID = (D1:$64687664;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_dvsl : TGUID = (D1:$6C737664;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_dv25 : TGUID = '{35327664-0000-0010-8000-00aa00389b71}';
    MEDIASUBTYPE_dv50 : TGUID = '{30357664-0000-0010-8000-00aa00389b71}';
    MEDIASUBTYPE_dvh1 : TGUID = '{31687664-0000-0010-8000-00aa00389b71}';

    MEDIASUBTYPE_MDVF: TGUID = (D1:$4656444D;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_RGB1: TGUID = (D1:$E436EB78;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_RGB4: TGUID = (D1:$E436EB79;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_RGB8: TGUID = (D1:$E436EB7A;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_RGB565: TGUID = (D1:$E436EB7B;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_RGB555: TGUID = (D1:$E436EB7C;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_RGB24: TGUID = (D1:$E436EB7D;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_RGB32:  TGUID = (D1:$E436EB7E;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_ARGB32: TGUID = (D1:$773c9ac0;D2:$3274;D3:$11d0;D4:($b7,$24,$00,$aa,$00,$6c,$1a,$1 ));
    MEDIASUBTYPE_Overlay: TGUID = (D1:$E436EB7F;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_MPEG1Packet: TGUID = (D1:$E436EB80;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_MPEG1Payload: TGUID = (D1:$E436EB81;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_MPEG1AudioPayload: TGUID = (D1:$00000050;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
    MEDIASUBTYPE_MPEG1System: TGUID = (D1:$E436EB84;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_MPEG1VideoCD: TGUID = (D1:$E436EB85;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_MPEG1Video: TGUID = (D1:$E436EB86;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_MPEG1Audio: TGUID = (D1:$E436EB87;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_MPEG2DATA : TGUID = '{C892E55B-252D-42b5-A316-D997E7A5D995}';
    MEDIASUBTYPE_MPEG2_VIDEO : TGUID = '{e06d8026-db46-11cf-b4d1-00805f6cbbea}';
    MEDIASUBTYPE_MPEG2_PROGRAM : TGUID = '{e06d8022-db46-11cf-b4d1-00805f6cbbea}';
    MEDIASUBTYPE_MPEG2_TRANSPORT : TGUID = '{e06d8023-db46-11cf-b4d1-00805f6cbbea}';
    MEDIASUBTYPE_MPEG2_TRANSPORT_STRIDE : TGUID = '{138AA9A4-1EE2-4c5b-988E-19ABFDBC8A11}';
    MEDIASUBTYPE_MPEG2_AUDIO : TGUID = '{e06d802b-db46-11cf-b4d1-00805f6cbbea}';
    MEDIASUBTYPE_Avi: TGUID = (D1:$E436EB88;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
    MEDIASUBTYPE_Asf: TGUID = (D1:$3db80f90;D2:$9412;D3:$11d1;D4:($ad,$ed,$00,$00,$f8,$75,$4b,$99));
    KSDATAFORMAT_SUBTYPE_BDA_MPEG2_TRANSPORT: TGUID = '{F4AEB342-0329-4fdd-A8FD-4AFF4926C978}';

//  MEDIASUBTYPE_QTMovie: TGUID = (D1:$E436EB89;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
//  MEDIASUBTYPE_QTRpza: TGUID = (D1:$617A7072;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIASUBTYPE_QTSmc: TGUID = (D1:$20636D73;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIASUBTYPE_QTRle: TGUID = (D1:$20656C72;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIASUBTYPE_QTJpeg: TGUID = (D1:$6765706A;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIASUBTYPE_PCMAudio_Obsolete: TGUID = (D1:$E436EB8A;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
//  MEDIASUBTYPE_PCM: TGUID = (D1:$00000001;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIASUBTYPE_WAVE: TGUID = (D1:$E436EB8B;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
//  MEDIASUBTYPE_AU: TGUID = (D1:$E436EB8C;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
//  MEDIASUBTYPE_AIFF: TGUID = (D1:$E436EB8D;D2:$524F;D3:$11CE;D4:($9F,$53,$00,$20,$AF,$0B,$A7,$70));
//  MEDIASUBTYPE_dvsd_: TGUID = (D1:$64737664;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIASUBTYPE_dvhd: TGUID = (D1:$64687664;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIASUBTYPE_dvsl: TGUID = (D1:$6C737664;D2:$0000;D3:$0010;D4:($80,$00,$00,$AA,$00,$38,$9B,$71));
//  MEDIASUBTYPE_Line21_BytePair: TGUID = (D1:$6E8D4A22;D2:$310C;D3:$11D0;D4:($B7,$9A,$00,$AA,$00,$37,$67,$A7));
//  MEDIASUBTYPE_Line21_GOPPacket: TGUID = (D1:$6E8D4A23;D2:$310C;D3:$11D0;D4:($B7,$9A,$00,$AA,$00,$37,$67,$A7));
//  MEDIASUBTYPE_Line21_VBIRawData: TGUID = (D1:$6E8D4A24;D2:$310C;D3:$11D0;D4:($B7,$9A,$00,$AA,$00,$37,$67,$A7));
//  MEDIASUBTYPE_DRM_Audio: TGUID = (D1:$00000009;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
//  MEDIASUBTYPE_IEEE_FLOAT: TGUID = (D1:$00000003;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
//  MEDIASUBTYPE_DOLBY_AC3_SPDIF: TGUID = (D1:$00000092;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
//  MEDIASUBTYPE_RAW_SPORT: TGUID = (D1:$00000240;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));
//  MEDIASUBTYPE_SPDIF_TAG_241h: TGUID = (D1:$00000241;D2:$0000;D3:$0010;D4:($80,$00,$00,$aa,$00,$38,$9b,$71));

//  MEDIASUBTYPE_DssVideo: TGUID = (D1:$A0AF4F81;D2:$E163;D3:$11D0;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));
//  MEDIASUBTYPE_DssAudio: TGUID = (D1:$A0AF4F82;D2:$E163;D3:$11D0;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));
    MEDIASUBTYPE_VPVideo: TGUID = (D1:$5A9B6A40;D2:$1A22;D3:$11D1;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));
//  MEDIASUBTYPE_VPVBI: TGUID = (D1:$5A9B6A41;D2:$1A22;D3:$11D1;D4:($BA,$D9,$00,$60,$97,$44,$11,$1A));

    FORMAT_None: TGUID = (D1:$0F6417D6;D2:$c318;D3:$11d0;D4:($a4, $3f, $00, $a0, $c9, $22, $31, $96));
    FORMAT_VideoInfo: TGUID = (D1:$05589f80;D2:$c356;D3:$11ce;D4:($bf,$01,$00,$aa,$00,$55,$59,$5a));
    FORMAT_VideoInfo2: TGUID = (D1:$F72A76A0;D2:$EB0A;D3:$11D0;D4:($AC,$E4,$00,$00,$C0,$CC,$16,$BA));
    FORMAT_WaveFormatEx: TGUID = (D1:$05589F81;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));
    FORMAT_MPEG1Video: TGUID = (D1:$05589F82;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));
    FORMAT_MPEGStreams: TGUID = (D1:$05589F83;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));
    FORMAT_DvInfo: TGUID = (D1:$05589F84;D2:$C356;D3:$11CE;D4:($BF,$01,$00,$AA,$00,$55,$59,$5A));
    FORMAT_MPEG2Video : TGUID = '{e06d80e3-db46-11cf-b4d1-00805f6cbbea}';
    FORMAT_DolbyAC3 : TGUID = '{e06d80e4-db46-11cf-b4d1-00805f6cbbea}';
    FORMAT_MPEG2Audio : TGUID = '{e06d80e5-db46-11cf-b4d1-00805f6cbbea}';
    FORMAT_DVD_LPCMAudio : TGUID = '{e06d80e6-db46-11cf-b4d1-00805f6cbbea}';

//  MEDIATYPE_MPEG2_PACK : TGUID = '{36523B13-8EE5-11d1-8CA3-0060B057664A}';
//  MEDIATYPE_MPEG2_PES : TGUID = '{e06d8020-db46-11cf-b4d1-00805f6cbbea}';
//  MEDIATYPE_CONTROL: TGUID = '{e06d8021-db46-11cf-b4d1-00805f6cbbea}';
//  MEDIATYPE_MPEG2_SECTIONS: TGUID = '{455f176c-4b06-47ce-9aef-8caef73df7b5}';
//  MEDIASUBTYPE_ATSC_SI: TGUID = '{b3c7397c-d303-414d-b33c-4ed2c9d29733}';
//  MEDIASUBTYPE_DVB_SI: TGUID = '{e9dd31a3-221d-4adb-8532-9af309c1a408}';

    MPEG2VIDEOINFO : TGUID = '{e06d80e3-db46-11cf-b4d1-00805f6cbbea}';

//  MEDIASUBTYPE_DOLBY_AC3 : TGUID = '{e06d802c-db46-11cf-b4d1-00805f6cbbea}';
//  MEDIASUBTYPE_DVD_SUBPICTURE : TGUID = '{e06d802d-db46-11cf-b4d1-00805f6cbbea}';
//  MEDIASUBTYPE_DVD_LPCM_AUDIO : TGUID = '{e06d8032-db46-11cf-b4d1-00805f6cbbea}';
//  MEDIASUBTYPE_DTS : TGUID = '{e06d8033-db46-11cf-b4d1-00805f6cbbea}';
//  MEDIASUBTYPE_SDDS : TGUID = '{e06d8034-db46-11cf-b4d1-00805f6cbbea}';

//  MEDIATYPE_DVD_ENCRYPTED_PACK : TGUID = '{ED0B916A-044D-11d1-AA78-00C04FC31D60}';
//  MEDIATYPE_DVD_NAVIGATION : TGUID = '{e06d802e-db46-11cf-b4d1-00805f6cbbea}';
//  MEDIASUBTYPE_DVD_NAVIGATION_PCI : TGUID = '{e06d802f-db46-11cf-b4d1-00805f6cbbea}';
//  MEDIASUBTYPE_DVD_NAVIGATION_DSI : TGUID = '{e06d8030-db46-11cf-b4d1-00805f6cbbea}';
//  MEDIASUBTYPE_DVD_NAVIGATION_PROVIDER : TGUID = '{e06d8031-db46-11cf-b4d1-00805f6cbbea}';

//  MEDIASUBTYPE_AnalogVideo_NTSC_M: TGUID = (D1:$0482DDE2;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_PAL_B: TGUID = (D1:$0482DDE5;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_PAL_D: TGUID = (D1:$0482DDE6;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_PAL_G: TGUID = (D1:$0482DDE7;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_PAL_H: TGUID = (D1:$0482DDE8;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_PAL_I: TGUID = (D1:$0482DDE9;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_PAL_M: TGUID = (D1:$0482DDEA;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_PAL_N : TGUID = (D1:$0482DDEB;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_PAL_N_COMBO: TGUID = (D1:$482ddec;D2:$7817;D3:$11cf;D4:($8a,$3,$00,$aa,$00,$6e,$cb,$65));

//  MEDIASUBTYPE_AnalogVideo_SECAM_B: TGUID = (D1:$0482DDF0;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_SECAM_D: TGUID = (D1:$0482DDF1;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_SECAM_G: TGUID = (D1:$0482DDF2;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_SECAM_H: TGUID = (D1:$0482DDF3;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_SECAM_K: TGUID = (D1:$0482DDF4;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_SECAM_K1: TGUID = (D1:$0482DDF5;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));
//  MEDIASUBTYPE_AnalogVideo_SECAM_L: TGUID = (D1:$0482DDF6;D2:$7817;D3:$11CF;D4:($8A,$03,$00,$AA,$00,$6E,$CB,$65));

  AMPROPSETID_Pin: TGUID = (D1:$9B00F101;D2:$1567;D3:$11D1;D4:($B3,$F1,$00,$AA,$00,$37,$61,$C5));

  PIN_CATEGORY_CAPTURE: TGUID = (D1:$FB6C4281;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_PREVIEW: TGUID = (D1:$FB6C4282;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_ANALOGVIDEOIN: TGUID = (D1:$FB6C4283;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_VBI: TGUID = (D1:$FB6C4284;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_VIDEOPORT: TGUID = (D1:$FB6C4285;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_NABTS: TGUID = (D1:$FB6C4286;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_EDS: TGUID = (D1:$FB6C4287;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_TELETEXT: TGUID = (D1:$FB6C4288;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_CC: TGUID = (D1:$FB6C4289;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_STILL: TGUID = (D1:$FB6C428A;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_TIMECODE: TGUID = (D1:$FB6C428B;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));
  PIN_CATEGORY_VIDEOPORT_VBI: TGUID = (D1:$FB6C428C;D2:$0353;D3:$11D1;D4:($90,$5F,$00,$00,$C0,$CC,$16,$BA));

  TIME_FORMAT_NONE: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));
  TIME_FORMAT_FRAME: TGUID = (D1:$7B785570;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));
  TIME_FORMAT_BYTE: TGUID = (D1:$7B785571;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));
  TIME_FORMAT_SAMPLE: TGUID = (D1:$7B785572;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));
  TIME_FORMAT_FIELD: TGUID = (D1:$7B785573;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));
  TIME_FORMAT_MEDIA_TIME: TGUID = (D1:$7B785574;D2:$8C82;D3:$11CF;D4:($BC,$0C,$00,$AA,$00,$AC,$74,$F6));

    WAVE_FORMAT_MPEG = $50;
    ACM_MPEG_LAYER2 = $02;
    ACM_MPEG_STEREO = $01;
    ACM_MPEG_ID_MPEG1 = $10;

   CDEF_CLASS_DEFAULT       = $0001;
   CDEF_BYPASS_CLASS_MANAGER    = $0002;
   CDEF_MERIT_ABOVE_DO_NOT_USE   = $0008;
   CDEF_DEVMON_CMGR_DEVICE   = $0010;
   CDEF_DEVMON_DMO   = $0020;
   CDEF_DEVMON_PNP_DEVICE   = $0040;
   CDEF_DEVMON_FILTER   = $0080;
   CDEF_DEVMON_SELECTIVE_MASK   = $00f0;

type
  ISampleGrabber = interface(IUnknown)
    ['{6B652FFF-11FE-4FCE-92AD-0266B5D7C78F}']
    function SetOneShot(OneShot: BOOL): HResult; stdcall;
    function SetMediaType(var pType: TAM_MEDIA_TYPE): HResult; stdcall;
    function GetConnectedMediaType(out pType: TAM_MEDIA_TYPE): HResult; stdcall;
    function SetBufferSamples(BufferThem: BOOL): HResult; stdcall;
    function GetCurrentBuffer(var pBufferSize: LongInt; pBuffer: Pointer): HResult; stdcall;
    function GetCurrentSample(out ppSample: IMediaSample): HResult; stdcall;
    function SetCallback(pCallback: ISampleGrabberCB; WhichMethodToCallback: LongInt): HResult; stdcall;
  end;

const
  MixerPref_NoDecimation	 = $1;
  MixerPref_DecimateOutput	 = $2;
  MixerPref_DecimateMask	 = $f;
  MixerPref_BiLinearFiltering	 = $10;
  MixerPref_PointFiltering	 = $20;
  MixerPref_FilteringMask	 = $f0;
  MixerPref_RenderTargetRGB	 = $100;
  MixerPref_RenderTargetYUV420	 = $200;
  MixerPref_RenderTargetYUV422	 = $400;
  MixerPref_RenderTargetYUV444	 = $800;
  MixerPref_RenderTargetReserved = $f000;
  MixerPref_RenderTargetMask	 = $ff00;

type
  PNORMALIZEDRECT = ^TNORMALIZEDRECT;
  TNORMALIZEDRECT = record
    left   : Single;
    top    : Single;
    right  : Single;
    bottom : Single;
  end;

const
  VMRMode_Windowed = $00000001;
  VMRMode_Windowless = $00000002;
  VMRMode_Renderless = $00000004;

  type

  IVMRMixerControl = interface(IUnknown)
    ['{1c1a17b0-bed0-415d-974b-dc6696131599}']
    //Alpha = Source alpha premultication factor (global alpha for source)
    function SetAlpha(dwStreamID: dword; Alpha: single): Hresult; stdcall;
    function GetAlpha(dwStreamID: dword; out pAlpha: single): Hresult; stdcall;
    function SetZOrder(dwStreamID, dwZ: dword): Hresult; stdcall;
    function GetZOrder(dwStreamID: dword; out pZ: dword): Hresult; stdcall;
    function SetOutputRect(dwStreamID: dword; const pRect: TNORMALIZEDRECT): Hresult; stdcall;
    function GetOutputRect(dwStreamID: dword; out pRect: TNORMALIZEDRECT): Hresult; stdcall;
    function SetBackgroundClr(ClrBkg: COLORREF): HRESULT; stdcall;
    function GetBackgroundClr(out lpClrBkg: COLORREF): HRESULT; stdcall;
    function SetMixingPrefs(dwMixerPrefs: dword): HRESULT; stdcall;
    function GetMixingPrefs(pdwMixerPrefs: dword): HRESULT; stdcall;
  end;

  IVMRVideoStreamControl = interface(IUnknown)
    ['{058d1f11-2a54-4bef-bd54-df706626b727}']
    function SetColorKey(clr: PDDCOLORKEY): HResult; stdcall;
    function GetColorKey(out pclr: TDDCOLORKEY): HResult; stdcall;
    function SetStreamActiveState(fActive: BOOL): HResult; stdcall;
    function GetStreamActiveState(out lpfActive: BOOL): HResult; stdcall;
  end;

  IVMRFilterConfig7 = interface(IUnknown)
    ['{9e5530c5-7034-48b4-bb46-0b8a6efc8e36}']
    function SetImageCompositor(lpVMRImgCompositor: pointer): Hresult; stdcall;
    function SetNumberOfStreams(dwMaxStreams: dword): Hresult; stdcall;
    function GetNumberOfStreams(out pdwMaxStreams: dword): Hresult; stdcall;
    function SetRenderingPrefs(dwRenderFlags: dword): Hresult; stdcall;
    function GetRenderingPrefs(out pdwRenderFlags: dword): Hresult; stdcall;
    function SetRenderingMode(Mode: dword): Hresult; stdcall;
    function GetRenderingMode(out pMode: dword): Hresult; stdcall;
  end;

  IVMRFilterConfig9 = interface(IUnknown)
    ['{5a804648-4f66-4867-9c43-4f5c822cf1b8}']
    function SetImageCompositor(lpVMRImgCompositor: pointer): HResult; stdcall;
    function SetNumberOfStreams(dwMaxStreams: DWORD): HResult; stdcall;
    function GetNumberOfStreams(out pdwMaxStreams: DWORD): HResult; stdcall;
    function SetRenderingPrefs(dwRenderFlags: DWORD): HResult; stdcall;
    function GetRenderingPrefs(out pdwRenderFlags: DWORD): HResult; stdcall;
    function SetRenderingMode(Mode: DWORD): HResult; stdcall;
    function GetRenderingMode(out pMode: DWORD): HResult; stdcall;
  end;

  IVMRWindowlessControl7 = interface(IUnknown)
    ['{0eb1088c-4dcd-46f0-878f-39dae86a51b7}']
    function GetNativeVideoSize(lpWidth, lpHeight, lpARWidth, lpARHeight: pLongInt): HResult; stdcall;
    function GetMinIdealVideoSize(lpWidth, lpHeight: plongint): HResult; stdcall;
    function GetMaxIdealVideoSize(lpWidth, lpHeight: plongint): HResult; stdcall;
    function SetVideoPosition(lpSRCRect, lpDSTRect: PRECT): HResult; stdcall;
    function GetVideoPosition(out lpSRCRect, lpDSTRect: TRECT): HResult; stdcall;
    function GetAspectRatioMode(lpAspectRatioMode: pWORD): HResult; stdcall;
    function SetAspectRatioMode(AspectRatioMode: longint): HResult; stdcall;
    function SetVideoClippingWindow(hwnd: HWND): HResult; stdcall;
    function RepaintVideo(hwnd: HWND; hdc: HDC): HResult; stdcall;
    function DisplayModeChanged: HResult; stdcall;
    function GetCurrentImage(out lpDib): HResult; stdcall;
    function SetBorderColor(Clr: COLORREF): HResult; stdcall;
    function GetBorderColor(out lpClr: COLORREF): HResult; stdcall;
    function SetColorKey(Clr: COLORREF): HResult; stdcall;
    function GetColorKey(out lpClr: COLORREF): HResult; stdcall;
  end;

  IVMRWindowlessControl9 = interface(IUnknown)
    ['{8f537d09-f85e-4414-b23b-502e54c79927}']
    function GetNativeVideoSize(lpWidth, lpHeight, lpARWidth, lpARHeigh: pLongInt): HResult; stdcall;
    function GetMinIdealVideoSize(lpWidth, lpHeight: pLongInt): HResult; stdcall;
    function GetMaxIdealVideoSize(lpWidth, lpHeight: pLongInt): HResult; stdcall;
    function SetVideoPosition(lpSRCRect, lpDSTRect: PRECT): HResult; stdcall;
    function GetVideoPosition(out lpSRCRect, lpDSTRect: TRECT): HResult; stdcall;
    function GetAspectRatioMode(lpAspectRatioMode: pdword): HResult; stdcall;
    function SetAspectRatioMode(AspectRatioMode: longint): HResult; stdcall;
    function SetVideoClippingWindow(hwnd: HWND): HResult; stdcall;
    function RepaintVideo(hwnd: HWND; hdc: HDC): HResult; stdcall;
    function DisplayModeChanged: HResult; stdcall;
    function GetCurrentImage(out lpDib: PBYTE): HResult; stdcall;
    function SetBorderColor(Clr: COLORREF): HResult; stdcall;
    function GetBorderColor(out lpClr: COLORREF): HResult; stdcall;
  end;

  PVMRFrequency = ^TVMRFrequency;
  TVMRFrequency = record
    dwNumerator   : DWORD;
    dwDenominator : DWORD;
  end;

  TVMRSampleFormat = (
    VMR_SampleDummy,
    VMR_SampleReserved,
    VMR_SampleProgressiveFrame,
    VMR_SampleFieldInterleavedEvenFirst,
    VMR_SampleFieldInterleavedOddFirst,
    VMR_SampleFieldSingleEven,
    VMR_SampleFieldSingleOdd
  );

  PVMRVideoDesc = ^TVMRVideoDesc;
  TVMRVideoDesc = record
    dwSize          : DWORD;
    dwSampleWidth   : DWORD;
    dwSampleHeight  : DWORD;
    SampleFormat    : TVMRSampleFormat;
    dwFourCC        : DWORD;
    InputSampleFreq : TVMRFrequency;
    OutputFrameFreq : TVMRFrequency;
  end;

Const
  DeinterlaceTech_Unknown             = $0000;
  DeinterlaceTech_BOBLineReplicate    = $0001;
  DeinterlaceTech_BOBVerticalStretch  = $0002;
  DeinterlaceTech_MedianFiltering     = $0004;
  DeinterlaceTech_EdgeFiltering       = $0010;
  DeinterlaceTech_FieldAdaptive       = $0020;
  DeinterlaceTech_PixelAdaptive       = $0040;
  DeinterlaceTech_MotionVectorSteered = $0080;

Type

  PVMR9DeinterlaceCaps = ^TVMR9DeinterlaceCaps;
  TVMR9DeinterlaceCaps = record
   dwSize                    : DWORD;
   dwNumPreviousOutputFrames : DWORD;
   dwNumForwardRefSamples    : DWORD;
   dwNumBackwardRefSamples   : DWORD;
   DeinterlaceTechnology     : DWORD;
  end;

  IVMRDeinterlaceControl7 = interface(IUnknown)
    ['{bb057577-0db8-4e6a-87a7-1a8c9a505a0f}']
    function GetNumberOfDeinterlaceModes(lpVideoDescription: pVMRVideoDesc; var lpdwNumDeinterlaceModes: DWORD; lpDeinterlaceModes: pGUID): HResult; stdcall;
    function GetDeinterlaceModeCaps(lpDeinterlaceMode: pGUID; lpVideoDescription: PVMRVideoDesc; var lpDeinterlaceCaps: TVMR9DeinterlaceCaps): HResult; stdcall;
    function GetDeinterlaceMode(dwStreamID: DWORD; out lpDeinterlaceMode: TGUID): HResult; stdcall;
    function SetDeinterlaceMode(dwStreamID: DWORD; lpDeinterlaceMode: pGUID): HResult; stdcall;
    function GetDeinterlacePrefs(out lpdwDeinterlacePrefs: DWORD): HResult; stdcall;
    function SetDeinterlacePrefs(dwDeinterlacePrefs: DWORD): HResult; stdcall;
    function GetActualDeinterlaceMode(dwStreamID: DWORD; out lpDeinterlaceMode: TGUID): HResult; stdcall;
  end;

  IVMRDeinterlaceControl9 = interface(IUnknown)
    ['{a215fb8d-13c2-4f7f-993c-003d6271a459}']
    function GetNumberOfDeinterlaceModes(lpVideoDescription: pVMRVideoDesc; var lpdwNumDeinterlaceModes: DWORD; lpDeinterlaceModes: pGUID): HResult; stdcall;
    function GetDeinterlaceModeCaps(lpDeinterlaceMode: pGUID; lpVideoDescription: PVMRVideoDesc; var lpDeinterlaceCaps: TVMR9DeinterlaceCaps): HResult; stdcall;
    function GetDeinterlaceMode(dwStreamID: DWORD; out lpDeinterlaceMode: TGUID): HResult; stdcall;
    function SetDeinterlaceMode(dwStreamID: DWORD; lpDeinterlaceMode: pGUID): HResult; stdcall;
    function GetDeinterlacePrefs(out lpdwDeinterlacePrefs: DWORD): HResult; stdcall;
    function SetDeinterlacePrefs(dwDeinterlacePrefs: DWORD): HResult; stdcall;
    function GetActualDeinterlaceMode(dwStreamID: DWORD; out lpDeinterlaceMode: TGUID): HResult; stdcall;
  end;


  FOURCC = dword;                    { a four character code }

  TRIFFChunk = packed record
    fcc: FOURCC;
    cb: dword;
  end;

  TRIFFList = packed record
    fcc: FOURCC;
    cb: dword;
    fccListType: FOURCC;
  end;

const
   ckidMAINAVIHEADER = $68697661;// avih
   HapvId1 = 'Hauppauge';
   HapvId2 = 'PVR';
   HapvId3 = 'Hauppauge WinTV PVR PCI II ';
   HapvId3Tun = 'TvTuner';
   HapvId3Aud = 'TvAudio';
   HapvId3Crb = 'Crossbar';
   HapvId1Iv = 'InterVideo!NonCSS! Video Decoder';
   HapvId2Iv = 'InterVideo! Video Decoder';
   HapvId1Ia = 'InterVideo!NonCSS! Audio Decoder';
   HapvId2Ia = 'InterVideo! Audio Decoder';

type
  TAVIMainHeader = packed record
    fcc: FOURCC;                   // avih
    cb: dword;
    dwMicroSecPerFrame: dword;
    dwMaxBytesPerSec: dword;
    dwReserved1: dword;
    dwFlags: dword;
    dwTotalFrames: dword;
    dwInitialFrames: dword;
    dwStreams: dword;
    dwSuggestedBufferSize: dword;
    dwWidth: dword;
    dwHeight: dword;
    dwReserved: array[0..3] of dword;
  end;

const
  AVIF_HASINDEX       = $00000010;
  AVIF_MUSTUSEINDEX   = $00000020;
  AVIF_ISINTERLEAVED  = $00000100;
  AVIF_TRUSTCKTYPE    = $00000800;
  AVIF_WASCAPTUREFILE = $00010000;
  AVIF_COPYRIGHTED    = $00020000;

  ckidODML            = $6C6D646F; // odml
  ckidAVIEXTHEADER    = $686C6D64; // dmlh

type
  TAVIExtHeader = packed record
    fcc: FOURCC;                       //  dmlh
    cb: dword;
    dwGrandFrames: dword;
    dwFuture: array[0..60] of dword;
  end;

const
  ckidSTREAMLIST    = $6C727473; // strl
  ckidSTREAMHEADER  = $68727473; // strh


type
  TAVIStreamHeader = packed record
     fcc: FOURCC;            // strh
     cb: dword;

     fccType: FOURCC;
     fccHandler: FOURCC;
     dwFlags: dword;

     wPriority: WORD;
     wLanguage: WORD;

     dwInitialFrames: dword;
     dwScale: dword;
     dwRate: dword;
     dwStart: dword;
     dwLength: dword;
     dwSuggestedBufferSize: dword;
     dwQuality: dword;
     dwSampleSize: dword;

     rcFrame: packed record
       left: SmallInt;
       top: SmallInt;
       right: SmallInt;
       bottom: SmallInt;
     end;
  end;

const

  streamtypeVIDEO = $73646976; // vids
  streamtypeAUDIO = $73647561; // auds
  streamtypeMIDI  = $7364696D; // mids
  streamtypeTEXT  = $73747874; // txts

  AVISF_DISABLED         = $00000001;
  AVISF_VIDEO_PALCHANGES = $00010000;

  ckidSTREAMFORMAT = $66727473; // strf

  ckidAVIOLDINDEX = $31786469;// idx1

type
  TAVIOldIndex = packed record
    fcc: FOURCC;      // idx1
    cb: dword;

    aIndex: array[0..0] of packed record
      dwChunkId: dword;
      dwFlags: dword;
      dwOffset: dword;
      dwSize: dword;
    end;
  end;

const
  AVIIF_LIST       = $00000001;
  AVIIF_KEYFRAME   = $00000010;

  AVIIF_NO_TIME    = $00000100;
  AVIIF_COMPRESSOR = $0FFF0000;


type

  TIntf = record
     I: IUnknown;
     U: Boolean;
  end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure FreeMediaType(pmt: PAM_MEDIA_TYPE);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
{ frees the format and/or unknown pointer allocated in the TAM_MEDIA_TYPE struct
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
    if not assigned (pmt) then Exit;
    if (pmt^.cbFormat <> 0) then begin
        CoTaskMemFree(pmt^.pbFormat);
    end;
    pmt^.cbFormat := 0;
    { pmt^.pbFormat := nil; IF ENABLED HANGS WITH FINDRENDERER }
    if assigned (pmt^.pUnk) then begin
       pmt^.pUnk := nil;
    end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
procedure DeleteMediaType(pmt: PAM_MEDIA_TYPE);
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   if not assigned (pmt) then Exit;
   FreeMediaType (pmt);
   CoTaskMemFree (pmt);
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function WidthBytes(Value: LongInt): LongInt;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Result := ((Value + 31) div 32) * 4;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function StrToGuid (s: string): TGUID;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   sTemp: String;
begin
   sTemp := trim(s);
   if length(sTemp) <> 38 then begin
      Result := GUID_NULL;
   end
   else begin
      Result := StringToGuid (sTemp);
   end;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function _IsEqualGUID (pGUID1: pGUID; pGUID2: pGUID): Boolean;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   result := CompareMem (pGUID1, pGUID2, sizeof (TGUID));
end;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function ReturnFourCC (sGUID: string): string;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   FourCC: string;
   FourCCHex: string;
   Guid: TGuid;
begin
   Result := '';
   if length(sGUID) < 15 then Exit;
   if sGUID[1] <> '{' then Exit;
   if sGUID[10] <> '-' then Exit;

   if Copy (sGUID, 11, 4) = '524F' then begin
      FourCCHex := Copy (sGUID, 2, 8);
      if FourCCHex = 'E436EB78' then begin
         FourCC := 'RGB1';
      end
      else if FourCCHex = 'E436EB78' then begin
         FourCC := 'RGB1';
      end
      else if FourCCHex = 'E436EB79' then begin
         FourCC := 'RGB4';
      end
      else if FourCCHex = 'E436EB7A' then begin
         FourCC := 'RGB8';
      end
      else if FourCCHex = 'E436EB7B' then begin
         FourCC := 'RGB565';
      end
      else if FourCCHex = 'E436EB7C' then begin
         FourCC := 'RGB555';
      end
      else if FourCCHex = 'E436EB7D' then begin
         FourCC := 'RGB24';
      end
      else if FourCCHex = 'E436EB7E' then begin
         FourCC := 'RGB32';
      end
      else begin
         FourCC := 'RGB';
      end;
   end
   else begin
      Guid := StrToGuid (sGUID);

      if _IsEqualGUID (@Guid, @MEDIASUBTYPE_MPEG2_VIDEO) then begin
         FourCC := 'MPEG2';
      end
      else if _IsEqualGUID (@Guid, @MEDIASUBTYPE_ARGB32) then begin
         FourCC := 'ARGB32';
      end
      else begin
         FourCC := '    ';
         FourCC[1] := char (StrToInt ('$' + Copy (sGUID, 8, 2)));
         FourCC[2] := char (StrToInt ('$' + Copy (sGUID, 6, 2)));
         FourCC[3] := char (StrToInt ('$' + Copy (sGUID, 4, 2)));
         FourCC[4] := char (StrToInt ('$' + Copy (sGUID, 2, 2)));
      end;
   end;
   Result := FourCC;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function GUIDToFourCC (GUID: TGUID): DWORD;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
var
   sGUID: string;
   FourCC: string;
   dwFourCC: DWORD;
begin
   FourCC := '    ';
   sGUID := GUIDToString (GUID);
   FourCC[1] := char (StrToInt ('$' + Copy (sGUID, 8, 2)));
   FourCC[2] := char (StrToInt ('$' + Copy (sGUID, 6, 2)));
   FourCC[3] := char (StrToInt ('$' + Copy (sGUID, 4, 2)));
   FourCC[4] := char (StrToInt ('$' + Copy (sGUID, 2, 2)));
   windows.CopyMemory (@dwFourCC, @FourCC[1], 4);
   Result := dwFourCC;
end;

{………………………………………………………………………………………………………………………………………………………………………………………………………………}
function FourCCTostring (FourCC: DWORD): string;
{………………………………………………………………………………………………………………………………………………………………………………………………………………}
begin
   Result := '    ';
   Result[1] := char ((FourCC and $FF));
   Result[2] := char ((FourCC and $FF00) shr 8);
   Result[3] := char ((FourCC and $FF0000) shr 16);
   Result[4] := char ((FourCC and $FF000000) shr 24);
end;


