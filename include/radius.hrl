%%% $Id%
%%%

%% define macros for RADIUS Codes
-define(AccessRequest,       1).
-define(AccessAccept,        2).
-define(AccessReject,        3).
-define(AccountingRequest,   4).
-define(AccountingResponse,  5).
-define(AccessChallenge,    11).
-define(StatusServer,       12).
-define(StatusClient,       13).

-record(radius,
      {code :: byte(),
      id :: byte(),
      authenticator :: binary() | [byte()],
      attributes :: binary() | [byte()]}).

%% define macros for RADIUS Attribute Types
-define(UserName,                1).
-define(UserPassword,            2).
-define(ChapPassword,            3).
-define(NasIpAddress,            4).
-define(NasPort,                 5).
-define(ServiceType,             6).
-define(FramedProtocol,          7).
-define(FramedIpAddress,         8).
-define(FramedIpNetmask,         9).
-define(FramedRouting,          10).
-define(FilterId,               11).
-define(FramedMtu,              12).
-define(FramedCompression,      13).
-define(LoginIpHost,            14).
-define(LoginService,           15).
-define(LoginTcpPort,           16).
-define(ReplyMessage,           18).
-define(CallbackNumber,         19).
-define(CallbackId,             20).
-define(FramedRoute,            22).
-define(FramedIpxNetwork,       23).
-define(State,                  24).
-define(Class,                  25).
-define(VendorSpecific,         26).
-define(SessionTimeout,         27).
-define(IdleTimeout,            28).
-define(TerminationAction,      29).
-define(CalledStationId,        30).
-define(CallingStationId,       31).
-define(NasIdentifier,          32).
-define(ProxyState,             33).
-define(LoginLatService,        34).
-define(LoginLatNode,           35).
-define(LoginLatGroup,          36).
-define(FramedAppleTalkLink,    37).
-define(FramedAppleTalkNetwork, 38).
-define(FramedAppleTalkZone,    39).
-define(AcctStatusType,         40).
-define(AcctDelayTime,          41).
-define(AcctInputOctets,        42).
-define(AcctOutputOctets,       43).
-define(AcctSessionId,          44).
-define(AcctAuthentic,          45).
-define(AcctSessionTime,        46).
-define(AcctInputPackets,       47).
-define(AcctOutputPackets,      48).
-define(AcctTerminateCause,     49).
-define(AcctMultiSessionId,     50).
-define(AcctLinkCount,          51).
-define(ChapChallenge,          60).
-define(NasPortType,            61).
-define(PortLimit,              62).
-define(LoginLatPort,           63).

%% define macros for AcctStatusType attribute values
-define(AccountingStart, 1).
-define(AccountingStop, 2).
-define(AccountingInterimUpdate, 3).
-define(AccountingON, 7).
-define(AccountingOFF, 8).

