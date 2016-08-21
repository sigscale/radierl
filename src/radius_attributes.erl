%%%---------------------------------------------------------------------
%%% @copyright 2011-2016 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2011-2016, Motivity Telecom
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%    - Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    - Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in
%%%      the documentation and/or other materials provided with the
%%%      distribution.
%%%    - Neither the name of Motivity Telecom nor the names of its
%%%      contributors may be used to endorse or promote products derived
%%%      from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%---------------------------------------------------------------------
%%% @doc This library module implements a user API for attributes in the
%%% 		{@link //radius. radius} application.
%%%
-module(radius_attributes).
-copyright('Copyright (c) 2011-2016 Motivity Telecom').
-author('vances@motivity.ca').

%% export the radius_attributes public API
-export([new/0, store/3, fetch/2, find/2]).
-export([codec/1]).
-export([hide/3, unhide/3]).

%% @headerfile "radius.hrl"
-include("radius.hrl").

%%----------------------------------------------------------------------
%%  The radius_attributes public API
%%----------------------------------------------------------------------

-type attributes() :: orddict:orddict().

-spec new() -> Attributes :: attributes().
%% @doc Create a new RADIUS protocol attributes list.
%%
new() ->
	orddict:new().

-spec store(Attribute :: pos_integer(), Value :: term(),
	Attributes :: attributes()) -> NewAttributes :: attributes().
%% @doc Add a new attribute to a RADIUS protocol attributes list.
%%
store(Attribute, Value, Attributes) when is_integer(Attribute),
		is_list(Attributes) ->
	orddict:store(Attribute, Value, Attributes).

-spec fetch(Attribute :: pos_integer(), Attributes :: attributes()) ->
	Value :: term().
%% @doc Returns the value for an attribute in a RADIUS protocol
%% 	attributes list.  Assumes that the attribute is present.
%%
fetch(Attribute, Attributes) ->
	orddict:fetch(Attribute, Attributes).

-spec find(Attribute :: pos_integer(), Attributes :: attributes()) ->
	Result :: {ok, Value :: term()} | error.
%% 	Attribute = integer()
%% 	Attributes = attributes()
%% 	Result = {ok, Value} | error
%% 	Value = term()
%% @doc Searches for an attribute in a RADIUS protocol attributes list.
%%
find(Attribute, Attributes) ->
	orddict:find(Attribute, Attributes).

-spec codec(In :: binary() | attributes()) -> attributes() | binary().
%% @doc Encode or decode a binary RADIUS protocol attributes field.
%%
codec(In) when is_binary(In) ->
	attributes(In, 0, new());
codec(In) when is_list(In) ->
	attributes(orddict:to_list(In), <<>>).

-spec hide(SharedSecret :: string(), Authenticator :: [byte()],
	Password :: string()) -> UserPassword :: [byte()].
%% @doc Hide the password in the User-Password attribute.
%%
hide(SharedSecret, Authenticator, Password)
		when length(Password) rem 16 > 0 ->
	PadLen = 16 - length(Password) rem 16,
	Pad = lists:duplicate(PadLen, 0),
	hide(SharedSecret, Authenticator, Password ++ Pad);
hide(SharedSecret, Authenticator, Password)
		when length(SharedSecret) > 0, length(Authenticator) == 16,
		length(Password) div 16 >= 1, length(Password) div 16 =< 8,
		length(Password) rem 16 == 0 ->
	hide(SharedSecret, Authenticator, Password, []).

-spec unhide(SharedSecret :: string(), Authenticator :: [byte()],
	UserPassword :: [byte()]) -> Password :: string().
%% @doc Return the password hidden in the User-Password attribute.
%%
unhide(SharedSecret, Authenticator, UserPassword)
		when length(SharedSecret) > 0, length(Authenticator) == 16,
		length(UserPassword) div 16 >= 1, length(UserPassword) div 16 =< 8,
		length(UserPassword) rem 16 == 0 ->
	unhide(SharedSecret, Authenticator, UserPassword, []).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
hide(_, _, [], Acc) ->
	lists:flatten(lists:reverse(Acc));
hide(Secret, Salt, Password, Acc) ->
	{Phead, Ptail} = lists:split(16, Password),
	Hash = binary_to_list(erlang:md5([Secret, Salt])),
	Fxor = fun(X, Y) -> X bxor Y end,
	Result = lists:zipwith(Fxor, Phead, Hash),
	hide(Secret, Result, Ptail, [Result | Acc]).

%% @hidden
unhide(_, _, [], Acc) ->
	Padded = lists:flatten(lists:reverse(Acc)),
	Fnull = fun(0) -> false; (_) -> true end,
	lists:takewhile(Fnull, Padded);
unhide(Secret, Salt, UserPassword, Acc) ->
	{Phead, Ptail} = lists:split(16, UserPassword),
	Hash = binary_to_list(erlang:md5([Secret, Salt])),
	Fxor = fun(X, Y) -> X bxor Y end,
	Result = lists:zipwith(Fxor, Phead, Hash),
	unhide(Secret, Phead, Ptail, [Result | Acc]).

%% @hidden
attributes(Bin, Offset, Attributes) when size(Bin) =< Offset ->
	Attributes;
attributes(Bin, Offset, Acc) ->
	Type = binary:at(Bin, Offset),
	Length = binary:at(Bin, Offset + 1),
	Value = binary:part(Bin, Offset + 2, Length - 2),
	NewAcc = attribute(Type, Value, Acc),
	attributes(Bin, Offset + Length, NewAcc).

%% @hidden
attribute(?UserName, Value, Acc) when size(Value) >= 1 ->
	UserName = binary_to_list(Value),
	orddict:store(?UserName, UserName, Acc);
attribute(?UserPassword, Value, Acc)
		when size(Value) >= 16, size(Value) =< 128 ->
	UserPassword = binary_to_list(Value),
	orddict:store(?UserPassword, UserPassword, Acc);
attribute(?ChapPassword, Value, Acc) when size(Value) == 17 ->
	ChapId= binary:first(Value),
	ChapPassword = binary:bin_to_list(Value, 1, 16),
	orddict:store(?ChapPassword, {ChapId, ChapPassword}, Acc);
attribute(?NasIpAddress, Value, Acc) when size(Value) == 4 ->
	NasIpAddress = {binary:first(Value), binary:at(Value, 1),
			binary:at(Value, 2), binary:at(Value, 3)},
	orddict:store(?NasIpAddress, NasIpAddress, Acc);
attribute(?NasPort, Value, Acc) when size(Value) == 4 ->
	NasPort = binary:decode_unsigned(Value),
	orddict:store(?NasPort, NasPort, Acc);
attribute(?ServiceType, Value, Acc) when size(Value) == 4 ->
	ServiceType = binary:decode_unsigned(Value),
	orddict:store(?ServiceType, ServiceType, Acc);
attribute(?FramedProtocol, Value, Acc) when size(Value) == 4 ->
	FramedProtocol = binary:decode_unsigned(Value),
	orddict:store(?FramedProtocol, FramedProtocol, Acc);
attribute(?FramedIpAddress, Value, Acc) when size(Value) == 4 ->
	FramedIpAddress = {binary:first(Value), binary:at(Value, 1),
			binary:at(Value, 2), binary:at(Value, 3)},
	orddict:store(?FramedIpAddress, FramedIpAddress, Acc);
attribute(?FramedIpNetmask, Value, Acc) when size(Value) == 4 ->
	FramedIpNetmask = {binary:first(Value), binary:at(Value, 1),
			binary:at(Value, 2), binary:at(Value, 3)},
	orddict:store(?FramedIpNetmask, FramedIpNetmask, Acc);
attribute(?FramedRouting, Value, Acc) when size(Value) == 4 ->
	FramedRouting = binary:decode_unsigned(Value),
	orddict:store(?FramedRouting, FramedRouting, Acc);
attribute(?FilterId, Value, Acc) when size(Value) >= 1 ->
	FilterId = binary_to_list(Value),
	orddict:store(?FilterId, FilterId, Acc);
attribute(?FramedMtu, Value, Acc) when size(Value) == 4 ->
	FramedMtu = binary:decode_unsigned(Value),
	orddict:store(?FramedMtu, FramedMtu, Acc);
attribute(?FramedCompression, Value, Acc) when size(Value) == 4 ->
	FramedCompression = binary:decode_unsigned(Value),
	orddict:store(?FramedCompression, FramedCompression, Acc);
attribute(?LoginIpHost, Value, Acc) when size(Value) == 4 ->
	LoginIpHost = {binary:first(Value), binary:at(Value, 1),
			binary:at(Value, 2), binary:at(Value, 3)},
	orddict:store(?LoginIpHost, LoginIpHost, Acc);
attribute(?LoginService, Value, Acc) when size(Value) == 4 ->
	LoginService = binary:decode_unsigned(Value),
	orddict:store(?LoginService, LoginService, Acc);
attribute(?LoginTcpPort, Value, Acc) when size(Value) == 4 ->
	LoginTcpPort = binary:decode_unsigned(Value),
	orddict:store(?LoginTcpPort, LoginTcpPort, Acc);
attribute(?ReplyMessage, Value, Acc) when size(Value) >= 1 ->
	ReplyMessage = binary_to_list(Value),
	orddict:store(?ReplyMessage, ReplyMessage, Acc);
attribute(?CallbackNumber, Value, Acc) when size(Value) >= 1 ->
	CallbackNumber = binary_to_list(Value),
	orddict:store(?CallbackNumber, CallbackNumber, Acc);
attribute(?CallbackId, Value, Acc) when size(Value) >= 1 ->
	CallbackId = binary_to_list(Value),
	orddict:store(?CallbackId, CallbackId, Acc);
attribute(?FramedRoute, Value, Acc) when size(Value) >= 1 ->
	FramedRoute = binary_to_list(Value),
	orddict:store(?FramedRoute, FramedRoute, Acc);
attribute(?FramedIpxNetwork, Value, Acc) when size(Value) == 4 ->
	FramedIpxNetwork = binary:decode_unsigned(Value),
	orddict:store(?FramedIpxNetwork, FramedIpxNetwork, Acc);
attribute(?State, Value, Acc) when size(Value) >= 1 ->
	State = binary_to_list(Value),
	orddict:store(?State, State, Acc);
attribute(?Class, Value, Acc) when size(Value) >= 1 ->
	Class = binary_to_list(Value),
	orddict:store(?Class, Class, Acc);
attribute(?VendorSpecific, <<0, VendorId:24, Rest/binary>>, Acc)
		when size(Rest) >= 1 ->
	VendorSpecific = {VendorId, Rest},
	orddict:store(?VendorSpecific, VendorSpecific, Acc);
attribute(?SessionTimeout, Value, Acc) when size(Value) == 4 ->
	SessionTimeout = binary:decode_unsigned(Value),
	orddict:store(?SessionTimeout, SessionTimeout, Acc);
attribute(?IdleTimeout, Value, Acc) when size(Value) == 4 ->
	IdleTimeout = binary:decode_unsigned(Value),
	orddict:store(?IdleTimeout, IdleTimeout, Acc);
attribute(?TerminationAction, Value, Acc) when size(Value) == 4 ->
	TerminationAction = binary:decode_unsigned(Value),
	orddict:store(?TerminationAction, TerminationAction, Acc);
attribute(?CalledStationId, Value, Acc) when size(Value) >= 1 ->
	CalledStationId = binary_to_list(Value),
	orddict:store(?CalledStationId, CalledStationId, Acc);
attribute(?CallingStationId, Value, Acc) when size(Value) >= 1 ->
	CallingStationId = binary_to_list(Value),
	orddict:store(?CallingStationId, CallingStationId, Acc);
attribute(?NasIdentifier, Value, Acc) when size(Value) >= 1 ->
	NasIdentifier = binary_to_list(Value),
	orddict:store(?NasIdentifier, NasIdentifier, Acc);
attribute(?ProxyState, Value, Acc) when size(Value) >= 1 ->
	ProxyState = binary_to_list(Value),
	orddict:store(?ProxyState, ProxyState, Acc);
attribute(?LoginLatService, Value, Acc) when size(Value) >= 1 ->
	LoginLatService = binary_to_list(Value),
	orddict:store(?LoginLatService, LoginLatService, Acc);
attribute(?LoginLatNode, Value, Acc) when size(Value) >= 1 ->
	LoginLatNode = binary_to_list(Value),
	orddict:store(?LoginLatNode, LoginLatNode, Acc);
attribute(?LoginLatGroup, Value, Acc) when size(Value) == 32 ->
	LoginLatGroup = binary_to_list(Value),
	orddict:store(?LoginLatGroup, LoginLatGroup, Acc);
attribute(?FramedAppleTalkLink, Value, Acc) when size(Value) == 4 ->
	FramedAppleTalkLink = binary:decode_unsigned(Value),
	orddict:store(?FramedAppleTalkLink, FramedAppleTalkLink, Acc);
attribute(?FramedAppleTalkNetwork, Value, Acc) when size(Value) == 4 ->
	FramedAppleTalkNetwork = binary:decode_unsigned(Value),
	orddict:store(?FramedAppleTalkNetwork, FramedAppleTalkNetwork, Acc);
attribute(?FramedAppleTalkZone, Value, Acc) when size(Value) >= 1 ->
	FramedAppleTalkZone = binary_to_list(Value),
	orddict:store(?FramedAppleTalkZone, FramedAppleTalkZone, Acc);
attribute(?AcctStatusType, Value, Acc) when size(Value) == 4 ->
	AcctStatusType = binary:decode_unsigned(Value),
	orddict:store(?AcctStatusType, AcctStatusType, Acc);
attribute(?AcctDelayTime, Value, Acc) when size(Value) == 4 ->
	AcctDelayTime = binary:decode_unsigned(Value),
	orddict:store(?AcctDelayTime, AcctDelayTime, Acc);
attribute(?AcctInputOctets, Value, Acc) when size(Value) == 4 ->
	AcctInputOctets = binary:decode_unsigned(Value),
	orddict:store(?AcctInputOctets, AcctInputOctets, Acc);
attribute(?AcctOutputOctets, Value, Acc) when size(Value) == 4 ->
	AcctOutputOctets = binary:decode_unsigned(Value),
	orddict:store(?AcctOutputOctets, AcctOutputOctets, Acc);
attribute(?AcctSessionId, Value, Acc) when size(Value) >= 1 ->
	AcctSessionId = binary_to_list(Value),
	orddict:store(?AcctSessionId, AcctSessionId, Acc);
attribute(?AcctAuthentic, Value, Acc) when size(Value) == 4 ->
	AcctAuthentic = binary:decode_unsigned(Value),
	orddict:store(?AcctAuthentic, AcctAuthentic, Acc);
attribute(?AcctSessionTime, Value, Acc) when size(Value) == 4 ->
	AcctSessionTime = binary:decode_unsigned(Value),
	orddict:store(?AcctSessionTime, AcctSessionTime, Acc);
attribute(?AcctInputPackets, Value, Acc) when size(Value) == 4 ->
	AcctInputPackets = binary:decode_unsigned(Value),
	orddict:store(?AcctInputPackets, AcctInputPackets, Acc);
attribute(?AcctOutputPackets, Value, Acc) when size(Value) == 4 ->
	AcctOutputPackets = binary:decode_unsigned(Value),
	orddict:store(?AcctOutputPackets, AcctOutputPackets, Acc);
attribute(?AcctTerminateCause, Value, Acc) when size(Value) == 4 ->
	AcctTerminateCause = binary:decode_unsigned(Value),
	orddict:store(?AcctTerminateCause, AcctTerminateCause, Acc);
attribute(?AcctMultiSessionId, Value, Acc) when size(Value) >= 1 ->
	AcctMultiSessionId = binary_to_list(Value),
	orddict:store(?AcctMultiSessionId, AcctMultiSessionId, Acc);
attribute(?AcctLinkCount, Value, Acc) when size(Value) == 4 ->
	AcctLinkCount = binary:decode_unsigned(Value),
	orddict:store(?AcctLinkCount, AcctLinkCount, Acc);
attribute(?AcctInputGigawords, Value, Acc) when size(Value) == 4 ->
	GigaWordsCount = binary:decode_unsigned(Value),
	orddict:store(?AcctInputGigawords, GigaWordsCount, Acc);
attribute(?AcctOutputGigawords, Value, Acc) when size(Value) == 4 ->
	GigaWordsCount = binary:decode_unsigned(Value),
	orddict:store(?AcctOutputGigawords, GigaWordsCount, Acc);
attribute(?EventTimestamp, Value, Acc) when size(Value) == 4 ->
	Seconds = binary:decode_unsigned(Value),
	orddict:store(?EventTimestamp, Seconds, Acc);
attribute(?ChapChallenge, Value, Acc) when size(Value) >= 5 ->
	ChapChallenge = binary_to_list(Value),
	orddict:store(?ChapChallenge, ChapChallenge, Acc);
attribute(?NasPortType, Value, Acc) when size(Value) == 4 ->
	NasPortType = binary:decode_unsigned(Value),
	orddict:store(?NasPortType, NasPortType, Acc);
attribute(?PortLimit, Value, Acc) when size(Value) == 4 ->
	PortLimit = binary:decode_unsigned(Value),
	orddict:store(?PortLimit, PortLimit, Acc);
attribute(?LoginLatPort, Value, Acc) when size(Value) >= 1 ->
	LoginLatPort = binary_to_list(Value),
	orddict:store(?LoginLatPort, LoginLatPort, Acc);
attribute(?TunnelType, <<Tag, Value:24>>, Acc) ->
	orddict:store(?TunnelType, {Tag, Value}, Acc);
attribute(?TunnelMediumType, <<Tag, Value:24>>, Acc) ->
	orddict:store(?TunnelMediumType, {Tag, Value}, Acc);
attribute(?TunnelClientEndpoint, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	orddict:store(?TunnelClientEndpoint, {Tag, S}, Acc);
attribute(?TunnelServerEndpoint, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	orddict:store(?TunnelServerEndpoint, {Tag, S}, Acc);
attribute(?AcctTunnelConnection, Value, Acc) when size(Value) >= 1 ->
	orddict:store(?AcctTunnelConnection, binary_to_list(Value), Acc);
attribute(?TunnelPassword, <<Tag, Salt:16, String/binary>>, Acc) ->
	S = binary_to_list(String),
	orddict:store(?TunnelPassword, {Tag, Salt, S}, Acc);
attribute(?ARAPPassword, <<Challenge:8/binary, Response:8/binary>>, Acc) ->
	orddict:store(?ARAPPassword, {Challenge, Response}, Acc);
attribute(?ARAPFeatures, <<Change, Length, Created:32, Expires:32, Time:32>>, Acc) ->
	orddict:store(?ARAPFeatures, {Change, Length, Created, Expires, Time}, Acc);
attribute(?ARAPZoneAccess, Value, Acc) when size(Value) == 4 ->
	ZoneAccess = binary:decode_unsigned(Value),
	orddict:store(?ARAPZoneAccess, ZoneAccess, Acc);
attribute(?ARAPSecurity, Value, Acc) when size(Value) == 4 ->
	Security = binary:decode_unsigned(Value),
	orddict:store(?ARAPSecurity, Security, Acc);
attribute(?ARAPSecurityData, Data, Acc) when size(Data) >= 1 ->
	orddict:store(?ARAPSecurityData, Data, Acc);
attribute(?PasswordRetry, Value, Acc) when size(Value) == 4 ->
	Retries = binary:decode_unsigned(Value),
	orddict:store(?PasswordRetry, Retries, Acc);
attribute(?Prompt, <<0:32>>, Acc) ->
	orddict:store(?Prompt, false, Acc);
attribute(?Prompt, <<1:32>>, Acc) ->
	orddict:store(?Prompt, true, Acc);
attribute(?ConnectInfo, Value, Acc) when size(Value) >= 1 ->
	Text = binary_to_list(Value),
	orddict:store(?ConnectInfo, Text, Acc);
attribute(?ConfigurationToken, Value, Acc) when size(Value) >= 1 ->
	String= binary_to_list(Value),
	orddict:store(?ConfigurationToken, String, Acc);
attribute(?EAPMessage, Data, Acc) when size(Data) >= 1 ->
	orddict:store(?EAPMessage, Data, Acc);
attribute(?MessageAuthenticator, String, Acc) when size(String) == 18 ->
	orddict:store(?MessageAuthenticator, String, Acc);
attribute(?TunnelPrivateGroupID, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	orddict:store(?TunnelPrivateGroupID, {Tag, S}, Acc);
attribute(?TunnelAssignmentID, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	orddict:store(?TunnelAssignmentID, {Tag, S}, Acc);
attribute(?TunnelPreference, <<Tag, Value:24>>, Acc) ->
	orddict:store(?TunnelPreference, {Tag, Value}, Acc);
attribute(?ARAPChallengeResponse, String, Acc) when size(String) == 18 ->
	orddict:store(?ARAPChallengeResponse, String, Acc);
attribute(?AcctInterimInterval, Value, Acc) when size(Value) == 4 ->
	Count = binary:decode_unsigned(Value),
	orddict:store(?AcctInterimInterval, Count, Acc);
attribute(?AcctTunnelPacketsLost, Value, Acc) when size(Value) == 4 ->
	Lost = binary:decode_unsigned(Value),
	orddict:store(?AcctTunnelPacketsLost, Lost, Acc);
attribute(?NasPortId, Text, Acc) when size(Text) >= 1 ->
	S = binary_to_list(Text),
	orddict:store(?TunnelPrivateGroupID, S, Acc);
attribute(?FramedPool, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?FramedPool, S, Acc);
attribute(?CUI, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?CUI, S, Acc);
attribute(?TunnelClientAuthID, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	orddict:store(?TunnelClientAuthID, {Tag, S}, Acc);
attribute(?TunnelServerAuthID, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	orddict:store(?TunnelServerAuthID, {Tag, S}, Acc);
attribute(?NasFilterRule, Data, Acc) when size(Data) >= 1 ->
	orddict:store(?NasFilterRule, Data, Acc);
attribute(?OriginatingLineInfo, Value, Acc) when size(Value) == 2 ->
	OLI = binary:decode_unsigned(Value),
	orddict:store(?OriginatingLineInfo, OLI, Acc);
attribute(?NasIPv6Address, <<A:16, B:16, C:16, D:16,
		E:16, F:16, G:16, H:16>>, Acc) ->
	orddict:store(?NasIPv6Address, {A, B, C, D, E, F, G, H}, Acc);
attribute(?FramedInterfaceId, Value, Acc) when size(Value) == 8 ->
	InterfaceID= binary_to_list(Value),
	orddict:store(?FramedInterfaceId, InterfaceID, Acc);
attribute(?FramedIPv6Prefix, <<0, PrefixLength, Prefix/binary>>, Acc) ->
	orddict:store(?FramedInterfaceId, {PrefixLength, Prefix}, Acc);
attribute(?LoginIPv6Host, <<A:16, B:16, C:16, D:16,
		E:16, F:16, G:16, H:16>>, Acc) ->
	orddict:store(?LoginIPv6Host, {A, B, C, D, E, F, G, H}, Acc);
attribute(?FramedIPv6Route, Text, Acc) when size(Text) >= 1 ->
	S = binary_to_list(Text),
	orddict:store(?FramedIPv6Route, S, Acc);
attribute(?FramedIPv6Pool, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?FramedIPv6Pool, S, Acc);
attribute(?ErrorCause, Value, Acc) when size(Value) == 4 ->
	Cause = binary:decode_unsigned(Value),
	orddict:store(?ErrorCause, Cause, Acc);
attribute(?EAPKeyName, Data, Acc) when size(Data) >= 1 ->
	orddict:store(?EAPKeyName, Data, Acc);
attribute(?DigestResponse, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestResponse, S, Acc);
attribute(?DigestRealm, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestRealm, S, Acc);
attribute(?DigestNonce, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestNonce, S, Acc);
attribute(?DigestResponseAuth, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestResponseAuth, S, Acc);
attribute(?DigestNextnonce, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestNextnonce, S, Acc);
attribute(?DigestMethod, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestMethod, S, Acc);
attribute(?DigestURI, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestURI, S, Acc);
attribute(?DigestQop, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestQop, S, Acc);
attribute(?DigestAlgorithm, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestAlgorithm, S, Acc);
attribute(?DigestEntityBodyHash, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestEntityBodyHash, S, Acc);
attribute(?DigestCNonce, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestCNonce, S, Acc);
attribute(?DigestNonceCount, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestNonceCount, S, Acc);
attribute(?DigestUsername, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestUsername, S, Acc);
attribute(?DigestOpaque, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestOpaque, S, Acc);
attribute(?DigestAuthParam, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestAuthParam, S, Acc);
attribute(?DigestAKAAuts, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestAKAAuts, S, Acc);
attribute(?DigestDomain, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestDomain, S, Acc);
attribute(?DigestStale, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestStale, S, Acc);
attribute(?DigestHA1, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?DigestHA1, S, Acc);
attribute(?SIPAOR, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?SIPAOR, S, Acc);
attribute(?AllowedCalledStationId, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?AllowedCalledStationId, S, Acc);
attribute(?EAPPeerId, Data, Acc) when size(Data) >= 1 ->
	orddict:store(?EAPPeerId, Data, Acc);
attribute(?EAPServerId, Data, Acc) when size(Data) >= 1 ->
	orddict:store(?EAPServerId, Data, Acc);
attribute(?MobilityDomainId, <<_:16, MDID:16>>, Acc) ->
	orddict:store(?MobilityDomainId, MDID, Acc);
attribute(?PreauthTimeout, Value, Acc) when size(Value) == 4 ->
	Seconds = binary:decode_unsigned(Value),
	orddict:store(?PreauthTimeout, Seconds, Acc);
attribute(?NetworkIdName, Data, Acc) when size(Data) >= 1 ->
	orddict:store(?NetworkIdName, Data, Acc);
attribute(?EAPoLAnnouncement, Data, Acc) when size(Data) >= 1 ->
	orddict:store(?EAPoLAnnouncement, Data, Acc);
attribute(?WLANHESSID, String, Acc) when size(String) == 17 ->
	S = binary_to_list(String),
	orddict:store(?WLANHESSID, S, Acc);
attribute(?WLANVenueInfo, <<0:16, VenueGroup, VenueType>>, Acc) ->
	orddict:store(?WLANVenueInfo, {VenueGroup, VenueType}, Acc);
attribute(?WLANVenueLanguage, String, Acc)
		when size(String) == 4, size(String) == 5 ->
	Language  = binary_to_list(String),
	orddict:store(?WLANVenueLanguage, Language, Acc);
attribute(?WLANVenueName, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	orddict:store(?WLANVenueName, S, Acc);
attribute(?WLANReasonCode, Value, Acc) when size(Value) == 4 ->
	Code = binary:decode_unsigned(Value),
	orddict:store(?WLANReasonCode, Code, Acc);
attribute(?WLANPairwiseCipher, <<OUI:24, SuiteType:8>>, Acc) ->
	orddict:store(?WLANPairwiseCipher, {OUI, SuiteType}, Acc);
attribute(?WLANGroupCipher, <<OUI:24, SuiteType:8>>, Acc) ->
	orddict:store(?WLANGroupCipher, {OUI, SuiteType}, Acc);
attribute(?WLANAKMSuite, <<OUI:24, SuiteType:8>>, Acc) ->
	orddict:store(?WLANAKMSuite, {OUI, SuiteType}, Acc);
attribute(?WLANGroupMgmtCipher, <<OUI:24, SuiteType:8>>, Acc) ->
	orddict:store(?WLANGroupMgmtCipher, {OUI, SuiteType}, Acc);
attribute(?WLANRFBand, <<_:24, RfBand:8>>, Acc) ->
	orddict:store(?WLANRFBand, RfBand, Acc);
attribute(_, _Value, Acc) ->
	Acc.

%% @hidden
attributes([], Attributes) ->
	Attributes;
attributes([{?UserName, UserName} | T], Acc) ->
	UN = list_to_binary(UserName),
	Length = size(UN) + 2,
	attributes(T, <<Acc/binary, ?UserName, Length, UN/binary>>);
attributes([{?UserPassword, UserPassword} | T], Acc) ->
	UP = list_to_binary(UserPassword),
	Length = size(UP) + 2,
	attributes(T, <<Acc/binary, ?UserPassword, Length, UP/binary>>);
attributes([{?ChapPassword, {ChapId, ChapPassword}} | T], Acc)
		when is_integer(ChapId), length(ChapPassword) =:= 16 ->
	BinChapPassword = list_to_binary(ChapPassword),
	attributes([{?ChapPassword, {ChapId, BinChapPassword}} | T], Acc);
attributes([{?ChapPassword, {ChapId, ChapPassword}} | T], Acc)
		when is_integer(ChapId), is_binary(ChapPassword),
		size(ChapPassword) =:= 16 ->
	Length = size(ChapPassword) + 3,
	attributes(T, <<Acc/binary, ?ChapPassword,
			Length, ChapId, ChapPassword/binary>>);
attributes([{?NasIpAddress, {A, B, C, D}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?NasIpAddress, 6, A, B, C, D>>);
attributes([{?NasPort, NasPort} | T], Acc) ->
	attributes(T, <<Acc/binary, ?NasPort, 6, NasPort:32>>);
attributes([{?ServiceType, ServiceType} | T], Acc) ->
	attributes(T, <<Acc/binary, ?ServiceType, 6, ServiceType:32>>);
attributes([{?FramedProtocol, FramedProtocol} | T], Acc) ->
	attributes(T, <<Acc/binary, ?FramedProtocol, 6, FramedProtocol:32>>);
attributes([{?FramedIpAddress, {A, B, C, D}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?FramedIpAddress, 6, A, B, C, D>>);
attributes([{?FramedIpNetmask, {A, B, C, D}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?FramedIpNetmask, 6, A, B, C, D>>);
attributes([{?FramedRouting, FramedRouting} | T], Acc) ->
	attributes(T, <<Acc/binary, ?FramedRouting, 6, FramedRouting:32>>);
attributes([{?FilterId, FilterId} | T], Acc) ->
	FI = list_to_binary(FilterId),
	Length = size(FI) + 2,
	attributes(T, <<Acc/binary, ?FilterId, Length, FI/binary>>);
attributes([{?FramedMtu, FramedMtu} | T], Acc) ->
	attributes(T, <<Acc/binary, ?FramedMtu, 6, FramedMtu:32>>);
attributes([{?FramedCompression, FramedCompression} | T], Acc) ->
	attributes(T, <<Acc/binary, ?FramedCompression, 6, FramedCompression:32>>);
attributes([{?LoginIpHost, {A, B, C, D}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?LoginIpHost, 6, A, B, C, D>>);
attributes([{?LoginService, LoginService} | T], Acc) ->
	attributes(T, <<Acc/binary, ?LoginService, 6, LoginService:32>>);
attributes([{?LoginTcpPort, LoginTcpPort} | T], Acc) ->
	attributes(T, <<Acc/binary, ?LoginTcpPort, 6, LoginTcpPort:32>>);
attributes([{?ReplyMessage, ReplyMessage} | T], Acc) ->
	RM = list_to_binary(ReplyMessage),
	Length = size(RM) + 2,
	attributes(T, <<Acc/binary, ?ReplyMessage, Length, RM/binary>>);
attributes([{?CallbackNumber, CallbackNumber} | T], Acc) ->
	CN = list_to_binary(CallbackNumber),
	Length = size(CN) + 2,
	attributes(T, <<Acc/binary, ?CallbackNumber, Length, CN/binary>>);
attributes([{?CallbackId, CallbackId} | T], Acc) ->
	CI = list_to_binary(CallbackId),
	Length = size(CI) + 2,
	attributes(T, <<Acc/binary, ?CallbackId, Length, CI/binary>>);
attributes([{?FramedRoute, FramedRoute} | T], Acc) ->
	FR = list_to_binary(FramedRoute),
	Length = size(FR) + 2,
	attributes(T, <<Acc/binary, ?FramedRoute, Length, FR/binary>>);
attributes([{?FramedIpxNetwork, FramedIpxNetwork} | T], Acc) ->
	attributes(T, <<Acc/binary, ?FramedIpxNetwork, 6, FramedIpxNetwork:32>>);
attributes([{?State, State} | T], Acc) ->
	ST = list_to_binary(State),
	Length = size(ST) + 2,
	attributes(T, <<Acc/binary, ?State, Length, ST/binary>>);
attributes([{?Class, Class} | T], Acc) ->
	CL = list_to_binary(Class),
	Length = size(CL) + 2,
	attributes(T, <<Acc/binary, ?Class, Length, CL/binary>>);
attributes([{?VendorSpecific, {VendorId, Bin}} | T], Acc)
		when is_integer(VendorId), is_binary(Bin) ->
	Length = size(Bin) + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length,
			0, VendorId:24, Bin/binary>>);
attributes([{?SessionTimeout, SessionTimeout} | T], Acc) ->
	attributes(T, <<Acc/binary, ?SessionTimeout, 6, SessionTimeout:32>>);
attributes([{?IdleTimeout, IdleTimeout} | T], Acc) ->
	attributes(T, <<Acc/binary, ?IdleTimeout, 6, IdleTimeout:32>>);
attributes([{?TerminationAction, TerminationAction} | T], Acc) ->
	attributes(T, <<Acc/binary, ?TerminationAction, 6, TerminationAction:32>>);
attributes([{?CalledStationId, CalledStationId} | T], Acc) ->
	CI = list_to_binary(CalledStationId),
	Length = size(CI) + 2,
	attributes(T, <<Acc/binary, ?CalledStationId, Length, CI/binary>>);
attributes([{?CallingStationId, CallingStationId} | T], Acc) ->
	CI = list_to_binary(CallingStationId),
	Length = size(CI) + 2,
	attributes(T, <<Acc/binary, ?CallingStationId, Length, CI/binary>>);
attributes([{?NasIdentifier, NasIdentifier} | T], Acc) ->
	NI = list_to_binary(NasIdentifier),
	Length = size(NI) + 2,
	attributes(T, <<Acc/binary, ?NasIdentifier, Length, NI/binary>>);
attributes([{?ProxyState, ProxyState} | T], Acc) ->
	PS = list_to_binary(ProxyState),
	Length = size(PS) + 2,
	attributes(T, <<Acc/binary, ?ProxyState, Length, PS/binary>>);
attributes([{?LoginLatService, LoginLatService} | T], Acc) ->
	LT = list_to_binary(LoginLatService),
	Length = size(LT) + 2,
	attributes(T, <<Acc/binary, ?LoginLatService, Length, LT/binary>>);
attributes([{?LoginLatNode, LoginLatNode} | T], Acc) ->
	LT = list_to_binary(LoginLatNode),
	Length = size(LT) + 2,
	attributes(T, <<Acc/binary, ?LoginLatNode, Length, LT/binary>>);
attributes([{?LoginLatGroup, LoginLatGroup} | T], Acc) ->
	LT = list_to_binary(LoginLatGroup),
	Length = size(LT) + 2,
	attributes(T, <<Acc/binary, ?LoginLatGroup, Length, LT/binary>>);
attributes([{?FramedAppleTalkLink, FramedAppleTalkLink} | T], Acc) ->
	attributes(T, <<Acc/binary, ?FramedAppleTalkLink, 6, FramedAppleTalkLink:32>>);
attributes([{?FramedAppleTalkNetwork, FramedAppleTalkNetwork} | T], Acc) ->
	attributes(T, <<Acc/binary, ?FramedAppleTalkNetwork, 6, FramedAppleTalkNetwork:32>>);
attributes([{?FramedAppleTalkZone, FramedAppleTalkZone} | T], Acc) ->
	FZ = list_to_binary(FramedAppleTalkZone),
	Length = size(FZ) + 2,
	attributes(T, <<Acc/binary, ?FramedAppleTalkZone, Length, FZ/binary>>);
attributes([{?AcctStatusType, AcctStatusType} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctStatusType, 6, AcctStatusType:32>>);
attributes([{?AcctDelayTime, AcctDelayTime} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctDelayTime, 6, AcctDelayTime:32>>);
attributes([{?AcctInputOctets, AcctInputOctets} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctInputOctets, 6, AcctInputOctets:32>>);
attributes([{?AcctOutputOctets, AcctOutputOctets} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctOutputOctets, 6, AcctOutputOctets:32>>);
attributes([{?AcctSessionId, AcctSessionId} | T], Acc) ->
	SI = list_to_binary(AcctSessionId),
	Length = size(SI) + 2,
	attributes(T, <<Acc/binary, ?AcctSessionId, Length, SI/binary>>);
attributes([{?AcctAuthentic, AcctAuthentic} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctAuthentic, 6, AcctAuthentic:32>>);
attributes([{?AcctSessionTime, AcctSessionTime} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctSessionTime, 6, AcctSessionTime:32>>);
attributes([{?AcctInputPackets, AcctInputPackets} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctInputPackets, 6, AcctInputPackets:32>>);
attributes([{?AcctOutputPackets, AcctOutputPackets} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctOutputPackets, 6, AcctOutputPackets:32>>);
attributes([{?AcctTerminateCause, AcctTerminateCause} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctTerminateCause, 6, AcctTerminateCause:32>>);
attributes([{?AcctMultiSessionId, AcctMultiSessionId} | T], Acc) ->
	SI = list_to_binary(AcctMultiSessionId),
	Length = size(SI) + 2,
	attributes(T, <<Acc/binary, ?AcctMultiSessionId, Length, SI/binary>>);
attributes([{?AcctLinkCount, AcctLinkCount} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctLinkCount, 6, AcctLinkCount:32>>);
attributes([{?AcctInputGigawords, GigaWordCount} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctInputGigawords, 6, GigaWordCount:32>>);
attributes([{?AcctOutputGigawords, GigaWordCount} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctOutputGigawords, 6, GigaWordCount:32>>);
attributes([{?EventTimestamp, Seconds} | T], Acc) ->
	attributes(T, <<Acc/binary, ?EventTimestamp, 6, Seconds:32>>);
attributes([{?ChapChallenge, ChapChallenge} | T], Acc) ->
	CC = list_to_binary(ChapChallenge),
	Length = size(CC) + 2,
	attributes(T, <<Acc/binary, ?ChapChallenge, Length, CC/binary>>);
attributes([{?NasPortType, NasPortType} | T], Acc) ->
	attributes(T, <<Acc/binary, ?NasPortType, 6, NasPortType:32>>);
attributes([{?PortLimit, PortLimit} | T], Acc) ->
	attributes(T, <<Acc/binary, ?PortLimit, 6, PortLimit:32>>);
attributes([{?LoginLatPort, LoginLatPort} | T], Acc) ->
	attributes(T, <<Acc/binary, ?LoginLatPort, 6, LoginLatPort:32>>);
attributes([{?TunnelType, {Tag, Value}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?TunnelType, 6, Tag, Value:24>>);
attributes([{?TunnelMediumType, {Tag, Value}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?TunnelMediumType, 6, Tag, Value:24>>);
attributes([{?TunnelClientEndpoint, {Tag, String}} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 3,
	attributes(T, <<Acc/binary, ?TunnelClientEndpoint, Length, Tag, S/binary>>);
attributes([{?TunnelServerEndpoint, {Tag, String}} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 3,
	attributes(T, <<Acc/binary, ?TunnelServerEndpoint, Length, Tag, S/binary>>);
attributes([{?AcctTunnelConnection, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?AcctTunnelConnection, Length, S/binary>>);
attributes([{?TunnelPassword, {Tag, Salt, String}} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 5,
	attributes(T, <<Acc/binary, ?TunnelPassword, Length, Tag, Salt:16, S/binary>>);
attributes([{?ARAPPassword, {Challenge, Response}} | T], Acc)
		when size(Challenge) == 8, size(Response) == 8  ->
	attributes(T, <<Acc/binary, ?ARAPPassword, 20,
		Challenge/binary, Response/binary>>);
attributes([{?ARAPFeatures, {Change, Length, Created, Expires, Time}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?ARAPFeatures, 18, Change, Length, Created:32,
			Expires:32, Time:32>>);
attributes([{?ARAPZoneAccess, ZoneAccess} | T], Acc) ->
	attributes(T, <<Acc/binary, ?ARAPZoneAccess, 6, ZoneAccess:32>>);
attributes([{?ARAPSecurity, Security} | T], Acc) ->
	attributes(T, <<Acc/binary, ?ARAPSecurity, 6, Security:32>>);
attributes([{?ARAPSecurityData, Data} | T], Acc) ->
	Length = size(Data) + 2,
	attributes(T, <<Acc/binary, ?ARAPSecurityData, Length, Data/binary>>);
attributes([{?PasswordRetry, Retries} | T], Acc) ->
	attributes(T, <<Acc/binary, ?PasswordRetry, 6, Retries:32>>);
attributes([{?Prompt, false} | T], Acc) ->
	attributes(T, <<Acc/binary, ?Prompt, 6, 0:32>>);
attributes([{?Prompt, true} | T], Acc) ->
	attributes(T, <<Acc/binary, ?Prompt, 6, 1:32>>);
attributes([{?ConnectInfo, Text} | T], Acc) ->
	S = list_to_binary(Text),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?ConnectInfo, Length, S/binary>>);
attributes([{?ConfigurationToken, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?ConfigurationToken, Length, S/binary>>);
attributes([{?EAPMessage, Data} | T], Acc) ->
	Length = size(Data) + 2,
	attributes(T, <<Acc/binary, ?EAPMessage, Length, Data/binary>>);
attributes([{?MessageAuthenticator, String} | T], Acc) ->
	S = list_to_binary(String),
	attributes(T, <<Acc/binary, ?MessageAuthenticator, 18, S/binary>>);
attributes([{?TunnelPrivateGroupID, {Tag, String}} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 3,
	attributes(T, <<Acc/binary, ?TunnelPrivateGroupID, Length, Tag, S/binary>>);
attributes([{?TunnelAssignmentID, {Tag, String}} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 3,
	attributes(T, <<Acc/binary, ?TunnelAssignmentID, Length, Tag, S/binary>>);
attributes([{?TunnelPreference, {Tag, Value}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?TunnelPreference, 6, Tag, Value:24>>);
attributes([{?ARAPChallengeResponse, Data} | T], Acc) ->
	attributes(T, <<Acc/binary, ?ARAPChallengeResponse, 10, Data/binary>>);
attributes([{?AcctInterimInterval, Count} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctInterimInterval, 6, Count:32>>);
attributes([{?AcctTunnelPacketsLost, Lost} | T], Acc) ->
	attributes(T, <<Acc/binary, ?AcctTunnelPacketsLost, 6, Lost:32>>);
attributes([{?NasPortId, Text} | T], Acc) ->
	S = list_to_binary(Text),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?NasPortId, Length, S/binary>>);
attributes([{?FramedPool, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?FramedPool, Length, S/binary>>);
attributes([{?CUI, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?CUI, Length, S/binary>>);
attributes([{?TunnelClientAuthID, {Tag, String}} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 3,
	attributes(T, <<Acc/binary, ?TunnelClientAuthID, Length, Tag, S/binary>>);
attributes([{?TunnelServerAuthID, {Tag, String}} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 3,
	attributes(T, <<Acc/binary, ?TunnelServerAuthID, Length, Tag, S/binary>>);
attributes([{?NasFilterRule, Data} | T], Acc) ->
	Length = size(Data) + 2,
	attributes(T, <<Acc/binary, ?NasFilterRule, Length, Data/binary>>);
attributes([{?OriginatingLineInfo, OLI} | T], Acc) ->
	attributes(T, <<Acc/binary, ?NasFilterRule, 4, OLI:16>>);
attributes([{?NasIPv6Address, {A, B, C, D, E, F, G, H}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?NasIPv6Address, 18, A:16, B:16, C:16,
			D:16, E:16, F:16, G:16, H:16>>);
attributes([{?FramedInterfaceId, InterfaceID} | T], Acc) ->
	S = list_to_binary(InterfaceID),
	attributes(T, <<Acc/binary, ?FramedInterfaceId, 10, S/binary>>);
attributes([{?FramedIPv6Prefix, {PrefixLength, Prefix}} | T], Acc) ->
	Length = size(Prefix) + 4,
	attributes(T, <<Acc/binary, ?FramedIPv6Prefix, Length, 0,
			PrefixLength, Prefix/binary>>);
attributes([{?LoginIPv6Host, {A, B, C, D, E, F, G, H}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?LoginIPv6Host, 18, A:16, B:16, C:16,
			D:16, E:16, F:16, G:16, H:16>>);
attributes([{?FramedIPv6Route, Text} | T], Acc) ->
	S = list_to_binary(Text),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?FramedIPv6Route, Length, S/binary>>);
attributes([{?FramedIPv6Pool, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?FramedIPv6Pool, Length, S/binary>>);
attributes([{?ErrorCause, Cause} | T], Acc) ->
	attributes(T, <<Acc/binary, ?ErrorCause, 6, Cause:32>>);
attributes([{?EAPKeyName, Data} | T], Acc) ->
	Length = size(Data) + 2,
	attributes(T, <<Acc/binary, ?EAPKeyName, Length, Data/binary>>);
attributes([{?DigestResponse, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestResponse, Length, S/binary>>);
attributes([{?DigestRealm, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestRealm, Length, S/binary>>);
attributes([{?DigestNonce, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestNonce, Length, S/binary>>);
attributes([{?DigestResponseAuth, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestResponseAuth, Length, S/binary>>);
attributes([{?DigestNextnonce, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestNextnonce, Length, S/binary>>);
attributes([{?DigestMethod, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestMethod, Length, S/binary>>);
attributes([{?DigestURI, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestURI, Length, S/binary>>);
attributes([{?DigestQop, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestQop, Length, S/binary>>);
attributes([{?DigestAlgorithm, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestAlgorithm, Length, S/binary>>);
attributes([{?DigestEntityBodyHash, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestEntityBodyHash, Length, S/binary>>);
attributes([{?DigestCNonce, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestCNonce, Length, S/binary>>);
attributes([{?DigestNonceCount, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestNonceCount, Length, S/binary>>);
attributes([{?DigestUsername, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestUsername, Length, S/binary>>);
attributes([{?DigestOpaque, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestOpaque, Length, S/binary>>);
attributes([{?DigestAuthParam, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestAuthParam, Length, S/binary>>);
attributes([{?DigestAKAAuts, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestAKAAuts, Length, S/binary>>);
attributes([{?DigestDomain, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestDomain, Length, S/binary>>);
attributes([{?DigestStale, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestStale, Length, S/binary>>);
attributes([{?DigestHA1, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?DigestHA1, Length, S/binary>>);
attributes([{?SIPAOR, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?SIPAOR, Length, S/binary>>);
attributes([{?AllowedCalledStationId, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?AllowedCalledStationId, Length, S/binary>>);
attributes([{?EAPPeerId, EAPPeerId} | T], Acc) ->
	Length = size(EAPPeerId) + 2,
	attributes(T, <<Acc/binary, ?EAPPeerId, Length, EAPPeerId/binary>>);
attributes([{?EAPServerId, EAPServerId} | T], Acc) ->
	Length = size(EAPServerId) + 2,
	attributes(T, <<Acc/binary, ?EAPServerId, Length, EAPServerId/binary>>);
attributes([{?MobilityDomainId, MDID} | T], Acc) ->
	attributes(T, <<Acc/binary, ?MobilityDomainId, 6, 0:16, MDID:16>>);
attributes([{?PreauthTimeout, Seconds} | T], Acc) ->
	attributes(T, <<Acc/binary, ?PreauthTimeout, 6, Seconds:32>>);
attributes([{?NetworkIdName, Data} | T], Acc) ->
	Length = size(Data) + 2,
	attributes(T, <<Acc/binary, ?NetworkIdName, Length, Data/binary>>);
attributes([{?EAPoLAnnouncement, Data} | T], Acc) ->
	Length = size(Data) + 2,
	attributes(T, <<Acc/binary, ?EAPoLAnnouncement, Length, Data/binary>>);
attributes([{?WLANHESSID, String} | T], Acc) when length(String) == 17->
	S = list_to_binary(String),
	attributes(T, <<Acc/binary, ?WLANHESSID, 19, S/binary>>);
attributes([{?WLANVenueInfo, {VenueGroup, VenueType}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?WLANVenueInfo, 6,
			0:16, VenueGroup, VenueType>>);
attributes([{?WLANVenueLanguage, Language} | T], Acc) ->
	S = list_to_binary(Language),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?WLANVenueLanguage, Length, S/binary>>);
attributes([{?WLANVenueName, String} | T], Acc) ->
	S = list_to_binary(String),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?WLANVenueName, Length, S/binary>>);
attributes([{?WLANReasonCode, Code} | T], Acc) ->
	attributes(T, <<Acc/binary, ?WLANReasonCode, 6, Code:32>>);
attributes([{?WLANPairwiseCipher, {OUI, SuiteType}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?WLANPairwiseCipher, 6,
			OUI:24, SuiteType:8>>);
attributes([{?WLANGroupCipher, {OUI, SuiteType}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?WLANGroupCipher, 6,
			OUI:24, SuiteType:8>>);
attributes([{?WLANAKMSuite, {OUI, SuiteType}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?WLANAKMSuite, 6,
			OUI:24, SuiteType:8>>);
attributes([{?WLANGroupMgmtCipher, {OUI, SuiteType}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?WLANGroupMgmtCipher, 6,
			OUI:24, SuiteType:8>>);
attributes([{?WLANRFBand, RfBand} | T], Acc) ->
	attributes(T, <<Acc/binary, ?WLANRFBand, 6, 0:24, RfBand:8>>).

