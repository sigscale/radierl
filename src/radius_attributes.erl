%%%---------------------------------------------------------------------
%%% @copyright 2011-2014 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2011-2014, Motivity Telecom
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
-copyright('Copyright (c) 2011-2014 Motivity Telecom').
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
attributes([{?ChapChallenge, ChapChallenge} | T], Acc) ->
	CC = list_to_binary(ChapChallenge),
	Length = size(CC) + 2,
	attributes(T, <<Acc/binary, ?ChapChallenge, Length, CC/binary>>);
attributes([{?NasPortType, NasPortType} | T], Acc) ->
	attributes(T, <<Acc/binary, ?NasPortType, 6, NasPortType:32>>);
attributes([{?PortLimit, PortLimit} | T], Acc) ->
	attributes(T, <<Acc/binary, ?PortLimit, 6, PortLimit:32>>);
attributes([{?LoginLatPort, LoginLatPort} | T], Acc) ->
	attributes(T, <<Acc/binary, ?LoginLatPort, 6, LoginLatPort:32>>).

