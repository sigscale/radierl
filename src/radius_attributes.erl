%%%---------------------------------------------------------------------
%%% @copyright 2016-2018 SigScale Global Inc
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%%
%%% Copyright (c) 2016-2018, SigScale Global Inc
%%%
%%% All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%---------------------------------------------------------------------
%%% @doc This library module implements a user API for attributes in the
%%% 		{@link //radius. radius} application.
%%%
-module(radius_attributes).
-copyright('Copyright (c) 2016-2018 SigScale Global Inc').
-author('vances@sigscale.org').

%% export the radius_attributes public API
-export([new/0, store/3, store/4, add/3, add/4,
		fetch/2, fetch/3, find/2, find/3, get_all/2]).
-export([codec/1]).
-export([hide/3, unhide/3, error_cause/1]).

-export_type([attributes/0]).

%% @headerfile "radius.hrl"
-include("radius.hrl").

%%----------------------------------------------------------------------
%%  The radius_attributes public API
%%----------------------------------------------------------------------

-type attributes() :: list({Attribute :: byte(), Value :: term()}).

-spec new() -> Attributes :: attributes().
%% @doc Create a new RADIUS protocol attributes list.
%%
new() ->
	[].

-spec store(Attribute, Value, Attributes) -> Attributes
	when
		Attribute :: byte(),
		Value :: term(),
		Attributes :: attributes().
%% @doc Add a new attribute to a RADIUS protocol attributes list.
%% 	If `Attribute' exists it is overwritten with the new `Value'.
%%
store(Attribute, Value, Attributes)
		when is_integer(Attribute), is_list(Attributes) ->
	lists:keystore(Attribute, 1, Attributes, {Attribute, Value}).

-spec store(Vendor, Attribute, Value, Attributes) -> Attributes
	when
		Vendor :: 1..16#ffffff,
		Attribute :: byte(),
		Value :: term(),
		Attributes :: attributes().
%% @doc Add a new vendor specific attribute to a RADIUS protocol
%% attributes list.
%% 	If `Attribute' exists it is overwritten with the new `Value'.
%%
store(Vendor, Attribute, Value, Attributes)
		when is_integer(Attribute), is_list(Attributes) ->
	store1(Vendor, Attribute, Value, Attributes, []).
%% @hidden
store1(Vendor, Attribute, Value,
		[{?VendorSpecific, {Vendor, {Attribute, _}}} | T ], Acc) ->
	lists:reverse(Acc) ++ [{?VendorSpecific, {Vendor, {Attribute, Value}}} | T];
store1(Vendor, Attribute, Value, [H | T], Acc) ->
	store1(Vendor, Attribute, Value, T, [H | Acc]);
store1(Vendor, Attribute, Value, [], Acc) ->
	lists:reverse([{?VendorSpecific, {Vendor, {Attribute, Value}}} | Acc]).

-spec add(Attribute, Value, Attributes) -> Attributes
	when
		Attribute :: byte(),
		Value :: term(),
		Attributes :: attributes().
%% @doc Add an attribute to a RADIUS protocol attributes list.
%% 	Multiple `Attribute's are allowed.
%%
add(Attribute, Value, Attributes) when is_integer(Attribute),
		is_list(Attributes) ->
	Attributes ++ [{Attribute, Value}].

-spec add(Vendor, Attribute, Value, Attributes) -> Attributes
	when
		Vendor :: 1..16#ffffff,
		Attribute :: byte(),
		Value :: term(),
		Attributes :: attributes().
%% @doc Add vendor specific attribute to a RADIUS protocol attributes list.
%% 	Multiple `Attribute's are allowed.
%%
add(Vendor, Attribute, Value, Attributes) when is_integer(Attribute),
		is_list(Attributes) ->
	Attributes ++ [{?VendorSpecific, {Vendor, {Attribute, Value}}}].

-spec fetch(Attribute, Attributes) -> Value
	when
		Attribute :: byte(),
		Attributes :: attributes(),
		Value :: term().
%% @doc Returns the value for an attribute in a RADIUS protocol
%% 	attributes list.  Assumes that the attribute is present.
%%
fetch(Attribute, Attributes) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		false ->
			exit(not_found);
		{Attribute, Value} ->
			Value
	end.

-spec fetch(Vendor, Attribute, Attributes) -> Value
	when
		Vendor :: 1..16#ffffff,
		Attribute :: byte(),
		Attributes :: attributes(),
		Value :: term().
%% @doc Returns the value for a vendor sepecific attribute in a RADIUS protocol
%% 	attributes list.  Assumes that the attribute is present.
%%
fetch(Vendor, Attribute, [{?VendorSpecific, {Vendor, {Attribute, Value}}} | _]) ->
	Value;
fetch(Vendor, Attribute, [_ | T]) ->
	fetch(Vendor, Attribute, T);
fetch(_Vendor, _Attribute, []) ->
	exit(not_found).

-spec find(Attribute, Attributes) -> Result
	when
		Attribute :: byte(),
		Attributes :: attributes(),
		Value :: term(),
		Result :: {ok, Value} | {error, not_found}.
%% @doc Searches for an attribute in a RADIUS protocol attributes list.
%%
find(Attribute, Attributes) ->
	case lists:keyfind(Attribute, 1, Attributes) of
		false ->
			{error, not_found};
		{Attribute, Value} ->
			{ok, Value}
	end.

-spec find(Vendor, Attribute, Attributes) -> Result
	when
		Vendor :: 1..16#ffffff,
		Attribute :: byte(),
		Attributes :: attributes(),
		Value :: term(),
		Result :: {ok, Value} | {error, not_found}.
%% @doc Searches for a vendor specific attribute in a
%% 	RADIUS protocol attributes list.
%%
find(Vendor, Attribute, [{?VendorSpecific, {Vendor, {Attribute, Value}}} | _]) ->
	{ok, Value};
find(Vendor, Attribute, [_ | T]) ->
	find(Vendor, Attribute, T);
find(_Vendor, _Attribute, []) ->
	{error, not_found}.

-spec get_all(Attribute, Attributes) -> Result
	when
		Attribute :: byte(),
		Attributes :: attributes(),
		Value :: term(),
		Result :: [Value].
%% @doc Returns all values for an `Attribute' which may occur
%% 	more than once in	the RADIUS protocol `Attributes' list.
%%
get_all(Attribute, Attributes) ->
	F = fun({Name, Value}) when Name == Attribute ->
				{true, Value};
			(_) ->
				false
	end,
	lists:filtermap(F, Attributes).

-spec codec(Attributes) -> Attributes
	when
		Attributes :: binary() | attributes().
%% @doc Encode or decode a binary RADIUS protocol attributes field.
%%
codec(Attributes) when is_binary(Attributes) ->
	attributes(Attributes, 0, new());
codec(Attributes) when is_list(Attributes) ->
	attributes(Attributes, <<>>).

-spec hide(SharedSecret, Authenticator, Password) -> UserPassword
	when
		SharedSecret :: string() | binary(),
		Authenticator :: [byte()] | binary(),
		Password :: string() | binary(),
		UserPassword :: string().
%% @doc Hide the password in the User-Password attribute.
%%
hide(SharedSecret, Authenticator, Password)
		when is_binary(SharedSecret) ->
	hide(binary_to_list(SharedSecret), Authenticator, Password);
hide(SharedSecret, Authenticator, Password)
		when is_binary(Authenticator) ->
	hide(SharedSecret, binary_to_list(Authenticator), Password);
hide(SharedSecret, Authenticator, Password)
		when is_binary(Password) ->
	hide(SharedSecret, Authenticator, binary_to_list(Password));
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

-spec unhide(SharedSecret, Authenticator, UserPassword) -> Password
	when
		SharedSecret :: string() | binary(),
		Authenticator :: [byte()] | binary(),
		UserPassword :: string() | binary(),
		Password :: string().
%% @doc Return the password hidden in the User-Password attribute.
%%
unhide(SharedSecret, Authenticator, UserPassword)
		when is_binary(SharedSecret) ->
	unhide(binary_to_list(SharedSecret), Authenticator, UserPassword);
unhide(SharedSecret, Authenticator, UserPassword)
		when is_binary(Authenticator) ->
	unhide(SharedSecret, binary_to_list(Authenticator), UserPassword);
unhide(SharedSecret, Authenticator, UserPassword)
		when is_binary(UserPassword) ->
	unhide(SharedSecret, Authenticator, binary_to_list(UserPassword));
unhide(SharedSecret, Authenticator, UserPassword)
		when length(SharedSecret) > 0, length(Authenticator) == 16,
		length(UserPassword) div 16 >= 1, length(UserPassword) div 16 =< 8,
		length(UserPassword) rem 16 == 0 ->
	unhide(SharedSecret, Authenticator, UserPassword, []).

-spec error_cause(ErrorCause) -> Result
	when
		ErrorCause :: byte(),
		Result :: string().
%% @doc Given the value of an `Error-Cause' attribute returns a
%% 	description in English.
error_cause(201) ->
	"Residual Session Context Removed";
error_cause(202) ->
	"Invalid EAP Packet (Ignored)";
error_cause(401) ->
	"Unsupported Attribute";
error_cause(402) ->
	"Missing Attribute";
error_cause(403) ->
	"NAS Identification Mismatch";
error_cause(404) ->
	"Invalid Request";
error_cause(405) ->
	"Unsupported Service";
error_cause(406) ->
	"Unsupported Extension";
error_cause(501) ->
	"Administratively Prohibited";
error_cause(502) ->
	"Request Not Routable (Proxy)";
error_cause(503) ->
	"Session Context Not Found";
error_cause(504) ->
	"Session Context Not Removable";
error_cause(505) ->
	"Other Proxy Processing Error";
error_cause(506) ->
	"Resources Unavailable";
error_cause(507) ->
	"Request Initiated".

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
hide(_, _, [], Acc) ->
	lists:flatten(lists:reverse(Acc));
hide(Secret, Salt, Password, Acc) ->
	{Phead, Ptail} = lists:split(16, Password),
	Hash = binary_to_list(crypto:hash(md5, [Secret, Salt])),
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
	Hash = binary_to_list(crypto:hash(md5, [Secret, Salt])),
	Fxor = fun(X, Y) -> X bxor Y end,
	Result = lists:zipwith(Fxor, Phead, Hash),
	unhide(Secret, Phead, Ptail, [Result | Acc]).

%% @hidden
attributes(Bin, Offset, Attributes) when size(Bin) =< Offset ->
	lists:reverse(Attributes);
attributes(Bin, Offset, Acc) ->
	Type = binary:at(Bin, Offset),
	Length = binary:at(Bin, Offset + 1),
	Value = binary:part(Bin, Offset + 2, Length - 2),
	NewAcc = attribute(Type, Value, Acc),
	attributes(Bin, Offset + Length, NewAcc).

%% @hidden
attribute(?UserName, Value, Acc) when size(Value) >= 1 ->
	UserName = binary_to_list(Value),
	[{?UserName, UserName} | Acc];
attribute(?UserPassword, Value, Acc)
		when size(Value) >= 16, size(Value) =< 128 ->
	UserPassword = binary_to_list(Value),
	[{?UserPassword, UserPassword} | Acc];
attribute(?ChapPassword, Value, Acc) when size(Value) == 17 ->
	ChapId= binary:first(Value),
	ChapPassword = binary:bin_to_list(Value, 1, 16),
	[{?ChapPassword, {ChapId, ChapPassword}} | Acc];
attribute(?NasIpAddress, Value, Acc) when size(Value) == 4 ->
	NasIpAddress = {binary:first(Value), binary:at(Value, 1),
			binary:at(Value, 2), binary:at(Value, 3)},
	[{?NasIpAddress, NasIpAddress} | Acc];
attribute(?NasPort, Value, Acc) when size(Value) == 4 ->
	NasPort = binary:decode_unsigned(Value),
	[{?NasPort, NasPort} | Acc];
attribute(?ServiceType, Value, Acc) when size(Value) == 4 ->
	ServiceType = binary:decode_unsigned(Value),
	[{?ServiceType, ServiceType} | Acc];
attribute(?FramedProtocol, Value, Acc) when size(Value) == 4 ->
	FramedProtocol = binary:decode_unsigned(Value),
	[{?FramedProtocol, FramedProtocol} | Acc];
attribute(?FramedIpAddress, Value, Acc) when size(Value) == 4 ->
	FramedIpAddress = {binary:first(Value), binary:at(Value, 1),
			binary:at(Value, 2), binary:at(Value, 3)},
	[{?FramedIpAddress, FramedIpAddress} | Acc];
attribute(?FramedIpNetmask, Value, Acc) when size(Value) == 4 ->
	FramedIpNetmask = {binary:first(Value), binary:at(Value, 1),
			binary:at(Value, 2), binary:at(Value, 3)},
	[{?FramedIpNetmask, FramedIpNetmask} | Acc];
attribute(?FramedRouting, Value, Acc) when size(Value) == 4 ->
	FramedRouting = binary:decode_unsigned(Value),
	[{?FramedRouting, FramedRouting} | Acc];
attribute(?FilterId, Value, Acc) when size(Value) >= 1 ->
	FilterId = binary_to_list(Value),
	[{?FilterId, FilterId} | Acc];
attribute(?FramedMtu, Value, Acc) when size(Value) == 4 ->
	FramedMtu = binary:decode_unsigned(Value),
	[{?FramedMtu, FramedMtu} | Acc];
attribute(?FramedCompression, Value, Acc) when size(Value) == 4 ->
	FramedCompression = binary:decode_unsigned(Value),
	[{?FramedCompression, FramedCompression} | Acc];
attribute(?LoginIpHost, Value, Acc) when size(Value) == 4 ->
	LoginIpHost = {binary:first(Value), binary:at(Value, 1),
			binary:at(Value, 2), binary:at(Value, 3)},
	[{?LoginIpHost, LoginIpHost} | Acc];
attribute(?LoginService, Value, Acc) when size(Value) == 4 ->
	LoginService = binary:decode_unsigned(Value),
	[{?LoginService, LoginService} | Acc];
attribute(?LoginTcpPort, Value, Acc) when size(Value) == 4 ->
	LoginTcpPort = binary:decode_unsigned(Value),
	[{?LoginTcpPort, LoginTcpPort} | Acc];
attribute(?ReplyMessage, Value, Acc) when size(Value) >= 1 ->
	ReplyMessage = binary_to_list(Value),
	[{?ReplyMessage, ReplyMessage} | Acc];
attribute(?CallbackNumber, Value, Acc) when size(Value) >= 1 ->
	CallbackNumber = binary_to_list(Value),
	[{?CallbackNumber, CallbackNumber} | Acc];
attribute(?CallbackId, Value, Acc) when size(Value) >= 1 ->
	CallbackId = binary_to_list(Value),
	[{?CallbackId, CallbackId} | Acc];
attribute(?FramedRoute, Value, Acc) when size(Value) >= 1 ->
	FramedRoute = binary_to_list(Value),
	[{?FramedRoute, FramedRoute} | Acc];
attribute(?FramedIpxNetwork, Value, Acc) when size(Value) == 4 ->
	FramedIpxNetwork = binary:decode_unsigned(Value),
	[{?FramedIpxNetwork, FramedIpxNetwork} | Acc];
attribute(?State, Value, Acc) when size(Value) >= 1 ->
	State = binary_to_list(Value),
	[{?State, State} | Acc];
attribute(?Class, Value, Acc) when size(Value) >= 1 ->
	Class = binary_to_list(Value),
	[{?Class, Class} | Acc];
attribute(?VendorSpecific, <<0, ?Cisco:24, ?H323CallOrigin,
		8, $a, $n, $s, $w, $e, $r>>, Acc) ->
	[{?VendorSpecific, {?Cisco, {?H323CallOrigin, answer}}} | Acc];
attribute(?VendorSpecific, <<0, ?Cisco:24, ?H323CallOrigin,
		11, $o, $r, $i, $g, $i, $n, $a, $t, $e>>, Acc) ->
	[{?VendorSpecific, {?Cisco, {?H323CallOrigin, originate}}} | Acc];
attribute(?VendorSpecific, <<0, ?Cisco:24, ?H323CallOrigin,
		10, $c, $a, $l, $l, $b, $a, $c, $k>>, Acc) ->
	[{?VendorSpecific, {?Cisco, {?H323CallOrigin, originate}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChapChallenge,
		VendorLength, MsChapChallenge/binary>>, Acc)
		when VendorLength > 2 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChapChallenge, MsChapChallenge}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChapResponse, 52, Ident,
		Flags, LmResponse:24/binary, NtResponse:24/binary>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChapResponse, {Ident, Flags, LmResponse, NtResponse}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChapDomain, VendorLength,
		Ident, String/binary>>, Acc) when VendorLength > 3 ->
	Domain = binary_to_list(String),
	[{?VendorSpecific, {?Microsoft,
			{?MsChapDomain, {Ident, Domain}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChapError, VendorLength,
		Ident, String/binary>>, Acc) when VendorLength > 3 ->
	Error = binary_to_list(String),
	[{?VendorSpecific, {?Microsoft,
			{?MsChapError, {Ident, Error}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChapCpw1, 72, Code, Ident,
		LmOldPassword:16/binary, LmNewPassword:16/binary,
		NtOldPassword:16/binary, NtNewPassword:16/binary,
		NewLmPasswordLength:16, Flags:16>>, Acc) when Code == 5 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChapCpw1, {Code, Ident, LmOldPassword, LmNewPassword,
			NtOldPassword, NtNewPassword, NewLmPasswordLength, Flags}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChapCpw2, 86, Code, Ident,
		OldNtHash:16/binary, OldLmHash:16/binary, LmResponse:24/binary,
		NtResponse:24/binary, Flags:16>>, Acc) when Code == 6 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChapCpw2, {Code, Ident, OldNtHash, OldLmHash, LmResponse,
			NtResponse, Flags}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChapLmEncPw,
		VendorLength, Code, Ident, SequenceNumber:16, String/binary>>, Acc)
		when VendorLength > 6, Code == 6 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChapLmEncPw, {Code, Ident, SequenceNumber, String}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChapNtEncPw,
		VendorLength, Code, Ident, SequenceNumber:16, String/binary>>, Acc)
		when VendorLength > 6, Code == 6 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChapNtEncPw, {Code, Ident, SequenceNumber, String}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChap2Response, 52,
		Ident, Flags, PeerChallenge:128, Reserved:16/binary,
		Response:24/binary>>, Acc) when Flags == 0, Reserved == 0 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChap2Response, {Ident, Flags, PeerChallenge,
			Reserved, Response}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChap2Success, 45, Ident,
		Authenticator:42/binary>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChap2Success, {Ident, Authenticator}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChap2Cpw, 70, Code, Ident,
		EncryptedHash:16/binary, NtResponse:24/binary, Flags:16>>, Acc)
		when Code == 7, Flags == 0 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChap2Cpw, {Code, Ident, EncryptedHash,
			NtResponse, Flags}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsChapMppeKeys, 34,
		Keys:32/binary>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsChapMppeKeys, Keys}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsMppeSendKey, VendorLength,
		Salt:16, Key/binary>>, Acc) when VendorLength > 4, (Salt bsr 15) == 1 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsMppeSendKey, {Salt, Key}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsMppeRecvKey, VendorLength,
		Salt:16, Key/binary>>, Acc) when VendorLength > 4, (Salt bsr 15) == 1 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsMppeRecvKey, {Salt, Key}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsMppeEncryptionPolicy, 6,
		1:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsMppeEncryptionPolicy, allowed}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsMppeEncryptionPolicy, 6,
		2:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsMppeEncryptionPolicy, required}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsMppeEncryptionTypes, 6,
		_:29, 0:1, 1:1, 0:1>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsMppeEncryptionTypes, [rc4_40bit]}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsMppeEncryptionTypes, 6,
		_:29, 1:1, 0:1, 0:1>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsMppeEncryptionTypes, [rc4_128bit]}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsMppeEncryptionTypes, 6,
		_:29, 1:1, 1:1, 0:1>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsMppeEncryptionTypes, [rc4_40bit, rc4_128bit]}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsBapUsage, 6, 0:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsBapUsage, not_allowed}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsBapUsage, 6, 1:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsBapUsage, allowed}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsBapUsage, 6, 2:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsBapUsage, required}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsLinkUtilizationThreshold, 6,
		Threshold:32>>, Acc) when Threshold > 0, Threshold =< 100 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsLinkUtilizationThreshold, Threshold}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsLinkDropTimeLimit, 6,
		Seconds:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsLinkDropTimeLimit, Seconds}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsOldArapPassword,
		VendorLength, Password/binary>>, Acc) when VendorLength > 3 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsOldArapPassword, Password}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsNewArapPassword,
		VendorLength, Password/binary>>, Acc) when VendorLength > 3 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsNewArapPassword, Password}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsArapPasswordChangeReason,
		6, 1:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsArapPasswordChangeReason, just_change}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsArapPasswordChangeReason,
		6, 2:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsArapPasswordChangeReason, expired}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsArapPasswordChangeReason,
		6, 3:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsArapPasswordChangeReason, admin_requires}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsArapPasswordChangeReason,
		6, 4:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsArapPasswordChangeReason, too_short}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsArapChallenge, 10,
		Challenge:8/binary>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsArapChallenge, Challenge}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsRasVendor, 6,
		0:8, VendorID:24>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsRasVendor, VendorID}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsRasVersion, VendorLength,
		Version/binary>>, Acc) when VendorLength > 3 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsRasVersion, Version}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsFilter, VendorLength,
		Filter/binary>>, Acc) when VendorLength > 3 ->
	[{?VendorSpecific, {?Microsoft,
			{?MsFilter, Filter}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsAcctAuthType,
		6, 1:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsAcctAuthType, pap}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsAcctAuthType,
		6, 2:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsAcctAuthType, chap}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsAcctAuthType,
		6, 3:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsAcctAuthType, mschap1}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsAcctAuthType,
		6, 4:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsAcctAuthType, mschap2}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsAcctAuthType,
		6, 5:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsAcctAuthType, eap}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsAcctEapType,
		6, 4:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsAcctEapType, md5}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsAcctEapType,
		6, 5:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsAcctEapType, otp}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsAcctEapType,
		6, 6:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsAcctEapType, gtc}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsAcctEapType,
		6, 13:32>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsAcctEapType, tls}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsPrimaryDnsServer,
		6, A, B, C, D>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsPrimaryDnsServer, {A, B, C, D}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsSecondaryDnsServer,
		6, A, B, C, D>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsSecondaryDnsServer, {A, B, C, D}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsPrimaryNbnsServer,
		6, A, B, C, D>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsPrimaryNbnsServer, {A, B, C, D}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Microsoft:24, ?MsSecondaryNbnsServer,
		6, A, B, C, D>>, Acc) ->
	[{?VendorSpecific, {?Microsoft,
			{?MsSecondaryNbnsServer, {A, B, C, D}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Ascend:24, ?AscendClientGateway,
		6, A, B, C, D>>, Acc) ->
	[{?VendorSpecific, {?Ascend,
			{?AscendClientGateway, {A, B, C, D}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Ascend:24, ?AscendDataRate,
		6, DataRate:32>>, Acc) ->
	[{?VendorSpecific, {?Ascend,
			{?AscendDataRate, DataRate}}} | Acc];
attribute(?VendorSpecific, <<0, ?Ascend:24, ?AscendXmitRate,
		6, XmitRate:32>>, Acc) ->
	[{?VendorSpecific, {?Ascend,
			{?AscendXmitRate, XmitRate}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikRecvLimit,
		6, RecvLimit:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik, {?MikrotikRecvLimit, RecvLimit}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikXmitLimit,
		6, XmitLimit:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik, {?MikrotikXmitLimit, XmitLimit}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikGroup,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Group = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikGroup, Group}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessForward,
		6, Forward:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik, {?MikrotikWirelessForward, Forward}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessSkipDot1x,
		6, SkipDot1x:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikWirelessSkipDot1x, SkipDot1x}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessEncAlgo,
		6, 0:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikWirelessEncAlgo, 'no-encryption'}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessEncAlgo,
		6, 1:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikWirelessEncAlgo, '40-bit-wep'}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessEncAlgo,
		6, 2:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikWirelessEncAlgo, '104-bit-wep'}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessEncAlgo,
		6, 3:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikWirelessEncAlgo, 'aes-ccm'}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessEncAlgo,
		6, 4:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikWirelessEncAlgo, tkip}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessEncKey,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Key = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikWirelessEncKey, Key}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikRateLimit,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Rate = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikRateLimit, Rate}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikRealm,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Realm = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikRealm, Realm}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikHostIp,
		6, A, B, C, D>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik, {?MikrotikHostIp, {A, B, C, D}}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikMarkId,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	ID = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikMarkId, ID}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikAdvertiseUrl,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	URL = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikAdvertiseUrl, URL}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikAdvertiseInterval,
		6, Interval:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikAdvertiseInterval, Interval}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikRecvLimitGigawords,
		6, Gigawords:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikRecvLimitGigawords, Gigawords}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikXmitLimitGigawords,
		6, Gigawords:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikXmitLimitGigawords, Gigawords}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessPsk,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	PSK = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikWirelessPsk, PSK}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikTotalLimit,
		6, Limit:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik, {?MikrotikTotalLimit, Limit}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikTotalLimitGigawords,
		6, Gigawords:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikTotalLimitGigawords, Gigawords}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikAddressList,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	List = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikAddressList, List}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessMpKey,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Key = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikWirelessMpKey, Key}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessComment,
		6, String/binary>>, Acc) ->
	Comment = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikWirelessComment, Comment}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikDelegatedIpv6Pool,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Pool = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikDelegatedIpv6Pool, Pool}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikDhcpOptionSet,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Set = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik, {?MikrotikDhcpOptionSet, Set}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikDhcpOptionParamStr1,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Str1 = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikDhcpOptionParamStr1, Str1}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikDhcpOptionParamStr2,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Str2 = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikDhcpOptionParamStr2, Str2}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessVlanId,
		6, ID:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik, {?MikrotikWirelessVlanId, ID}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessVlanIdType,
		6, Type:32>>, Acc) ->
	[{?VendorSpecific, {?Mikrotik, {?MikrotikWirelessVlanIdType, Type}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessMinSignal,
		6, String/binary>>, Acc) ->
	Signal = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikWirelessMinSignal, Signal}}} | Acc];
attribute(?VendorSpecific, <<0, ?Mikrotik:24, ?MikrotikWirelessMaxSignal,
		VendorLength, String/binary>>, Acc) when VendorLength > 2 ->
	Signal = binary_to_list(String),
	[{?VendorSpecific, {?Mikrotik,
			{?MikrotikWirelessMaxSignal, Signal}}} | Acc];
attribute(?VendorSpecific, <<0, VendorID:24, Rest/binary>>, Acc)
		when size(Rest) >= 1 ->
	VendorSpecific = {VendorID, Rest},
	[{?VendorSpecific, VendorSpecific} | Acc];
attribute(?SessionTimeout, Value, Acc) when size(Value) == 4 ->
	SessionTimeout = binary:decode_unsigned(Value),
	[{?SessionTimeout, SessionTimeout} | Acc];
attribute(?IdleTimeout, Value, Acc) when size(Value) == 4 ->
	IdleTimeout = binary:decode_unsigned(Value),
	[{?IdleTimeout, IdleTimeout} | Acc];
attribute(?TerminationAction, Value, Acc) when size(Value) == 4 ->
	TerminationAction = binary:decode_unsigned(Value),
	[{?TerminationAction, TerminationAction} | Acc];
attribute(?CalledStationId, Value, Acc) when size(Value) >= 1 ->
	CalledStationId = binary_to_list(Value),
	[{?CalledStationId, CalledStationId} | Acc];
attribute(?CallingStationId, Value, Acc) when size(Value) >= 1 ->
	CallingStationId = binary_to_list(Value),
	[{?CallingStationId, CallingStationId} | Acc];
attribute(?NasIdentifier, Value, Acc) when size(Value) >= 1 ->
	NasIdentifier = binary_to_list(Value),
	[{?NasIdentifier, NasIdentifier} | Acc];
attribute(?ProxyState, Value, Acc) when size(Value) >= 1 ->
	ProxyState = binary_to_list(Value),
	[{?ProxyState, ProxyState} | Acc];
attribute(?LoginLatService, Value, Acc) when size(Value) >= 1 ->
	LoginLatService = binary_to_list(Value),
	[{?LoginLatService, LoginLatService} | Acc];
attribute(?LoginLatNode, Value, Acc) when size(Value) >= 1 ->
	LoginLatNode = binary_to_list(Value),
	[{?LoginLatNode, LoginLatNode} | Acc];
attribute(?LoginLatGroup, Value, Acc) when size(Value) == 32 ->
	LoginLatGroup = binary_to_list(Value),
	[{?LoginLatGroup, LoginLatGroup} | Acc];
attribute(?FramedAppleTalkLink, Value, Acc) when size(Value) == 4 ->
	FramedAppleTalkLink = binary:decode_unsigned(Value),
	[{?FramedAppleTalkLink, FramedAppleTalkLink} | Acc];
attribute(?FramedAppleTalkNetwork, Value, Acc) when size(Value) == 4 ->
	FramedAppleTalkNetwork = binary:decode_unsigned(Value),
	[{?FramedAppleTalkNetwork, FramedAppleTalkNetwork} | Acc];
attribute(?FramedAppleTalkZone, Value, Acc) when size(Value) >= 1 ->
	FramedAppleTalkZone = binary_to_list(Value),
	[{?FramedAppleTalkZone, FramedAppleTalkZone} | Acc];
attribute(?AcctStatusType, Value, Acc) when size(Value) == 4 ->
	AcctStatusType = binary:decode_unsigned(Value),
	[{?AcctStatusType, AcctStatusType} | Acc];
attribute(?AcctDelayTime, Value, Acc) when size(Value) == 4 ->
	AcctDelayTime = binary:decode_unsigned(Value),
	[{?AcctDelayTime,AcctDelayTime} | Acc];
attribute(?AcctInputOctets, Value, Acc) when size(Value) == 4 ->
	AcctInputOctets = binary:decode_unsigned(Value),
	[{?AcctInputOctets, AcctInputOctets} | Acc];
attribute(?AcctOutputOctets, Value, Acc) when size(Value) == 4 ->
	AcctOutputOctets = binary:decode_unsigned(Value),
	[{?AcctOutputOctets, AcctOutputOctets} | Acc];
attribute(?AcctSessionId, Value, Acc) when size(Value) >= 1 ->
	AcctSessionId = binary_to_list(Value),
	[{?AcctSessionId, AcctSessionId} | Acc];
attribute(?AcctAuthentic, Value, Acc) when size(Value) == 4 ->
	AcctAuthentic = binary:decode_unsigned(Value),
	[{?AcctAuthentic, AcctAuthentic} | Acc];
attribute(?AcctSessionTime, Value, Acc) when size(Value) == 4 ->
	AcctSessionTime = binary:decode_unsigned(Value),
	[{?AcctSessionTime, AcctSessionTime} | Acc];
attribute(?AcctInputPackets, Value, Acc) when size(Value) == 4 ->
	AcctInputPackets = binary:decode_unsigned(Value),
	[{?AcctInputPackets, AcctInputPackets} | Acc];
attribute(?AcctOutputPackets, Value, Acc) when size(Value) == 4 ->
	AcctOutputPackets = binary:decode_unsigned(Value),
	[{?AcctOutputPackets, AcctOutputPackets} | Acc];
attribute(?AcctTerminateCause, Value, Acc) when size(Value) == 4 ->
	AcctTerminateCause = binary:decode_unsigned(Value),
	[{?AcctTerminateCause, AcctTerminateCause} | Acc];
attribute(?AcctMultiSessionId, Value, Acc) when size(Value) >= 1 ->
	AcctMultiSessionId = binary_to_list(Value),
	[{?AcctMultiSessionId, AcctMultiSessionId} | Acc];
attribute(?AcctLinkCount, Value, Acc) when size(Value) == 4 ->
	AcctLinkCount = binary:decode_unsigned(Value),
	[{?AcctLinkCount, AcctLinkCount} | Acc];
attribute(?AcctInputGigawords, Value, Acc) when size(Value) == 4 ->
	GigaWordsCount = binary:decode_unsigned(Value),
	[{?AcctInputGigawords, GigaWordsCount} | Acc];
attribute(?AcctOutputGigawords, Value, Acc) when size(Value) == 4 ->
	GigaWordsCount = binary:decode_unsigned(Value),
	[{?AcctOutputGigawords, GigaWordsCount} | Acc];
attribute(?EventTimestamp, Value, Acc) when size(Value) == 4 ->
	Seconds = binary:decode_unsigned(Value),
	[{?EventTimestamp, Seconds} | Acc];
attribute(?ChapChallenge, Value, Acc) when size(Value) >= 5 ->
	ChapChallenge = binary_to_list(Value),
	[{?ChapChallenge, ChapChallenge} | Acc];
attribute(?NasPortType, Value, Acc) when size(Value) == 4 ->
	NasPortType = binary:decode_unsigned(Value),
	[{?NasPortType, NasPortType} | Acc];
attribute(?PortLimit, Value, Acc) when size(Value) == 4 ->
	PortLimit = binary:decode_unsigned(Value),
	[{?PortLimit, PortLimit} | Acc];
attribute(?LoginLatPort, Value, Acc) when size(Value) >= 1 ->
	LoginLatPort = binary_to_list(Value),
	[{?LoginLatPort, LoginLatPort} | Acc];
attribute(?TunnelType, <<Tag, Value:24>>, Acc) ->
	[{?TunnelType, {Tag, Value}} | Acc];
attribute(?TunnelMediumType, <<Tag, Value:24>>, Acc) ->
	[{?TunnelMediumType, {Tag, Value}} | Acc];
attribute(?TunnelClientEndpoint, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	[{?TunnelClientEndpoint, {Tag, S}} | Acc];
attribute(?TunnelServerEndpoint, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	[{?TunnelServerEndpoint, {Tag, S}} | Acc];
attribute(?AcctTunnelConnection, Value, Acc) when size(Value) >= 1 ->
	[{?AcctTunnelConnection, binary_to_list(Value)} | Acc];
attribute(?TunnelPassword, <<Tag, Salt:16, String/binary>>, Acc) ->
	S = binary_to_list(String),
	[{?TunnelPassword, {Tag, Salt, S}} | Acc];
attribute(?ARAPPassword, <<Challenge:8/binary, Response:8/binary>>, Acc) ->
	[{?ARAPPassword, {Challenge, Response}} | Acc];
attribute(?ARAPFeatures, <<Change, Length, Created:32, Expires:32, Time:32>>, Acc) ->
	[{?ARAPFeatures, {Change, Length, Created, Expires, Time}} | Acc];
attribute(?ARAPZoneAccess, Value, Acc) when size(Value) == 4 ->
	ZoneAccess = binary:decode_unsigned(Value),
	[{?ARAPZoneAccess, ZoneAccess} | Acc];
attribute(?ARAPSecurity, Value, Acc) when size(Value) == 4 ->
	Security = binary:decode_unsigned(Value),
	[{?ARAPSecurity, Security} | Acc];
attribute(?ARAPSecurityData, Data, Acc) when size(Data) >= 1 ->
	[{?ARAPSecurityData, Data} | Acc];
attribute(?PasswordRetry, Value, Acc) when size(Value) == 4 ->
	Retries = binary:decode_unsigned(Value),
	[{?PasswordRetry, Retries} | Acc];
attribute(?Prompt, <<0:32>>, Acc) ->
	[{?Prompt, false} | Acc];
attribute(?Prompt, <<1:32>>, Acc) ->
	[{?Prompt, true} | Acc];
attribute(?ConnectInfo, Value, Acc) when size(Value) >= 1 ->
	Text = binary_to_list(Value),
	[{?ConnectInfo, Text} | Acc];
attribute(?ConfigurationToken, Value, Acc) when size(Value) >= 1 ->
	String= binary_to_list(Value),
	[{?ConfigurationToken, String} | Acc];
attribute(?EAPMessage, Data, Acc) when size(Data) >= 1 ->
	[{?EAPMessage, Data} | Acc];
attribute(?MessageAuthenticator, String, Acc) when size(String) == 16 ->
	[{?MessageAuthenticator, String} | Acc];
attribute(?TunnelPrivateGroupID, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	[{?TunnelPrivateGroupID, {Tag, S}} | Acc];
attribute(?TunnelAssignmentID, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	[{?TunnelAssignmentID, {Tag, S}} | Acc];
attribute(?TunnelPreference, <<Tag, Value:24>>, Acc) ->
	[{?TunnelPreference, {Tag, Value}} | Acc];
attribute(?ARAPChallengeResponse, String, Acc) when size(String) == 8 ->
	[{?ARAPChallengeResponse, String} | Acc];
attribute(?AcctInterimInterval, Value, Acc) when size(Value) == 4 ->
	Count = binary:decode_unsigned(Value),
	[{?AcctInterimInterval, Count} | Acc];
attribute(?AcctTunnelPacketsLost, Value, Acc) when size(Value) == 4 ->
	Lost = binary:decode_unsigned(Value),
	[{?AcctTunnelPacketsLost, Lost} | Acc];
attribute(?NasPortId, Text, Acc) when size(Text) >= 1 ->
	S = binary_to_list(Text),
	[{?NasPortId, S} | Acc];
attribute(?FramedPool, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?FramedPool, S} | Acc];
attribute(?CUI, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?CUI, S} | Acc];
attribute(?TunnelClientAuthID, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	[{?TunnelClientAuthID, {Tag, S}} | Acc];
attribute(?TunnelServerAuthID, <<Tag, String/binary>>, Acc) ->
	S = binary_to_list(String),
	[{?TunnelServerAuthID, {Tag, S}} | Acc];
attribute(?NasFilterRule, Data, Acc) when size(Data) >= 1 ->
	[{?NasFilterRule, Data} | Acc];
attribute(?OriginatingLineInfo, Value, Acc) when size(Value) == 2 ->
	OLI = binary:decode_unsigned(Value),
	[{?OriginatingLineInfo, OLI} | Acc];
attribute(?NasIPv6Address, <<A:16, B:16, C:16, D:16,
		E:16, F:16, G:16, H:16>>, Acc) ->
	[{?NasIPv6Address, {A, B, C, D, E, F, G, H}} | Acc];
attribute(?FramedInterfaceId, Value, Acc) when size(Value) == 8 ->
	InterfaceID= binary_to_list(Value),
	[{?FramedInterfaceId, InterfaceID} | Acc];
attribute(?FramedIPv6Prefix, <<0, PrefixLength, Prefix/binary>>, Acc) ->
	[{?FramedIPv6Prefix, {PrefixLength, Prefix}} | Acc];
attribute(?LoginIPv6Host, <<A:16, B:16, C:16, D:16,
		E:16, F:16, G:16, H:16>>, Acc) ->
	[{?LoginIPv6Host, {A, B, C, D, E, F, G, H}} | Acc];
attribute(?FramedIPv6Route, Text, Acc) when size(Text) >= 1 ->
	S = binary_to_list(Text),
	[{?FramedIPv6Route, S} | Acc];
attribute(?FramedIPv6Pool, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?FramedIPv6Pool, S} | Acc];
attribute(?ErrorCause, Value, Acc) when size(Value) == 4 ->
	Cause = binary:decode_unsigned(Value),
	[{?ErrorCause, Cause} | Acc];
attribute(?EAPKeyName, Data, Acc) when size(Data) >= 1 ->
	[{?EAPKeyName, Data} | Acc];
attribute(?DigestResponse, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestResponse, S} | Acc];
attribute(?DigestRealm, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestRealm, S} | Acc];
attribute(?DigestNonce, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestNonce, S} | Acc];
attribute(?DigestResponseAuth, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestResponseAuth, S} | Acc];
attribute(?DigestNextnonce, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestNextnonce, S} | Acc];
attribute(?DigestMethod, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestMethod, S} | Acc];
attribute(?DigestURI, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestURI, S} | Acc];
attribute(?DigestQop, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestQop, S} | Acc];
attribute(?DigestAlgorithm, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestAlgorithm, S} | Acc];
attribute(?DigestEntityBodyHash, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestEntityBodyHash, S} | Acc];
attribute(?DigestCNonce, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestCNonce, S} | Acc];
attribute(?DigestNonceCount, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestNonceCount, S} | Acc];
attribute(?DigestUsername, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestUsername, S} | Acc];
attribute(?DigestOpaque, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestOpaque, S} | Acc];
attribute(?DigestAuthParam, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestAuthParam, S} | Acc];
attribute(?DigestAKAAuts, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestAKAAuts, S} | Acc];
attribute(?DigestDomain, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestDomain, S} | Acc];
attribute(?DigestStale, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestStale, S} | Acc];
attribute(?DigestHA1, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?DigestHA1, S} | Acc];
attribute(?SIPAOR, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?SIPAOR, S} | Acc];
attribute(?DelegatedIPv6Prefix, <<0, PrefixLength, Prefix/binary>>, Acc)
		when size(Prefix) >= 4, size(Prefix) =< 20 ->
	[{?DelegatedIPv6Prefix, {PrefixLength, Prefix}} | Acc];
attribute(?MIP6FeatureVector, Value, Acc) when size(Value) == 4 ->
	Caps = binary:decode_unsigned(Value),
	[{?MIP6FeatureVector, Caps} | Acc];
attribute(?MIP6HomeLinkPrefix, <<PrefixLength, Prefix:16/binary>>, Acc) ->
	[{?MIP6HomeLinkPrefix, {PrefixLength, Prefix}} | Acc];
attribute(?OperatorName, <<NameSpaceId, Text/binary>>, Acc) ->
	OperatorName = binary_to_list(Text),
	[{?OperatorName, {NameSpaceId, OperatorName}} | Acc];
attribute(?LocationInformation, <<Index:16, Code:8, Entity:8,
		SightingTime:64, TimeToLive:64, Method/binary>>, Acc) when size(Method) >= 1 ->
	S = binary_to_list(Method),
	[{?LocationInformation, {Index, Code, Entity, SightingTime,
			TimeToLive, S}} | Acc];
attribute(?LocationData, <<Index:16, Location/binary>>, Acc)
		when size(Location) >= 1 ->
	[{?LocationData, {Index, Location}} | Acc];
attribute(?BasicLocationPolicyRules, <<Flags:16, RetentionExpires:64/binary,
		NoteWell/binary>>, Acc) ->
	S = binary_to_list(NoteWell),
	[{?BasicLocationPolicyRules, {Flags, RetentionExpires, S}} | Acc];
attribute(?ExtendedLocationPolicyRules, String, Acc) when size(String) >= 1 ->
	RulesReference = binary_to_list(String),
	[{?ExtendedLocationPolicyRules, RulesReference} | Acc];
attribute(?LocationCapable, Value, Acc) when size(Value) == 4 ->
	Caps = binary:decode_unsigned(Value),
	[{?LocationCapable, Caps} | Acc];
attribute(?RequestedLocationInfo, Value, Acc) when size(Value) == 4 ->
	Caps = binary:decode_unsigned(Value),
	[{?RequestedLocationInfo, Caps} | Acc];
attribute(?AllowedCalledStationId, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?AllowedCalledStationId, S} | Acc];
attribute(?EAPPeerId, Data, Acc) when size(Data) >= 1 ->
	[{?EAPPeerId, Data} | Acc];
attribute(?EAPServerId, Data, Acc) when size(Data) >= 1 ->
	[{?EAPServerId, Data} | Acc];
attribute(?MobilityDomainId, <<_:16, MDID:16>>, Acc) ->
	[{?MobilityDomainId, MDID} | Acc];
attribute(?PreauthTimeout, Value, Acc) when size(Value) == 4 ->
	Seconds = binary:decode_unsigned(Value),
	[{?PreauthTimeout, Seconds} | Acc];
attribute(?NetworkIdName, Data, Acc) when size(Data) >= 1 ->
	[{?NetworkIdName, Data} | Acc];
attribute(?EAPoLAnnouncement, Data, Acc) when size(Data) >= 1 ->
	[{?EAPoLAnnouncement, Data} | Acc];
attribute(?WLANHESSID, String, Acc) when size(String) == 17 ->
	S = binary_to_list(String),
	[{?WLANHESSID, S} | Acc];
attribute(?WLANVenueInfo, <<0:16, VenueGroup, VenueType>>, Acc) ->
	[{?WLANVenueInfo, {VenueGroup, VenueType}} | Acc];
attribute(?WLANVenueLanguage, String, Acc)
		when size(String) == 4, size(String) == 5 ->
	Language  = binary_to_list(String),
	[{?WLANVenueLanguage, Language} | Acc];
attribute(?WLANVenueName, String, Acc) when size(String) >= 1 ->
	S = binary_to_list(String),
	[{?WLANVenueName, S} | Acc];
attribute(?WLANReasonCode, Value, Acc) when size(Value) == 4 ->
	Code = binary:decode_unsigned(Value),
	[{?WLANReasonCode, Code} | Acc];
attribute(?WLANPairwiseCipher, <<OUI:24, SuiteType:8>>, Acc) ->
	[{?WLANPairwiseCipher, {OUI, SuiteType}} | Acc];
attribute(?WLANGroupCipher, <<OUI:24, SuiteType:8>>, Acc) ->
	[{?WLANGroupCipher, {OUI, SuiteType}} | Acc];
attribute(?WLANAKMSuite, <<OUI:24, SuiteType:8>>, Acc) ->
	[{?WLANAKMSuite, {OUI, SuiteType}} | Acc];
attribute(?WLANGroupMgmtCipher, <<OUI:24, SuiteType:8>>, Acc) ->
	[{?WLANGroupMgmtCipher, {OUI, SuiteType}} | Acc];
attribute(?WLANRFBand, <<_:24, RfBand:8>>, Acc) ->
	[{?WLANRFBand, RfBand} | Acc];
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
attributes([{?VendorSpecific, {?Cisco,
		{?H323CallOrigin, CallOrigin}}} | T], Acc) when CallOrigin == answer;
		CallOrigin == originate; CallOrigin == callback ->
	CallOriginB = atom_to_binary(CallOrigin, latin1),
	VendorLength = size(CallOriginB) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Cisco:24,
			?H323CallOrigin, VendorLength, CallOriginB/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChapChallenge, MsChapChallenge}}} | T], Acc)
		when is_binary(MsChapChallenge) ->
	VendorLength = size(MsChapChallenge) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsChapChallenge, VendorLength, MsChapChallenge/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChapResponse, {Ident, Flags, LmResponse, NtResponse}}}} | T],
		Acc) when is_integer(Ident), is_integer(Flags),
		(LmResponse) == 24, size(NtResponse) == 24 ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 58, 0, ?Microsoft:24,
			?MsChapResponse, 52, Ident, Flags,
			LmResponse/binary, NtResponse/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChapDomain, {Ident, Domain}}}} | T], Acc)
		when is_integer(Ident), is_list(Domain) ->
	String = list_to_binary(Domain),
	VendorLength = size(String) + 3,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsChapDomain, VendorLength, Ident, String/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChapError, {Ident, Error}}}} | T], Acc)
		when is_integer(Ident), is_list(Error) ->
	String = list_to_binary(Error),
	VendorLength = size(String) + 3,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsChapError, VendorLength, Ident, String/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChapCpw1, {Code, Ident, LmOldPassword, LmNewPassword,
		NtOldPassword, NtNewPassword, NewLmPasswordLength, Flags}}}} | T], Acc)
		when Code == 5, is_integer(Ident),
		size(LmOldPassword) == 16, size(LmNewPassword) == 16,
		size(NtOldPassword) == 16, size(NtNewPassword) == 16, 
		is_integer(NewLmPasswordLength), is_integer(Flags) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 78, 0, ?Microsoft:24,
			?MsChapCpw1, 72, Code, Ident, LmOldPassword/binary,
			LmNewPassword/binary, NtOldPassword/binary,
			NtNewPassword/binary, NewLmPasswordLength:16, Flags:16>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChapCpw2, {Code, Ident, OldNtHash, OldLmHash,
		LmResponse, NtResponse, Flags}}}} | T], Acc) when Code == 6,
		is_integer(Ident), size(OldNtHash) == 16, size(OldLmHash) == 16,
		size(LmResponse) == 24, size(NtResponse) == 24, is_integer(Flags) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 92, 0, ?Microsoft:24,
			?MsChapCpw2, 86, Code, Ident, OldNtHash/binary, OldLmHash/binary,
			LmResponse/binary, NtResponse/binary, Flags:16>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChapLmEncPw, {Code, Ident, SequenceNumber, String}}}} | T],
		Acc) when Code == 6, is_integer(Ident), is_integer(SequenceNumber),
		is_binary(String) ->
	VendorLength = size(String) + 6,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsChapLmEncPw, VendorLength, Code, Ident, SequenceNumber:16,
			String/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChapNtEncPw, {Code, Ident, SequenceNumber, String}}}} | T],
		Acc) when Code == 6, is_integer(Ident), is_integer(SequenceNumber),
		is_binary(String) ->
	VendorLength = size(String) + 6,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsChapNtEncPw, VendorLength, Code, Ident, SequenceNumber:16,
			String/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChap2Response, {Ident, Flags, PeerChallenge,
		Reserved, Response}}}} | T], Acc) when is_integer(Ident),
		Flags == 0, is_integer(PeerChallenge), Reserved == <<0:64>>,
		size(Response) == 24 ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 58, 0, ?Microsoft:24,
			?MsChap2Response, 52, Ident, Flags, PeerChallenge:128,
			Reserved/binary, Response/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChap2Success, {Ident, Authenticator}}}} | T], Acc)
		when is_integer(Ident), size(Authenticator) == 42 ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 51, 0, ?Microsoft:24,
			?MsChap2Success, 45, Ident, Authenticator/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChap2Cpw, {Code, Ident, EncryptedHash, NtResponse,
		Flags}}}} | T], Acc) when Code == 7, is_integer(Ident),
		size(EncryptedHash) == 16, size(NtResponse) == 24, Flags == 0 ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 76, 0, ?Microsoft:24,
			?MsChap2Cpw, 70, Code, Ident, EncryptedHash/binary,
			NtResponse/binary, Flags:16>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsChapMppeKeys, Keys}}} | T], Acc) when is_binary(Keys) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 40, 0, ?Microsoft:24,
			?MsChapMppeKeys, 34, Keys/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsMppeSendKey, {Salt, Key}}}} | T], Acc)
		when (Salt bsr 15) == 1, is_binary(Key) ->
	VendorLength = size(Key) + 4,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsMppeSendKey, VendorLength, Salt:16, Key/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsMppeRecvKey, {Salt, Key}}}} | T], Acc)
		when (Salt bsr 15) == 1, is_binary(Key) ->
	VendorLength = size(Key) + 4,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsMppeRecvKey, VendorLength, Salt:16, Key/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsMppeEncryptionPolicy, allowed}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsMppeEncryptionPolicy, 6, 1:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsMppeEncryptionPolicy, required}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsMppeEncryptionPolicy, 6, 2:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsMppeEncryptionTypes, Types}}} | T], Acc) ->
	L = case lists:member(rc4_40bit, Types) of
		true ->
			1;
		false ->
			0
	end,
	S = case lists:member(rc4_128bit, Types) of
		true ->
			1;
		false ->
			0
	end,
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsMppeEncryptionTypes, 6, 0:29, S:1, L:1, 0:1>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsBapUsage, not_allowed}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsBapUsage, 6, 0:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsBapUsage, allowed}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsBapUsage, 6, 1:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsBapUsage, required}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsBapUsage, 6, 2:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsLinkUtilizationThreshold, Threshold}}} | T], Acc)
		when Threshold > 0, Threshold =< 100 ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsLinkUtilizationThreshold, 6, Threshold:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsLinkDropTimeLimit, Seconds}}} | T], Acc)
		when is_integer(Seconds) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsLinkDropTimeLimit, 6, Seconds:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsOldArapPassword, Password}}} | T], Acc)
		when is_binary(Password) ->
	VendorLength = size(Password) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsOldArapPassword, VendorLength, Password/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsNewArapPassword, Password}}} | T], Acc)
		when is_binary(Password) ->
	VendorLength = size(Password) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsNewArapPassword, VendorLength, Password/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsArapPasswordChangeReason, just_change}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsArapPasswordChangeReason, 6, 1:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsArapPasswordChangeReason, expired}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsArapPasswordChangeReason, 6, 2:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsArapPasswordChangeReason, admin_requires}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsArapPasswordChangeReason, 6, 3:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsArapPasswordChangeReason, too_short}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsArapPasswordChangeReason, 6, 4:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsArapChallenge, Challenge}}} | T], Acc)
		when size(Challenge) == 8 ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 16, 0, ?Microsoft:24,
			?MsNewArapPassword, 10, Challenge/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsRasVendor, VendorID}}} | T], Acc) when is_integer(VendorID)->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsRasVendor, 6, 0:8, VendorID:24>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsRasVersion, Version}}} | T], Acc) when is_binary(Version)->
	VendorLength = size(Version) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsRasVersion, VendorLength, Version/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsFilter, Filter}}} | T], Acc) when is_binary(Filter)->
	VendorLength = size(Filter) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Microsoft:24,
			?MsFilter, VendorLength, Filter/binary>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsAcctAuthType, pap}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsAcctAuthType, 6, 1:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsAcctAuthType, chap}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsAcctAuthType, 6, 2:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsAcctAuthType, mschap1}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsAcctAuthType, 6, 3:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsAcctAuthType, mschap2}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsAcctAuthType, 6, 4:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsAcctAuthType, eap}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsAcctAuthType, 6, 5:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsAcctEapType, md5}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsAcctEapType, 6, 4:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsAcctEapType, otp}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsAcctEapType, 6, 5:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsAcctEapType, gtc}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsAcctEapType, 6, 6:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsAcctEapType, tls}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsAcctEapType, 6, 13:32>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsPrimaryDnsServer, {A, B, C, D}}}} | T], Acc)
		when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsPrimaryDnsServer, 6, A, B, C, D>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsSecondaryDnsServer, {A, B, C, D}}}} | T], Acc)
		when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsSecondaryDnsServer, 6, A, B, C, D>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsPrimaryNbnsServer, {A, B, C, D}}}}| T], Acc)
		when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsPrimaryNbnsServer, 6, A, B, C, D>>);
attributes([{?VendorSpecific, {?Microsoft,
		{?MsSecondaryNbnsServer, {A, B, C, D}}}} | T], Acc)
		when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Microsoft:24,
			?MsSecondaryNbnsServer, 6, A, B, C, D>>);
attributes([{?VendorSpecific, {?Ascend,
		{?AscendClientGateway, {A, B, C, D}}}} | T], Acc)
		when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Ascend:24,
			?AscendClientGateway, 6, A, B, C, D>>);
attributes([{?VendorSpecific, {?Ascend,
		{?AscendDataRate, DataRate}}} | T], Acc)
		when is_integer(DataRate) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Ascend:24,
			?AscendDataRate, 6, DataRate:32>>);
attributes([{?VendorSpecific, {?Ascend,
		{?AscendXmitRate, XmitRate}}} | T], Acc)
		when is_integer(XmitRate) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Ascend:24,
			?AscendXmitRate, 6, XmitRate:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikRecvLimit, Limit}}} | T], Acc) when Limit >= 0 ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikRecvLimit, 6, Limit:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikXmitLimit, Limit}}} | T], Acc) when Limit >= 0 ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikXmitLimit, 6, Limit:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikGroup, Group}}} | T], Acc) when is_list(Group) ->
	Bin = list_to_binary(Group),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikGroup, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessForward, Forward}}} | T], Acc)
		when is_integer(Forward) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikWirelessForward, 6, Forward:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessSkipDot1x, Skip}}} | T], Acc)
		when is_integer(Skip) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikWirelessSkipDot1x, 6, Skip:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessEncAlgo, 'no-encryption'}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikWirelessEncAlgo, 6, 0:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessEncAlgo, '40-bit-wep'}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikWirelessEncAlgo, 6, 1:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessEncAlgo, '104-bit-wep'}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikWirelessEncAlgo, 6, 2:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessEncAlgo, 'aes-ccm'}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikWirelessEncAlgo, 6, 3:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessEncAlgo, tkip}}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikWirelessEncAlgo, 6, 4:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessEncKey, Key}}} | T], Acc) when is_list(Key) ->
	Bin = list_to_binary(Key),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikWirelessEncAlgo, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikRateLimit, Limit}}} | T], Acc) when is_list(Limit) ->
	Bin = list_to_binary(Limit),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikRateLimit, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikRealm, Realm}}} | T], Acc) when is_list(Realm) ->
	Bin = list_to_binary(Realm),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikRealm, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikHostIp, {A, B, C, D}}}} | T], Acc)
		when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikHostIp, 6, A, B, C, D>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikMarkId, ID}}} | T], Acc) when is_list(ID) ->
	Bin = list_to_binary(ID),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikMarkId, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikAdvertiseUrl, URL}}} | T], Acc) when is_list(URL) ->
	Bin = list_to_binary(URL),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikAdvertiseUrl, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikAdvertiseInterval, Interval}}} | T], Acc)
		when is_integer(Interval) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikAdvertiseInterval, 6, Interval:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikRecvLimitGigawords, Gigawords}}} | T], Acc)
		when is_integer(Gigawords) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikRecvLimitGigawords, 6, Gigawords:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikXmitLimitGigawords, Gigawords}}} | T], Acc)
		when is_integer(Gigawords) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikXmitLimitGigawords, 6, Gigawords:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessPsk, PSK}}} | T], Acc) when is_list(PSK) ->
	Bin = list_to_binary(PSK),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikWirelessPsk, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikTotalLimit, Limit}}} | T], Acc) when is_integer(Limit) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikTotalLimit, 6, Limit:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikTotalLimitGigawords, Gigawords}}} | T], Acc)
		when is_integer(Gigawords) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikTotalLimitGigawords, 6, Gigawords:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikAddressList, List}}} | T], Acc) when is_list(List) ->
	Bin = list_to_binary(List),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikAddressList, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessMpKey, Key}}} | T], Acc) when is_list(Key) ->
	Bin = list_to_binary(Key),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikWirelessMpKey, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessComment, Comment}}} | T], Acc) when is_list(Comment) ->
	Bin = list_to_binary(Comment),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikWirelessComment, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikDelegatedIpv6Pool, Pool}}} | T], Acc) when is_list(Pool) ->
	Bin = list_to_binary(Pool),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikDelegatedIpv6Pool, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikDhcpOptionSet, Set}}} | T], Acc) when is_list(Set) ->
	Bin = list_to_binary(Set),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikDhcpOptionSet, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikDhcpOptionParamStr1, Str1}}} | T], Acc) when is_list(Str1) ->
	Bin = list_to_binary(Str1),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikDhcpOptionParamStr1, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikDhcpOptionParamStr2, Str2}}} | T], Acc) when is_list(Str2) ->
	Bin = list_to_binary(Str2),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikDhcpOptionParamStr2, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessVlanId, ID}}} | T], Acc) when is_integer(ID) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikWirelessVlanId, 6, ID:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessVlanIdType, Type}}} | T], Acc) when is_integer(Type) ->
	attributes(T, <<Acc/binary, ?VendorSpecific, 12, 0, ?Mikrotik:24,
			?MikrotikWirelessVlanIdType, 6, Type:32>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessMinSignal, Signal}}} | T], Acc) when is_list(Signal) ->
	Bin = list_to_binary(Signal),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikWirelessMinSignal, VendorLength, Bin/binary>>);
attributes([{?VendorSpecific, {?Mikrotik,
		{?MikrotikWirelessMaxSignal, Signal}}} | T], Acc) when is_list(Signal) ->
	Bin = list_to_binary(Signal),
	VendorLength = size(Bin) + 2,
	Length = VendorLength + 6,
	attributes(T, <<Acc/binary, ?VendorSpecific, Length, 0, ?Mikrotik:24,
			?MikrotikWirelessMaxSignal, VendorLength, Bin/binary>>);
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
attributes([{?EAPMessage, Data} | T], Acc) when size(Data) =< 16#ff ->
	Length = size(Data) + 2,
	attributes(T, <<Acc/binary, ?EAPMessage, Length, Data/binary>>);
attributes([{?MessageAuthenticator, String} | T], Acc) ->
	attributes(T, <<Acc/binary, ?MessageAuthenticator, 18, String/binary>>);
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
	attributes(T, <<Acc/binary, ?OriginatingLineInfo, 4, OLI:16>>);
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
attributes([{?DelegatedIPv6Prefix, {PrefixLength, Prefix}} | T], Acc) ->
	Length = size(Prefix) + 4,
	attributes(T, <<Acc/binary, ?DelegatedIPv6Prefix, Length, 0,
			PrefixLength, Prefix/binary>>);
attributes([{?MIP6FeatureVector, Caps} | T], Acc) ->
	attributes(T, <<Acc/binary, ?MIP6FeatureVector, 6, Caps:32>>);
attributes([{?MIP6HomeLinkPrefix, {PrefixLength, Prefix}} | T], Acc) ->
	attributes(T, <<Acc/binary, ?MIP6HomeLinkPrefix, 19, 0,
			PrefixLength, Prefix/binary>>);
attributes([{?OperatorName, {NameSpaceId, OperatorName}} | T], Acc) ->
	S = list_to_binary(OperatorName),
	Length = size(S) + 3,
	attributes(T, <<Acc/binary, ?OperatorName, Length, NameSpaceId, S/binary>>);
attributes([{?LocationInformation, {Index, Code, Entity, SightingTime,
		TimeToLive, Method}} | T], Acc) ->
	M = list_to_binary(Method),
	Length = size(M) + 22,
	attributes(T, <<Acc/binary, ?LocationInformation, Length, Index:16, Code:8,
			Entity:8, SightingTime:64, TimeToLive:64, M/binary>>);
attributes([{?LocationData, {Index, Location}} | T], Acc) ->
	Length = size(Location) + 4,
	attributes(T, <<Acc/binary, ?LocationData, Length, Index:16, Location/binary>>);
attributes([{?BasicLocationPolicyRules,
		{Flags, RetentionExpires, NoteWell}} | T], Acc) ->
	S = list_to_binary(NoteWell),
	Length = size(S) + 12,
	attributes(T, <<Acc/binary, ?BasicLocationPolicyRules, Length,
			Flags:16, RetentionExpires:64/binary, S/binary>>);
attributes([{?ExtendedLocationPolicyRules, RulesetReference} | T], Acc) ->
	S = list_to_binary(RulesetReference),
	Length = size(S) + 2,
	attributes(T, <<Acc/binary, ?ExtendedLocationPolicyRules, Length, S/binary>>);
attributes([{?LocationCapable, Caps} | T], Acc) ->
	attributes(T, <<Acc/binary, ?LocationCapable, 6, Caps:32>>);
attributes([{?RequestedLocationInfo, Caps} | T], Acc) ->
	attributes(T, <<Acc/binary, ?RequestedLocationInfo, 6, Caps:32>>);
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

