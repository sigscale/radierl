%%%---------------------------------------------------------------------
%%% @copyright 2016-2018 SigScale Global Inc
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%%
%%% Copyright (c) 2016-2018, SigScale Global Inc
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
%%%    - Neither the name of SigScale Global Inc nor the names of its
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
%%% @doc Radius attributes tests
%%%--------------------------------------------------------------------
%%%
-module(radius_attributes_SUITE).

%% common_test required callbacks
-export([suite/0, init_per_suite/1, end_per_suite/1, sequences/0, all/0]).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("radius.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

%% @spec () -> DefaultData
%% 	DefaultData = [tuple()]
%% @doc Require variables and set default values for the suite.
%%
suite() ->
	[].

%% @spec (Config) -> Config
%% 	Config = [tuple()]
%% @doc Initiation before the whole suite.
init_per_suite(Config) ->
	Attributes = <<?UserName, 7, <<"sally">>/binary,
			?NasPort, 6, 0:32, ?EAPMessage, 18,
			<<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>/binary,
			?EAPMessage, 18,
			<<16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31>>/binary,
			?VendorSpecific, 20, 0, ?Mikrotik:24,
			?MikrotikRealm, 14, <<"sigscale.net">>/binary,
			?NasPortId, 7, <<"wlan0">>/binary>>,
	[{example_attributes, Attributes} | Config].

%% @spec (Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok.

%% @spec () -> Sequences
%% 	Sequences = [{SeqName, Testcases}]
%% 	SeqName = atom()
%% 	Testcases = [atom()]
%% @doc Group test cases into a test sequence.
%%
sequences() ->
	[].

%% @spec () -> TestCases
%% 	TestCases = [Case]
%% 	Case = atom()
%% @doc Returns a list of all test cases in this test suite.
%%
all() ->
	[store_noexist, store_exist,
			store_vendor_noexist, store_vendor_exist,
			add, add_vendor,
			fetch_exist, fetch_noexist,
			fetch_vendor_exist, fetch_vendor_noexist,
			find_exist, find_noexist,
			find_vendor_exist, find_vendor_noexist,
			get_all, password, example1, example2, example3].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

store_noexist() ->
	[{userdata, [{doc, "Store a new attribute value"}]}].

store_noexist(Config) ->
	AttrsBin1 = ?config(example_attributes, Config),
	Attrs1 = radius_attributes:codec(AttrsBin1),
	Attrs2 = radius_attributes:store(?NasPortType, 19, Attrs1),
	AttrsBin2 = radius_attributes:codec(Attrs2),
	SizeBin1 = size(AttrsBin1),
	<<AttrsBin1:SizeBin1/binary, ?NasPortType, 6, 19:32>> = AttrsBin2.

store_exist() ->
	[{userdata, [{doc, "Overwrite an existing attribute value"}]}].

store_exist(Config) ->
	AttrsBin1 = ?config(example_attributes, Config),
	Attrs1 = radius_attributes:codec(AttrsBin1),
	Attrs2 = radius_attributes:store(?NasPort, 17, Attrs1),
	AttrsBin2 = radius_attributes:codec(Attrs2),
	<<_:7/binary, ?NasPort, 6, 17:32, _:63/binary>> = AttrsBin2.

store_vendor_noexist() ->
	[{userdata, [{doc, "Store a new vendor specific attribute value"}]}].

store_vendor_noexist(Config) ->
	AttrsBin1 = ?config(example_attributes, Config),
	Attrs1 = radius_attributes:codec(AttrsBin1),
	DataRate = 256000,
	Attrs2 = radius_attributes:store(?Ascend,
			?AscendDataRate, DataRate, Attrs1),
	AttrsBin2 = radius_attributes:codec(Attrs2),
	SizeBin1 = size(AttrsBin1),
	<<AttrsBin1:SizeBin1/binary, ?VendorSpecific, 12, 0,
			?Ascend:24, ?AscendDataRate, 6, DataRate:32>> = AttrsBin2.

store_vendor_exist() ->
	[{userdata, [{doc, "Overwrite an existing vendor specific attribute value"}]}].

store_vendor_exist(Config) ->
	AttrsBin1 = ?config(example_attributes, Config),
	Attrs1 = radius_attributes:codec(AttrsBin1),
	Realm = "example.com",
	SizeRealm = length(Realm),
	Attrs2 = radius_attributes:store(?Mikrotik, ?MikrotikRealm, Realm, Attrs1),
	AttrsBin2 = radius_attributes:codec(Attrs2),
	<<_:49/binary, ?VendorSpecific, 19, 0, ?Mikrotik:24, ?MikrotikRealm,
			13, Realm1:SizeRealm/binary, _:7/binary>> = AttrsBin2,
	Realm1 = list_to_binary(Realm).

add() ->
	[{userdata, [{doc, "Add an attribute"}]}].

add(Config) ->
	AttrsBin1 = ?config(example_attributes, Config),
	Attrs1 = radius_attributes:codec(AttrsBin1),
	Attrs2 = radius_attributes:add(?PortLimit, 1, Attrs1),
	AttrsBin2 = radius_attributes:codec(Attrs2),
	SizeBin1 = size(AttrsBin1),
	<<AttrsBin1:SizeBin1/binary, ?PortLimit, 6, 1:32>> = AttrsBin2.

add_vendor() ->
	[{userdata, [{doc, "Add a vendor specific attribute"}]}].

add_vendor(Config) ->
	AttrsBin1 = ?config(example_attributes, Config),
	Vendor = ?Mikrotik,
	Attribute = ?MikrotikRealm,
	Value = "example.net",
	Attrs1 = radius_attributes:codec(AttrsBin1),
	Attrs2 = radius_attributes:add(Vendor, Attribute, Value, Attrs1),
	AttrsBin2 = radius_attributes:codec(Attrs2),
	SizeBin1 = size(AttrsBin1),
	<<AttrsBin1:SizeBin1/binary, ?VendorSpecific, L1, 0, Vendor1:24,
			Attribute1, L2, Value1/binary>> = AttrsBin2,
	Vendor1 = Vendor,
	Attribute1 = Attribute,
	L2 = length(Value) + 2,
	L1 = L2 + 6,
	Value1 = list_to_binary(Value).

fetch_exist() ->
	[{userdata, [{doc, "Get the value of an existing attribute"}]}].

fetch_exist(Config) ->
	AttrsBin = ?config(example_attributes, Config),
	Attrs = radius_attributes:codec(AttrsBin),
	"wlan0" = radius_attributes:fetch(?NasPortId, Attrs).

fetch_noexist() ->
	[{userdata, [{doc, "Attempt to get a nonexistent attribute"}]}].

fetch_noexist(Config) ->
	AttrsBin = ?config(example_attributes, Config),
	Attrs = radius_attributes:codec(AttrsBin),
	case catch radius_attributes:fetch(?State, Attrs) of
		{'EXIT', not_found} ->
			ok
	end.

fetch_vendor_exist() ->
	[{userdata, [{doc, "Get the value of an existing vendor specific attribute"}]}].

fetch_vendor_exist(Config) ->
	AttrsBin = ?config(example_attributes, Config),
	Attrs = radius_attributes:codec(AttrsBin),
	"sigscale.net" = radius_attributes:fetch(?Mikrotik, ?MikrotikRealm, Attrs).

fetch_vendor_noexist() ->
	[{userdata, [{doc, "Attempt to get a nonexistent vendor specific attribute"}]}].

fetch_vendor_noexist(Config) ->
	AttrsBin = ?config(example_attributes, Config),
	Attrs = radius_attributes:codec(AttrsBin),
	case catch radius_attributes:fetch(?Microsoft, ?MsChapError, Attrs) of
		{'EXIT', not_found} ->
			ok
	end.

find_exist() ->
	[{userdata, [{doc, "Look for the value of an existing attribute"}]}].

find_exist(Config) ->
	AttrsBin = ?config(example_attributes, Config),
	Attrs = radius_attributes:codec(AttrsBin),
	{ok, "wlan0"} = radius_attributes:find(?NasPortId, Attrs).

find_noexist() ->
	[{userdata, [{doc, "Look for the value of a nonexistent attribute"}]}].

find_noexist(Config) ->
	AttrsBin = ?config(example_attributes, Config),
	Attrs = radius_attributes:codec(AttrsBin),
	{error, not_found} = radius_attributes:find(?State, Attrs).

find_vendor_exist() ->
	[{userdata, [{doc, "Look for the value of an existing vendor specific attribute"}]}].

find_vendor_exist(Config) ->
	AttrsBin = ?config(example_attributes, Config),
	Attrs = radius_attributes:codec(AttrsBin),
	{ok, "sigscale.net"} = radius_attributes:find(?Mikrotik, ?MikrotikRealm, Attrs).

find_vendor_noexist() ->
	[{userdata, [{doc, "Look for the value of a nonexistent vendor specific attribute"}]}].

find_vendor_noexist(Config) ->
	AttrsBin = ?config(example_attributes, Config),
	Attrs = radius_attributes:codec(AttrsBin),
	{error, not_found} = radius_attributes:find(?Microsoft, ?MsChapError, Attrs).

get_all() ->
	[{userdata, [{doc, "Get all values of an attribute"}]}].

get_all(Config) ->
	AttrsBin = ?config(example_attributes, Config),
	Attrs = radius_attributes:codec(AttrsBin),
	EAPMessages = radius_attributes:get_all(?EAPMessage, Attrs),
	[<<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,
			<<16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31>>] = EAPMessages.

password() ->
	[{userdata, [{doc, "User-Password hiding"}]}].

password(_Config) ->
	Authenticator = [rand:uniform(255) || _ <- lists:seq(1, 16)],
	Secret = [rand:uniform(95) + 31 || _ <- lists:seq(1, 16)],
	ShortSecret = [rand:uniform(95) + 31 || _ <- lists:seq(1, 3)],
	LongSecret = [rand:uniform(95) + 31 || _ <- lists:seq(1, 128)],
	Password = [rand:uniform(95) + 31 || _ <- lists:seq(1, 16)],
	ShortPassword = [rand:uniform(95) + 31 || _ <- lists:seq(1, 3)],
	MediumPassword = [rand:uniform(95) + 31 || _ <- lists:seq(1, 19)],
	LongPassword = [rand:uniform(95) + 31 || _ <- lists:seq(1, 128)],
	UserPassword1 = radius_attributes:hide(Secret,
			Authenticator, Password),
	Password = radius_attributes:unhide(Secret,
			Authenticator, UserPassword1),
	UserPassword2 = radius_attributes:hide(ShortSecret,
			Authenticator, Password),
	Password = radius_attributes:unhide(ShortSecret,
			Authenticator, UserPassword2),
	UserPassword3 = radius_attributes:hide(LongSecret,
			Authenticator, Password),
	Password = radius_attributes:unhide(LongSecret,
			Authenticator, UserPassword3),
	UserPassword4 = radius_attributes:hide(Secret,
			Authenticator, ShortPassword),
	ShortPassword = radius_attributes:unhide(Secret,
			Authenticator, UserPassword4),
	UserPassword5 = radius_attributes:hide(Secret,
			Authenticator, MediumPassword),
	MediumPassword = radius_attributes:unhide(Secret,
			Authenticator, UserPassword5),
	UserPassword6 = radius_attributes:hide(Secret,
			Authenticator, LongPassword),
	LongPassword = radius_attributes:unhide(Secret,
			Authenticator, UserPassword6).

example1() ->
	[{userdata, [{doc, "RFC2865 7.1 User Telnet to Specified Host"}]}].

example1(_Config) ->
	SharedSecret = "xyzzy5461",
	Request = <<16#01, 16#00, 16#00, 16#38, 16#0f, 16#40, 16#3f,
			16#94, 16#73, 16#97, 16#80, 16#57, 16#bd, 16#83, 16#d5,
			16#cb, 16#98, 16#f4, 16#22, 16#7a, 16#01, 16#06, 16#6e,
			16#65, 16#6d, 16#6f, 16#02, 16#12, 16#0d, 16#be, 16#70,
			16#8d, 16#93, 16#d4, 16#13, 16#ce, 16#31, 16#96, 16#e4,
			16#3f, 16#78, 16#2a, 16#0a, 16#ee, 16#04, 16#06, 16#c0,
			16#a8, 16#01, 16#10, 16#05, 16#06, 16#00, 16#00, 16#00,
			16#03>>,
	#radius{code = ?AccessRequest, id = Id,
			authenticator = RequestAuthenticator,
			attributes = BinaryRequestAttributes} = radius:codec(Request),
	RequestAttributes = radius_attributes:codec(BinaryRequestAttributes),
	"nemo" = radius_attributes:fetch(?UserName, RequestAttributes),
	UserPassword = radius_attributes:fetch(?UserPassword, RequestAttributes),
	"arctangent" = radius_attributes:unhide(SharedSecret,
			RequestAuthenticator, UserPassword),
	{192, 168, 1, 16} = radius_attributes:fetch(?NasIpAddress,
			RequestAttributes),
	3 = radius_attributes:fetch(?NasPort, RequestAttributes),
	Response = <<16#02, 16#00, 16#00, 16#26, 16#86, 16#fe, 16#22,
			16#0e, 16#76, 16#24, 16#ba, 16#2a, 16#10, 16#05, 16#f6,
			16#bf, 16#9b, 16#55, 16#e0, 16#b2, 16#06, 16#06, 16#00,
			16#00, 16#00, 16#01, 16#0f, 16#06, 16#00, 16#00, 16#00,
			16#00, 16#0e, 16#06, 16#c0, 16#a8, 16#01, 16#03>>,
	#radius{code = ?AccessAccept, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = BinaryResponseAttributes} = radius:codec(Response),
	Hash = crypto:hash(md5, [<<2, Id, 38:16>>, RequestAuthenticator,
			BinaryResponseAttributes, SharedSecret]),
	ResponseAuthenticator = binary_to_list(Hash),
	ResponseAttributes = radius_attributes:codec(BinaryResponseAttributes),
	1 = radius_attributes:fetch(?ServiceType, ResponseAttributes),
	0 = radius_attributes:fetch(?LoginService, ResponseAttributes),
	{192, 168, 1, 3} = radius_attributes:fetch(?LoginIpHost,
			ResponseAttributes).

example2() ->
	[{userdata, [{doc, "RFC2865 7.2 Framed User Authenticating with CHAP"}]}].

example2(_Config) ->
	SharedSecret = "xyzzy5461",
	Request = <<16#01, 16#01, 16#00, 16#47, 16#2a, 16#ee, 16#86,
			16#f0, 16#8d, 16#0d, 16#55, 16#96, 16#9c, 16#a5, 16#97,
			16#8e, 16#0d, 16#33, 16#67, 16#a2, 16#01, 16#08, 16#66,
			16#6c, 16#6f, 16#70, 16#73, 16#79, 16#03, 16#13, 16#16,
			16#e9, 16#75, 16#57, 16#c3, 16#16, 16#18, 16#58, 16#95,
			16#f2, 16#93, 16#ff, 16#63, 16#44, 16#07, 16#72, 16#75,
			16#04, 16#06, 16#c0, 16#a8, 16#01, 16#10, 16#05, 16#06,
			16#00, 16#00, 16#00, 16#14, 16#06, 16#06, 16#00, 16#00,
			16#00, 16#02, 16#07, 16#06, 16#00, 16#00, 16#00, 16#01>>,
	#radius{code = ?AccessRequest, id = Id,
			authenticator = RequestAuthenticator,
			attributes = BinaryRequestAttributes} = radius:codec(Request),
	RequestAttributes = radius_attributes:codec(BinaryRequestAttributes),
	"flopsy" = radius_attributes:fetch(?UserName, RequestAttributes),
	_ChapPassword = radius_attributes:fetch(?ChapPassword, RequestAttributes),
	{192, 168, 1, 16} = radius_attributes:fetch(?NasIpAddress,
			RequestAttributes),
	20 = radius_attributes:fetch(?NasPort, RequestAttributes),
	2 = radius_attributes:fetch(?ServiceType, RequestAttributes),
	1 = radius_attributes:fetch(?FramedProtocol, RequestAttributes),
	Response = <<16#02, 16#01, 16#00, 16#38, 16#15, 16#ef, 16#bc,
			16#7d, 16#ab, 16#26, 16#cf, 16#a3, 16#dc, 16#34, 16#d9,
			16#c0, 16#3c, 16#86, 16#01, 16#a4, 16#06, 16#06, 16#00,
			16#00, 16#00, 16#02, 16#07, 16#06, 16#00, 16#00, 16#00,
			16#01, 16#08, 16#06, 16#ff, 16#ff, 16#ff, 16#fe, 16#0a,
			16#06, 16#00, 16#00, 16#00, 16#02, 16#0d, 16#06, 16#00,
			16#00, 16#00, 16#01, 16#0c, 16#06, 16#00, 16#00, 16#05,
			16#dc>>,
	#radius{code = ?AccessAccept, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = BinaryResponseAttributes} = radius:codec(Response),
	Hash = crypto:hash(md5, [<<2, Id, 56:16>>, RequestAuthenticator,
			BinaryResponseAttributes, SharedSecret]),
	ResponseAuthenticator = binary_to_list(Hash),
	ResponseAttributes = radius_attributes:codec(BinaryResponseAttributes),
	2 = radius_attributes:fetch(?ServiceType, ResponseAttributes),
	1 = radius_attributes:fetch(?FramedProtocol, ResponseAttributes),
	{255, 255, 255, 254} = radius_attributes:fetch(?FramedIpAddress,
			ResponseAttributes),
	% The Service-Type in the RFC doesn't match the hex dump (Errata ID: 1469)
	2 = radius_attributes:fetch(?FramedRouting, ResponseAttributes),
	1 = radius_attributes:fetch(?FramedCompression, ResponseAttributes),
	1500 = radius_attributes:fetch(?FramedMtu, ResponseAttributes).

example3() ->
	[{userdata, [{doc, "RFC2865 7.3 User with Challenge-Response card"}]}].

example3(_Config) ->
	SharedSecret = "xyzzy5461",
	Request1 = <<16#01, 16#02, 16#00, 16#39, 16#f3, 16#a4, 16#7a,
			16#1f, 16#6a, 16#6d, 16#76, 16#71, 16#0b, 16#94, 16#7a,
			16#b9, 16#30, 16#41, 16#a0, 16#39, 16#01, 16#07, 16#6d,
			16#6f, 16#70, 16#73, 16#79, 16#02, 16#12, 16#33, 16#65,
			16#75, 16#73, 16#77, 16#82, 16#89, 16#b5, 16#70, 16#88,
			16#5e, 16#15, 16#08, 16#48, 16#25, 16#c5, 16#04, 16#06,
			16#c0, 16#a8, 16#01, 16#10, 16#05, 16#06, 16#00, 16#00,
			16#00, 16#07>>,
	#radius{code = ?AccessRequest, id = Id1,
			authenticator = Request1Authenticator,
			attributes = BinaryRequest1Attributes} = radius:codec(Request1),
	Request1Attributes = radius_attributes:codec(BinaryRequest1Attributes),
	"mopsy" = radius_attributes:fetch(?UserName, Request1Attributes),
	UserPassword1  = radius_attributes:fetch(?UserPassword, Request1Attributes),
	"challenge" = radius_attributes:unhide(SharedSecret, Request1Authenticator,
			UserPassword1),
	{192, 168, 1, 16} = radius_attributes:fetch(?NasIpAddress,
			Request1Attributes),
	7 = radius_attributes:fetch(?NasPort, Request1Attributes),
	Response1 = <<16#0b, 16#02, 16#00, 16#4e, 16#36, 16#f3, 16#c8,
			16#76, 16#4a, 16#e8, 16#c7, 16#11, 16#57, 16#40, 16#3c,
			16#0c, 16#71, 16#ff, 16#9c, 16#45, 16#12, 16#30, 16#43,
			16#68, 16#61, 16#6c, 16#6c, 16#65, 16#6e, 16#67, 16#65,
			16#20, 16#33, 16#32, 16#37, 16#36, 16#39, 16#34, 16#33,
			16#30, 16#2e, 16#20, 16#20, 16#45, 16#6e, 16#74, 16#65,
			16#72, 16#20, 16#72, 16#65, 16#73, 16#70, 16#6f, 16#6e,
			16#73, 16#65, 16#20, 16#61, 16#74, 16#20, 16#70, 16#72,
			16#6f, 16#6d, 16#70, 16#74, 16#2e, 16#18, 16#0a, 16#33,
			16#32, 16#37, 16#36, 16#39, 16#34, 16#33, 16#30>>,
	#radius{code = ?AccessChallenge, id = Id1,
			authenticator = Response1Authenticator,
			attributes = BinaryResponse1Attributes} = radius:codec(Response1),
	Hash1 = crypto:hash(md5, [<<11, Id1, 78:16>>, Request1Authenticator,
			BinaryResponse1Attributes, SharedSecret]),
	Response1Authenticator = binary_to_list(Hash1),
	Response1Attributes = radius_attributes:codec(BinaryResponse1Attributes),
	ReplyMessage = "Challenge 32769430.  Enter response at prompt.",
	ReplyMessage = radius_attributes:fetch(?ReplyMessage, Response1Attributes),
	"32769430" = radius_attributes:fetch(?State, Response1Attributes),
	% The hex dump in the RFC has an incorrect length in State (Errata ID: 1469)
	Request2 = <<16#01, 16#03, 16#00, 16#43, 16#b1, 16#22, 16#55,
			16#6d, 16#42, 16#8a, 16#13, 16#d0, 16#d6, 16#25, 16#38,
			16#07, 16#c4, 16#57, 16#ec, 16#f0, 16#01, 16#07, 16#6d,
			16#6f, 16#70, 16#73, 16#79, 16#02, 16#12, 16#69, 16#2c,
			16#1f, 16#20, 16#5f, 16#c0, 16#81, 16#b9, 16#19, 16#b9,
			16#51, 16#95, 16#f5, 16#61, 16#a5, 16#81, 16#04, 16#06,
			16#c0, 16#a8, 16#01, 16#10, 16#05, 16#06, 16#00, 16#00,
			16#00, 16#07, 16#18, 16#0a, 16#33, 16#32, 16#37, 16#36,
			16#39, 16#34, 16#33, 16#30>>,
	#radius{code = ?AccessRequest, id = Id2,
			authenticator = Request2Authenticator,
			attributes = BinaryRequest2Attributes} = radius:codec(Request2),
	Request2Attributes = radius_attributes:codec(BinaryRequest2Attributes),
	"mopsy" = radius_attributes:fetch(?UserName, Request2Attributes),
	UserPassword2 = radius_attributes:fetch(?UserPassword, Request2Attributes),
	"99101462" = radius_attributes:unhide(SharedSecret, Request2Authenticator,
			UserPassword2),
	{192, 168, 1, 16} = radius_attributes:fetch(?NasIpAddress,
			Request2Attributes),
	7 = radius_attributes:fetch(?NasPort, Request2Attributes),
	Response2 = <<16#03, 16#03, 16#00, 16#14, 16#a4, 16#2f, 16#4f,
			16#ca, 16#45, 16#91, 16#6c, 16#4e, 16#09, 16#c8, 16#34,
			16#0f, 16#9e, 16#74, 16#6a, 16#a0>>,
	#radius{code = ?AccessReject, id = Id2,
			authenticator = Response2Authenticator,
			attributes = BinaryResponse2Attributes} = radius:codec(Response2),
	Hash2 = crypto:hash(md5, [<<3, Id2, 20:16>>, Request2Authenticator,
			BinaryResponse2Attributes, SharedSecret]),
	Response2Authenticator = binary_to_list(Hash2),
	[] = radius_attributes:codec(BinaryResponse2Attributes).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

