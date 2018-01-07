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
%%% @doc Radius API tests
%%%--------------------------------------------------------------------
%%%
-module(radius_api_SUITE).

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

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
	[{timetrap, {minutes, 1}}].

%% @spec (Config) -> Config
%% 	Config = [tuple()]
%% @doc Initiation before the whole suite.
%%
init_per_suite(Config) ->
	SharedSecret = "xyzzy5461",
	NewConfig = [{secret, SharedSecret} | Config],
	radius_test_lib:init_per_suite(NewConfig).

%% @spec (Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	radius_test_lib:end_per_suite(Config).

%% @spec (TestCase, Config) -> Config
%% 	Config = [tuple()]
%% @doc Initiation before each test case.
%%
init_per_testcase(start_and_stop, Config) ->
	Config;
init_per_testcase(_TestCase, Config) ->
	Sup = start(),
	Port = radius:port(Sup),
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet,
			{ip, {127, 0, 0, 1}}, binary]),
	[{sup, Sup}, {port, Port}, {socket, Socket} | Config].

%% @spec (TestCase, Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after each test case.
%%
end_per_testcase(start_and_stop, _Config) ->
	ok;
end_per_testcase(_TestCase, Config) ->
	Socket = ?config(socket, Config),
	gen_udp:close(Socket),
	Sup = ?config(sup, Config),
	stop(Sup).

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
	[start_and_stop, access_request, wait_response, lost_response].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

start_and_stop() ->
	[{userdata, [{doc, "Start and stop a RADIUS server"}]}].

start_and_stop(_Config) ->
	Sup = start(),
	stop(Sup).

access_request() ->
	[{userdata, [{doc, "Send a RADIUS access request"}]}].

access_request(Config) ->
	Secret = ?config(secret, Config),
	User = "nemo",
	Password = "arctangent",
	Authenticator = radius:authenticator(),
	UserPassword = radius_attributes:hide(Secret, Authenticator, Password), 
	AttributeList0 = radius_attributes:new(),
	AttributeList1 = radius_attributes:store(?UserName, User, AttributeList0),
	AttributeList2 = radius_attributes:store(?UserPassword, UserPassword,
			AttributeList1),
	Id = 0,
	Request = #radius{code = ?AccessRequest,
			id = Id,
			authenticator = Authenticator,
			attributes = AttributeList2},
	RequestPacket = radius:codec(Request),
	Port = ?config(port, Config),
	Address = {127, 0, 0, 1},
	Socket = ?config(socket, Config),
	ok = gen_udp:send(Socket, Address, Port, RequestPacket),
	{ok, {Address, Port, ResponsePacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = Id} = radius:codec(ResponsePacket).

wait_response() ->
	[{userdata, [{doc, "Callback sends delayed response"}]}].

wait_response(Config) ->
	Secret = ?config(secret, Config),
	User = "walter",
	Password = "white",
	Authenticator = radius:authenticator(),
	UserPassword = radius_attributes:hide(Secret, Authenticator, Password), 
	AttributeList0 = radius_attributes:new(),
	AttributeList1 = radius_attributes:store(?UserName, User, AttributeList0),
	AttributeList2 = radius_attributes:store(?UserPassword, UserPassword,
			AttributeList1),
	Id = 1,
	Request = #radius{code = ?AccessRequest,
			id = Id,
			authenticator = Authenticator,
			attributes = AttributeList2},
	RequestPacket = radius:codec(Request),
	Port = ?config(port, Config),
	Address = {127, 0, 0, 1},
	Socket = ?config(socket, Config),
	ok = gen_udp:send(Socket, Address, Port, RequestPacket),
	{ok, {Address, Port, ResponsePacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = Id} = radius:codec(ResponsePacket).

lost_response() ->
	[{userdata, [{doc, "Resend a RADIUS access request"}]}].

lost_response(Config) ->
	Secret = ?config(secret, Config),
	User = "nemo",
	Password = "arctangent",
	Authenticator = radius:authenticator(),
	UserPassword = radius_attributes:hide(Secret, Authenticator, Password), 
	AttributeList0 = radius_attributes:new(),
	AttributeList1 = radius_attributes:store(?UserName, User, AttributeList0),
	AttributeList2 = radius_attributes:store(?UserPassword, UserPassword,
			AttributeList1),
	Id = 2,
	Request = #radius{code = ?AccessRequest,
			id = Id,
			authenticator = Authenticator,
			attributes = AttributeList2},
	RequestPacket = radius:codec(Request),
	Port = ?config(port, Config),
	Address = {127, 0, 0, 1},
	Socket = ?config(socket, Config),
	ok = gen_udp:send(Socket, Address, Port, RequestPacket),
	{ok, {Address, Port, ResponsePacket}} = gen_udp:recv(Socket, 0),
	ok = gen_udp:send(Socket, Address, Port, RequestPacket),
	{ok, {Address, Port, ResponsePacket}} = gen_udp:recv(Socket, 0),
	ok = gen_udp:send(Socket, Address, Port, RequestPacket),
	{ok, {Address, Port, ResponsePacket}} = gen_udp:recv(Socket, 0),
	#radius{code = ?AccessAccept, id = Id} = radius:codec(ResponsePacket).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

start() ->
	{ok, Sup} = radius:start(radius_test_callback, 0),
	Sup.

stop(Sup) ->
	ok = radius:stop(Sup).

