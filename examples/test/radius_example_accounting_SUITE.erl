%%%---------------------------------------------------------------------
%%% @copyright 2016-2017 SigScale Global Inc
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%%
%%% Copyright (c) 2016-2017, SigScale Global Inc
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
%%% @doc Radius authentication callback module tests
%%%--------------------------------------------------------------------
%%%
-module(radius_example_accounting_SUITE).

%% common_test required callbacks
-export([suite/0, init_per_suite/1, end_per_suite/1, sequences/0, all/0]).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("radius.hrl").

-define(TIMEOUT, 2000).

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
init_per_suite(Config) ->
	SharedSecret = "xyzzy5461",
	ok = application:start(radius),
	DbDir = ?config(priv_dir, Config) ++ "/db",
	application:load(mnesia),
	ok = application:set_env(mnesia, dir, DbDir),
	ok = mnesia:create_schema([node()]),
	ok = application:start(mnesia),
	{ok, [radius_client, radius_user]} = radius_example:install([node()]),
	LogDir = ?config(priv_dir, Config) ++ "log",
	ok = file:make_dir(LogDir),
	application:load(radius_example),
	ok = application:set_env(radius_example, accounting_dir, LogDir),
	ok = application:start(radius_example),
	{ok, Port} = application:get_env(radius_example, accounting_port),
	[{secret, SharedSecret}, {port, Port}, {log_dir, LogDir} | Config].

%% @spec (Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after the whole suite.
%%
end_per_suite(_Config) ->
	ok = application:stop(radius_example),
	{atomic, ok} = mnesia:delete_table(radius_user),
	{atomic, ok} = mnesia:delete_table(radius_client),
	ok = application:stop(mnesia),
	ok = mnesia:delete_schema([node()]),
	ok = application:stop(radius).

%% @spec (TestCase, Config) -> Config
%% 	Config = [tuple()]
%% @doc Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet,
			{ip, {127, 0, 0, 1}}, binary]),
   [{socket, Socket} | Config].

%% @spec (TestCase, Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after each test case.
%%
end_per_testcase(_TestCase, Config) ->
	Socket = ?config(socket, Config),
	ok = gen_udp:close(Socket).

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
	[client_unknown, client, accounting_on, start, interim_update, stop,
			accounting_off, log_file].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

client() ->
	[{userdata, [{doc, "Add client to database"}]}].

client(Config) ->
	Address = {127, 0, 0, 1},
	Secret = ?config(secret, Config),
	ok = radius_example:add_client(Address, Secret),
	{ok, Secret} = radius_example:find_client(Address).

client_unknown() ->
	[{userdata, [{doc, "Request from unkown client"}]}].

client_unknown(Config) ->
	Id = 0,
	RequestAuthenticator = radius:authenticator(),
	Attributes0 = radius_attributes:new(),
	Attributes = radius_attributes:codec(Attributes0),
	Request = radius:codec(#radius{code = ?AccessRequest, id = Id,
			authenticator = RequestAuthenticator, attributes = Attributes}),
	Address = {127, 0, 0, 1},
	Socket = ?config(socket, Config),
	Port = ?config(port, Config),
	ok = gen_udp:send(Socket, Address, Port, Request),
	{error, timeout} = gen_udp:recv(Socket, 0, ?TIMEOUT).

accounting_on() ->
	[{userdata, [{doc, "Accounting request; accounting on"}]}].

accounting_on(Config) ->
	Id = 0,
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?NasIpAddress,
			{192, 168, 1, 16}, Attributes0),
	Attributes2 = radius_attributes:store(?AcctStatusType, 7, Attributes1),
	Attributes3 = radius_attributes:store(?AcctSessionId, "00000000", Attributes2),
	send(Id, Attributes3, Config).

start() ->
	[{userdata, [{doc, "Accounting request; user service start"}]}].

start(Config) ->
	Id = 1,
	UserName = "nemo",
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?AcctStatusType, 1, Attributes0),
	Attributes2 = radius_attributes:store(?UserName, UserName, Attributes1),
	Attributes3 = radius_attributes:store(?NasIpAddress,
			{192, 168, 1, 16}, Attributes2),
	Attributes4 = radius_attributes:store(?NasPort, 3, Attributes3),
	Attributes5 = radius_attributes:store(?FramedIpAddress,
			{172, 30, 1, 116}, Attributes4),
	Attributes6 = radius_attributes:store(?AcctSessionId, "00000001", Attributes5),
	Attributes7 = radius_attributes:store(?AcctAuthentic, 1, Attributes6),
	send(Id, Attributes7, Config).

interim_update() ->
	[{userdata, [{doc, "Accounting request; user service interim update"}]}].

interim_update(Config) ->
	Id = 2,
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?AcctStatusType, 3, Attributes0),
	Attributes2 = radius_attributes:store(?NasIpAddress,
			{192, 168, 1, 16}, Attributes1),
	Attributes3 = radius_attributes:store(?AcctSessionId, "00000001", Attributes2),
	Attributes4 = radius_attributes:store(?AcctSessionTime, 3600, Attributes3),
	Attributes5 = radius_attributes:store(?AcctInputOctets, 1231234, Attributes4),
	Attributes6 = radius_attributes:store(?AcctOutputOctets, 1234512345, Attributes5),
	Attributes7 = radius_attributes:store(?AcctInputPackets, 1234, Attributes6),
	Attributes8 = radius_attributes:store(?AcctInputPackets, 16543, Attributes7),
	send(Id, Attributes8, Config).

stop() ->
	[{userdata, [{doc, "Accounting request; user service stop"}]}].

stop(Config) ->
	Id = 3,
	UserName = "nemo",
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?AcctStatusType, 2, Attributes0),
	Attributes2 = radius_attributes:store(?UserName, UserName, Attributes1),
	Attributes3 = radius_attributes:store(?NasIpAddress,
			{192, 168, 1, 16}, Attributes2),
	Attributes4 = radius_attributes:store(?NasPort, 3, Attributes3),
	Attributes5 = radius_attributes:store(?AcctSessionId, "00000001", Attributes4),
	Attributes6 = radius_attributes:store(?AcctSessionTime, 3610, Attributes5),
	Attributes7 = radius_attributes:store(?AcctInputOctets, 1234567, Attributes6),
	Attributes8 = radius_attributes:store(?AcctOutputOctets, 1234567890, Attributes7),
	Attributes9 = radius_attributes:store(?AcctInputPackets, 1678, Attributes8),
	Attributes10 = radius_attributes:store(?AcctInputPackets, 16789, Attributes9),
	Attributes11 = radius_attributes:store(?AcctTerminateCause, 1, Attributes10),
	send(Id, Attributes11, Config).

accounting_off() ->
	[{userdata, [{doc, "Accounting request; accounting stop"}]}].

accounting_off(Config) ->
	Id = 4,
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?AcctStatusType, 7, Attributes0),
	Attributes2 = radius_attributes:store(?AcctSessionId, "00000000", Attributes1),
	Attributes3 = radius_attributes:store(?NasIpAddress,
			{192, 168, 1, 16}, Attributes2),
	send(Id, Attributes3, Config).

log_file() ->
	[{userdata, [{doc, "Write log to file"}]}].

log_file(Config) ->
	Dir = ?config(log_dir, Config),
	FileName = Dir ++ "/radius_acct.txt",
	ok = radius_example:log_file(FileName),
	{ok, _Binary} = file:read_file(FileName).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

send(Id, AttributeList, Config) ->
	SharedSecret = ?config(secret, Config),
	Attributes = radius_attributes:codec(AttributeList),
	RequestLength = size(Attributes) + 20,
	RequestAuthenticator = crypto:hash(md5, [<<?AccountingRequest, Id,
			RequestLength:16, 0:128>>, Attributes, SharedSecret]),
	Request = radius:codec(#radius{code = ?AccountingRequest, id = Id,
			authenticator = RequestAuthenticator, attributes = Attributes}),
	Socket = ?config(socket, Config),
	Address = {127, 0, 0, 1},
	Port = ?config(port, Config),
	ok = gen_udp:send(Socket, Address, Port, Request),
	{ok, {Address, Port, Response}} = gen_udp:recv(Socket, 0, ?TIMEOUT),
	#radius{code = ?AccountingResponse, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = BinaryResponseAttributes} = radius:codec(Response),
	ResponseLength = binary:decode_unsigned(binary:part(Response, 2, 2)),
	Hash = crypto:hash(md5, [<<?AccountingResponse, Id, ResponseLength:16>>,
			RequestAuthenticator, BinaryResponseAttributes, SharedSecret]),
	ResponseAuthenticator = binary_to_list(Hash),
	[] = radius_attributes:codec(BinaryResponseAttributes).

