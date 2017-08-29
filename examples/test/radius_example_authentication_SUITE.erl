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
-module(radius_example_authentication_SUITE).

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
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?ServiceType, 2, Attributes0),
	Attributes2 = radius_attributes:store(?FramedProtocol, 1, Attributes1),
	Attributes3 = radius_attributes:store(?FramedIpNetmask,
			{255, 255, 255, 0}, Attributes2),
	User1Attributes = radius_attributes:store(?FramedIpAddress,
			{192, 168, 1, 15}, Attributes3),
	User2Attributes = radius_attributes:store(?FramedIpNetmask,
			{192, 168, 1, 16}, Attributes3),
	ok = application:start(radius),
	DbDir = ?config(priv_dir, Config) ++ "/db",
	application:load(mnesia),
	ok = application:set_env(mnesia, dir, DbDir),
	ok = mnesia:create_schema([node()]),
	ok = application:start(mnesia),
	{ok, [radius_client, radius_user]} = radius_example:install([node()]),
	ok = application:start(radius_example),
	{ok, Port} = application:get_env(radius_example, authentication_port),
	[{secret, SharedSecret}, {user1_attributes, User1Attributes},
			{user2_attributes, User2Attributes}, {port, Port} | Config].

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
	{ok, Socket} = gen_udp:open(0, [{active, false}, inet, binary]),
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
	[client_unknown, client, user, user_unknown, wrong_password,
			request_simple, request_chap].

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

user() ->
	[{userdata, [{doc, "Add user to database"}]}].

user(Config) ->
	User1Attributes = ?config(user1_attributes, Config),
	User2Attributes = ?config(user2_attributes, Config),
	ok = radius_example:add_user("nemo", "arctangent", User1Attributes),
	ok = radius_example:add_user("flopsy", "arcsine", User2Attributes),
	{ok, "arctangent", User1Attributes} = radius_example:find_user("nemo"),
	{ok, "arcsine", User2Attributes} = radius_example:find_user("flopsy").

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

user_unknown() ->
	[{userdata, [{doc, "Access request with unknown username"}]}].

user_unknown(Config) ->
	Id = 1,
	UserName = "bogus",
	Password = "ignored",
	SharedSecret = ?config(secret, Config),
	RequestAuthenticator = radius:authenticator(),
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?UserName, UserName, Attributes0),
	UserPassword = radius_attributes:hide(SharedSecret,
			RequestAuthenticator, Password),
	Attributes2 = radius_attributes:store(?UserPassword,
			UserPassword, Attributes1),
	Attributes3 = radius_attributes:store(?NasIpAddress,
			{192, 168, 1, 14}, Attributes2),
	Attributes4 = radius_attributes:store(?NasPort, 2, Attributes3),
	Attributes = radius_attributes:codec(Attributes4),
	Request = radius:codec(#radius{code = ?AccessRequest, id = Id,
			authenticator = RequestAuthenticator, attributes = Attributes}),
	Socket = ?config(socket, Config),
	Address = {127, 0, 0, 1},
	Port = ?config(port, Config),
	ok = gen_udp:send(Socket, Address, Port, Request),
	{ok, {Address, Port, Response}} = gen_udp:recv(Socket, 0, ?TIMEOUT),
	#radius{code = ?AccessReject, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = BinaryResponseAttributes} = radius:codec(Response),
	Length = binary:decode_unsigned(binary:part(Response, 2, 2)),
	Hash = crypto:hash(md5, [<<?AccessReject, Id, Length:16>>, RequestAuthenticator,
			BinaryResponseAttributes, SharedSecret]),
	ResponseAuthenticator = binary_to_list(Hash),
	[] = radius_attributes:codec(BinaryResponseAttributes).

wrong_password() ->
	[{userdata, [{doc, "Access request with wrong password"}]}].

wrong_password(Config) ->
	Id = 2,
	UserName = "nemo",
	Password = "bogus",
	SharedSecret = ?config(secret, Config),
	RequestAuthenticator = radius:authenticator(),
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?UserName, UserName, Attributes0),
	UserPassword = radius_attributes:hide(SharedSecret,
			RequestAuthenticator, Password),
	Attributes2 = radius_attributes:store(?UserPassword,
			UserPassword, Attributes1),
	Attributes3 = radius_attributes:store(?NasIpAddress,
			{192, 168, 1, 15}, Attributes2),
	Attributes4 = radius_attributes:store(?NasPort, 2, Attributes3),
	Attributes = radius_attributes:codec(Attributes4),
	Request = radius:codec(#radius{code = ?AccessRequest, id = Id,
			authenticator = RequestAuthenticator, attributes = Attributes}),
	Socket = ?config(socket, Config),
	Address = {127, 0, 0, 1},
	Port = ?config(port, Config),
	ok = gen_udp:send(Socket, Address, Port, Request),
	{ok, {Address, Port, Response}} = gen_udp:recv(Socket, 0, ?TIMEOUT),
	#radius{code = ?AccessReject, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = BinaryResponseAttributes} = radius:codec(Response),
	Length = binary:decode_unsigned(binary:part(Response, 2, 2)),
	Hash = crypto:hash(md5, [<<?AccessReject, Id, Length:16>>, RequestAuthenticator,
			BinaryResponseAttributes, SharedSecret]),
	ResponseAuthenticator = binary_to_list(Hash),
	[] = radius_attributes:codec(BinaryResponseAttributes).

request_simple() ->
	[{userdata, [{doc, "Simple access request with name and password"}]}].

request_simple(Config) ->
	Id = 3,
	UserName = "nemo",
	Password = "arctangent",
	SharedSecret = ?config(secret, Config),
	RequestAuthenticator = radius:authenticator(),
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?UserName, UserName, Attributes0),
	UserPassword = radius_attributes:hide(SharedSecret,
			RequestAuthenticator, Password),
	Attributes2 = radius_attributes:store(?UserPassword,
			UserPassword, Attributes1),
	Attributes3 = radius_attributes:store(?NasIpAddress,
			{192, 168, 1, 16}, Attributes2),
	Attributes4 = radius_attributes:store(?NasPort, 3, Attributes3),
	Attributes = radius_attributes:codec(Attributes4),
	Request = radius:codec(#radius{code = ?AccessRequest, id = Id,
			authenticator = RequestAuthenticator, attributes = Attributes}),
	Socket = ?config(socket, Config),
	Address = {127, 0, 0, 1},
	Port = ?config(port, Config),
	ok = gen_udp:send(Socket, Address, Port, Request),
	{ok, {Address, Port, Response}} = gen_udp:recv(Socket, 0, ?TIMEOUT),
	#radius{code = ?AccessAccept, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = BinaryResponseAttributes} = radius:codec(Response),
	Length = binary:decode_unsigned(binary:part(Response, 2, 2)),
	Hash = crypto:hash(md5, [<<?AccessAccept, Id, Length:16>>, RequestAuthenticator,
			BinaryResponseAttributes, SharedSecret]),
	ResponseAuthenticator = binary_to_list(Hash),
	User1Attributes = ?config(user1_attributes, Config),
	User1Attributes = radius_attributes:codec(BinaryResponseAttributes).

request_chap() ->
	[{userdata, [{doc, "CHAP access request"}]}].

request_chap(Config) ->
	Id = 4,
	UserName = "flopsy",
	Password = "arcsine",
	SharedSecret = ?config(secret, Config),
	RequestAuthenticator = radius:authenticator(),
	Attributes0 = radius_attributes:new(),
	Attributes1 = radius_attributes:store(?UserName, UserName, Attributes0),
	ChapId = 0,
	ChapPassword = crypto:hash(md5, [ChapId, Password, RequestAuthenticator]),
	Attributes2 = radius_attributes:store(?ChapPassword,
			{ChapId, ChapPassword}, Attributes1),
	Attributes3 = radius_attributes:store(?NasIpAddress,
			{192, 168, 1, 16}, Attributes2),
	Attributes4 = radius_attributes:store(?NasPort, 20, Attributes3),
	Attributes5 = radius_attributes:store(?ServiceType, 2, Attributes4),
	Attributes6 = radius_attributes:store(?FramedProtocol, 1, Attributes5),
	Attributes = radius_attributes:codec(Attributes6),
	Request = radius:codec(#radius{code = ?AccessRequest, id = Id,
			authenticator = RequestAuthenticator, attributes = Attributes}),
	Socket = ?config(socket, Config),
	Address = {127, 0, 0, 1},
	Port = ?config(port, Config),
	ok = gen_udp:send(Socket, Address, Port, Request),
	{ok, {Address, Port, Response}} = gen_udp:recv(Socket, 0, ?TIMEOUT),
	#radius{code = ?AccessAccept, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = BinaryResponseAttributes} = radius:codec(Response),
	Length = binary:decode_unsigned(binary:part(Response, 2, 2)),
	Hash = crypto:hash(md5, [<<?AccessAccept, Id, Length:16>>, RequestAuthenticator,
			BinaryResponseAttributes, SharedSecret]),
	ResponseAuthenticator = binary_to_list(Hash),
	User2Attributes = ?config(user2_attributes, Config),
	User2Attributes = radius_attributes:codec(BinaryResponseAttributes).
	
%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

