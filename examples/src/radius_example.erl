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
%%% @doc This library module implements the user API to the
%%% 		{@link //radius_example. radius_example} application.
%%%
-module(radius_example).
-copyright('Copyright (c) 2016-2017 SigScale Global Inc').
-author('vances@sigscale.org').

%% export the radius_example public API
-export([add_client/2, find_client/1]).
-export([add_user/3, find_user/1]).
-export([log_file/1]).

%% export the radius_example private API
-export([install/1]).

%% define client table entries record
-record(radius_client, {address, secret}).

%% define user table entries record
-record(radius_user, {name, password, attributes}).

-define(WAITFORSCHEMA, 10000).
-define(WAITFORTABLES, 10000).

-define(LOGNAME, radius_acct).

%%----------------------------------------------------------------------
%%  The radius_example public API
%%----------------------------------------------------------------------

-spec add_client(Address :: inet:ip_address(), Secret :: string()) ->
	Result :: ok | {error, Reason :: term()}.
%% @doc Store the shared secret for a client.
%%
add_client(Address, Secret) when is_list(Address), is_list(Secret) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	add_client(AddressTuple, Secret);
add_client(Address, Secret) when is_tuple(Address), is_list(Secret) ->
	F = fun() ->
				R = #radius_client{address = Address, secret = Secret},
				mnesia:write(R)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec find_client(Address :: inet:ip_address()) ->
	Result :: {ok, Secret :: string()} | error.
%% @doc Look up the shared secret for a client.
%%
find_client(Address) when is_list(Address) ->
	{ok, AddressTuple} = inet_parse:address(Address),
	find_client(AddressTuple);
find_client(Address) when is_tuple(Address) ->
	F = fun() ->
				mnesia:read(radius_client, Address, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#radius_client{secret = Secret}]} ->
			{ok, Secret};
		{atomic, []} ->
			error;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec add_user(UserName :: string(), Password :: string(),
		Attributes :: binary() | [byte()]) -> ok | {error, Reason :: term()}.
%% @doc Store the password and static attributes for a user.
%%
add_user(UserName, Password, Attributes) when is_list(UserName),
		is_list(Password), (is_list(Attributes) orelse is_binary(Attributes)) -> 
	F = fun() ->
				R = #radius_user{name = UserName, password = Password,
						attributes = Attributes},
				mnesia:write(R)
	end,
	case mnesia:transaction(F) of
		{atomic, ok} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec find_user(UserName :: string()) ->
	Result :: {ok, Password :: string(),
		Attributes :: binary() | [byte()]} | error.
%% @doc Look up a user and return the password and attributes assigned.
%%
find_user(UserName) when is_list(UserName) ->
	F = fun() ->
				mnesia:read(radius_user, UserName, read)
	end,
	case mnesia:transaction(F) of
		{atomic, [#radius_user{password = Password, attributes = Attributes}]} ->
			{ok, Password, Attributes};
		{atomic, []} ->
			error;
		{aborted, Reason} ->
			exit(Reason)
	end.

-spec log_file(FileName :: string()) -> ok.
%% @doc Write all logged accounting records to a file.
%% 
log_file(FileName) when is_list(FileName) ->
   {ok, IODevice} = file:open(FileName, [write]),
   file_chunk(?LOGNAME, IODevice, start).

%%----------------------------------------------------------------------
%%  The radius_example private API
%%----------------------------------------------------------------------

-spec install(Nodes :: [node()]) -> {ok, Tables :: [atom()]}.
%% @doc Initialize a new installation.
%% 	`Nodes' is a list of the nodes where the 
%% 	{@link //radius_example. radius_example} application will run.
%% 	An mnesia schema should be created and mnesia started on
%% 	all nodes before running this function. e.g.&#058;
%% 	```
%% 		1> mnesia:create_schema([node()]).
%% 		ok
%% 		2> mnesia:start().
%% 		ok
%% 		3> {@module}:install([node()]).
%% 		{ok,[radius_client,radius_user]}
%% 		ok
%% 	'''
%%
%% @private
%%
install(Nodes) when is_list(Nodes) ->
	try
		case mnesia:wait_for_tables([schema], ?WAITFORSCHEMA) of
			ok ->
				ok;
			SchemaResult ->
				throw(SchemaResult)
		end,
		case mnesia:create_table(radius_client, [{disc_copies, Nodes},
				{attributes, record_info(fields, radius_client)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new radius_client table.~n");
			{aborted, {already_exists, radius_client}} ->
				error_logger:warning_msg("Found existing radius_client table.~n");
			T1Result ->
				throw(T1Result)
		end,
		case mnesia:create_table(radius_user, [{disc_copies, Nodes},
				{attributes, record_info(fields, radius_user)}]) of
			{atomic, ok} ->
				error_logger:info_msg("Created new radius_user table.~n");
			{aborted, {already_exists, radius_user}} ->
				error_logger:warning_msg("Found existing radius_user table.~n");
			T2Result ->
				throw(T2Result)
		end,
		Tables = [radius_client, radius_user],
		case mnesia:wait_for_tables(Tables, ?WAITFORTABLES) of
			ok ->
				Tables;
			TablesResult ->
				throw(TablesResult)
		end
	of
		Result -> {ok, Result}
	catch
		throw:Error ->
			mnesia:error_description(Error)
	end.
	
%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @hidden
file_chunk(Log, IODevice, Continuation) ->
	case disk_log:chunk(Log, Continuation) of
		eof ->
			file:close(IODevice);
		{error, Reason} ->
			file:close(IODevice),
			exit(Reason);
		{Continuation2, Terms} ->
			Fun =  fun(Event) ->
						io:fwrite(IODevice, "~999p~n", [Event])
			end,
			lists:foreach(Fun, Terms),
			file_chunk(Log, IODevice, Continuation2)
	end.

