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
%%% @doc This library module implements the user API to the
%%% 		{@link //radius. radius} application.
%%%
%%% 	===Callback Functions===
%%% 	====init/2====
%%% 	<b><tt>Module:init(Address :: {@link //kernel/inet:ip_address(). ip_address()},
%%% 	Port :: pos_integer()) ->
%%% 		{ok, State :: term()} | {error, Reason :: term()}</tt></b>
%%%
%%% 	Whenever a {@link //radius/radius_server. radius_server} is started
%%% 	using {@link //radius/radius:start/3. radius:start/3,4}, or
%%% 	{@link //radius/radius:start_link/3. radius:start_link/3,4},
%%% 	this function is called by the new process to initialize.
%%% 
%%% 	<tt>Address</tt> is the {@link //kernel/inet:ip_address(). ip_address()}
%%% 	which the {@link //kernel/inet:socket(). socket()} is listening on.
%%% 
%%% 	<tt>Port</tt> is the {@link //kernel/inet:port_number(). port_number()}
%%% 	which the {@link //kernel/inet:socket(). socket()} is listening on.
%%% 
%%% 	This function should return <tt>{ok, State}</tt> if the callback handler
%%% 	will be able service RADIUS requests on this
%%% 	{@link //radius/radius_server. radius_server} or
%%% 	<tt>{error, Reason}</tt> indicating the problem.
%%%
%%% 	<tt>State</tt> may be any term.
%%% 
%%% 	====request/4====
%%% 	<b><tt>Module:request(Address :: {@link //kernel/inet:ip_address(). ip_address()},
%%% 	Port :: pos_integer(), RadiusRequest :: binary(), State :: term()) ->
%%% 	{ok, RadiusResponse :: binary()}
%%% 			| {error, Reason :: ignore | term()}</tt></b>
%%%
%%% 	When a new valid RADIUS packet is received a
%%% 	{@link //radius/radius_fsm. radius_fsm} transaction handler is started
%%% 	which will call this function to process the request. 
%%% 
%%% 	<tt>Address</tt> is the {@link //kernel/inet:ip_address(). ip_address()}
%%% 	of the client making the request.
%%% 
%%% 	<tt>Port</tt> is the {@link //kernel/inet:port_number(). port_number()}
%%% 	of the client making the request.
%%%
%%% 	<tt>State</tt> has the value returned from <tt>Module:init/2</tt>.
%%% 
%%% 	This function should return <tt>{ok, RadiusResponse}</tt>,
%%% 	if the request was valid, where  <tt>RadiusResponse</tt> is a result
%%% 	from a call to {@link //radius/codec. radius:codec/1}.
%%% 	
%%% 	If the RADIUS Authenticator or Attributes were badly formed
%%% 	<tt>{error, ignore}</tt> should be returned to silently discard the
%%% 	received packet and ignore retransmissions.  In the event of an error
%%% 	<tt>{error, Reason}</tt> should be returned which will stop the
%%% 	{@link //radius/radius_fsm. radius_fsm} transaction handler and report
%%% 	an error.
%%% 
%%% 	====terminate/2====
%%% 	<b><tt>Module:terminate(Reason :: term(), State :: term()) ->
%%% 		any()</tt></b>
%%%
%%% 	This function is called by a {@link //radius/radius_server. radius_server}
%%% 	when it is about to terminate. It should be the opposite of
%%% 	<tt>Module:init/1</tt> and do any necessary cleaning up. When it
%%% 	returns, the {@link //radius/radius_server. radius_server} terminates
%%% 	with <tt>Reason</tt>. The return value is ignored.
%%%
%%% 	<tt>State</tt> has the value returned from <tt>Module:init/2</tt>.
%%% 
-module(radius).
-copyright('Copyright (c) 2011-2016 Motivity Telecom').
-author('vances@motivity.ca').

%% export the radius public API
-export([start/2, start/3, start_link/2, start_link/3, stop/1]).
-export([codec/1]).

%% export the radius private API
-export([port/1, authenticator/2]).

%% define the functions a radius callback module must export
-callback init(Address :: inet:ip_address(), Port :: integer()) ->
	{ok, State :: term()} | {error, Reason :: term()}.
-callback request(Address :: inet:ip_address(), Port :: integer(),
		Packet :: binary(), State :: term()) ->
	{ok, Response :: binary()} | {error, Reason :: ignore | term()}.
-callback terminate(Reason :: term(), State :: term()) -> any().

%% @headerfile "radius.hrl"
-include("radius.hrl").

%%----------------------------------------------------------------------
%%  The radius public API
%%----------------------------------------------------------------------

-spec start(Module :: atom(), Port :: non_neg_integer()) ->
	{ok, Pid :: pid()}
		| {error, Error :: already_present
		| {already_started, Pid :: pid()} | term()}.
%% @doc Start a RADIUS protocol server using the callback module
%% 	`Module' listening on `Port'.
%%
start(Module, Port) when is_atom(Module), is_integer(Port) ->
	{ok, Address} = application:get_env(radius, address),
	start(Module, Port, Address).

-spec start(Module :: atom(), Port :: non_neg_integer(),
		Address :: inet:ip_address()) ->
	{ok, Pid :: pid()}
		| {error, Error :: already_present
		| {already_started, Pid :: pid()} | term()}.
%% @doc Start a RADIUS protocol server using the callback module
%% 	`Module' listening on `Port' with address `Address'.
%%
start(Module, Port, Address) when is_list(Address) ->
	case inet_parse:address(Address) of
		{ok, IP} ->
			start(Module, Port, IP);
		{error, Reason} ->
			{error, Reason}
	end;
start(Module, Port, Address) when is_atom(Module),
		is_integer(Port), is_tuple(Address) ->
	supervisor:start_child(radius, [[Module, Port, Address]]).

-spec start_link(Module :: atom(), Port :: non_neg_integer()) ->
	{ok, Pid :: pid()}
		| {error, Error :: already_present
		| {already_started, Pid :: pid()} | term()}.
%% @doc Start a RADIUS protocol server as part of a supervision tree
%% 	 using the callback module `Module' listening on `Port'.
%%
start_link(Module, Port) ->
	{ok, Sup} = start(Module, Port),
	link(Sup),
	{ok, Sup}.

-spec start_link(Module :: atom(), Port :: non_neg_integer(),
		Address :: inet:ip_address()) ->
	{ok, Pid :: pid()}
		| {error, Error :: already_present
		| {already_started, Pid :: pid()} | term()}.
%% @doc Start a RADIUS protocol server as part of a supervision tree
%% 	using the callback module `Module' listening on `Port' with
%% 	address `Address'.
%%
start_link(Module, Port, Address) ->
	{ok, Sup} = start(Module, Port, Address),
	link(Sup),
	{ok, Sup}.

-spec stop(Pid :: pid()) -> ok.
%% @doc Stop a running RADIUS protocol server.
%%
stop(Pid) ->
	supervisor:terminate_child(whereis(radius), Pid).

-spec codec(In :: binary() | #radius{}) -> Out :: #radius{} | binary().
%% 	Out = radius() | binary()
%% @doc Encode or decode a binary RADIUS protocol packet.
%%
codec(<<Code, Identifier, Length:16, Authenticator:16/binary,
		_/binary>> = In) ->
	Attributes = binary:part(In, 20, Length - 20), 
	#radius{code = Code, id = Identifier, 
			authenticator = binary_to_list(Authenticator),
			attributes = Attributes};
codec(#radius{authenticator = Authenticator} = In)
		when length(Authenticator) == 16 ->
	codec(In#radius{authenticator = list_to_binary(Authenticator)});
codec(#radius{attributes = Attributes} = In)
		when is_list(Attributes) ->
	codec(In#radius{attributes = radius_attributes:codec(Attributes)});
codec(#radius{code = Code, id = Identifier,
		authenticator = Authenticator, attributes = Attributes})
		when is_integer(Code), is_integer(Identifier),
		is_binary(Authenticator), is_binary(Attributes) ->
	Length = size(Attributes) + 20,
	<<Code, Identifier, Length:16,
			Authenticator/binary, Attributes/binary>>.
	
%%----------------------------------------------------------------------
%%  The radius private API
%%----------------------------------------------------------------------

-spec port(Pid :: pid()) -> Port :: pos_integer().
%% @doc Return the `Port' which the RADIUS server is listening on.
%% @private
port(Pid) ->
	Children = supervisor:which_children(Pid),
   {_Id, Server, _Type, _Modules} = lists:keyfind(radius_server, 1, Children),
	gen_server:call(Server, port).

-spec authenticator(SharedSecret :: string(), Id :: integer()) ->
	Authenticator :: [byte()].
%% @doc Return a 16 octet random number to use as a request Authenticator.
%% @private
authenticator(SharedSecret, Id) ->
	{X, Y, Z} = erlang:now(),
	binary_to_list(erlang:md5([SharedSecret, Id, <<X:32, Y:32, Z:32>>])).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

