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
%%% @doc This library module implements the user API to the
%%% 		{@link //radius. radius} application.
%%%
%%% 	==Callback Functions==
%%% 	<h3 class="function">Module:init/2</h3>
%%% 	<div class="spec">
%%% 	<b><tt>Module:init(Address, Port) -> {ok, State} {error, Reason}</tt></b>
%%% 	<ul class="definitions">
%%% 		<li><tt>Module = atom()</tt></li>
%%% 		<li><tt>Address = {@link //kernel/inet:ip_address(). ip_address()}</tt></li>
%%% 		<li><tt>Port = {@link //kernel/inet:port_number(). port_number()}</tt></li>
%%% 		<li><tt>Opts = list()</tt></li>
%%% 		<li><tt>State = any()</tt></li>
%%% 		<li><tt>Reason = term()</tt></li>
%%%	</ul></div>
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
%%% 	<h3 class="function">Module:request/4</h3>
%%% 	<div class="spec">
%%% 	<b><tt>Module:request(Address, Port, RadiusRequest, State) ->
%%% 		{ok, RadiusResponse} | {ok, wait} | {error, Reason}</tt></b>
%%% 	<ul class="definitions">
%%% 		<li><tt>Module = atom()</tt></li>
%%% 		<li><tt>Address = {@link //kernel/inet:ip_address(). ip_address()}</tt></li>
%%% 		<li><tt>Port = {@link //kernel/inet:port_number(). port_number()}</tt></li>
%%% 		<li><tt>RadiusRequest :: binary()</tt></li>
%%% 		<li><tt>State = any()</tt></li>
%%% 		<li><tt>RadiusResponse :: binary()</tt></li>
%%% 		<li><tt>Reason = ignore | term()</tt></li>
%%%	</ul></div>
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
%%% 	This function may return <tt>{ok, RadiusResponse}</tt> if the request
%%% 	was valid, and a response is immediately available, where
%%% 	<tt>RadiusResponse</tt> is a result from a call to
%%% 	{@link //radius/codec. radius:codec/1}.
%%%
%%% 	This function may also return <tt>{ok, wait}</tt> if the request
%%% 	was valid but a response is not immediately available, in which case
%%% 	{@link //radius/radius_fsm. radius_fsm} process will wait to receive
%%% 	a response sent with {@link //radius/radius:response/2. response/2}.
%%%
%%% 	If the RADIUS Authenticator or Attributes were badly formed
%%% 	<tt>{error, ignore}</tt> should be returned to silently discard the
%%% 	received packet and ignore retransmissions.  In the event of an error
%%% 	<tt>{error, Reason}</tt> should be returned which will stop the
%%% 	{@link //radius/radius_fsm. radius_fsm} transaction handler and report
%%% 	an error.
%%% 
%%% 	<h3 class="function">Module:terminate/2</h3>
%%% 	<div class="spec">
%%% 	<b><tt>Module:terminate(Reason, State) -> any()</tt></b>
%%% 	<ul class="definitions">
%%% 		<li><tt>Module = atom()</tt></li>
%%% 		<li><tt>Reason = term()</tt></li>
%%% 		<li><tt>State = any()</tt></li>
%%%	</ul></div>
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
-copyright('Copyright (c) 2016-2018 SigScale Global Inc').
-author('vances@sigscale.org').

%% export the radius public API
-export([start/2, start/3, start_link/2, start_link/3, stop/1]).
-export([codec/1, authenticator/0, response/2]).

%% export the radius private API
-export([port/1]).

%% define the functions a radius callback module must export
-callback init(Address :: inet:ip_address(), Port :: integer()) ->
	{ok, State :: term()} | {error, Reason :: term()}.
-callback request(Address :: inet:ip_address(), Port :: integer(),
		Packet :: binary(), State :: term()) ->
	{ok, Response :: binary()} | {ok, wait}
		| {error, Reason :: ignore | term()}.
-callback terminate(Reason :: term(), State :: term()) -> any().

%% @headerfile "radius.hrl"
-include("radius.hrl").

%%----------------------------------------------------------------------
%%  The radius public API
%%----------------------------------------------------------------------

-spec start(Module, Port) ->
		{ok, Pid} | {error, Reason}
	when
		Module :: atom(),
		Port :: non_neg_integer(),
		Pid :: pid(),
		Reason :: already_present | {already_started, Pid} | term().
%% @doc Start a RADIUS protocol server using the callback module
%% 	`Module' listening on `Port'.
%%
start(Module, Port) when is_atom(Module), is_integer(Port) ->
	start(Module, Port, []).

-spec start(Module, Port, Opts) ->
		{ok, Pid} | {error, Reason}
	when
		Module :: atom(),
		Port :: non_neg_integer(),
		Opts :: [{ip, inet:socket_address()} |
				{fd, non_neg_integer()} |
				inet:address_family() |
				{port, inet:port_number()} |
				gen_udp:option()],
		Pid :: pid(),
		Reason :: already_present | {already_started, Pid} | term().
%% @doc Start a RADIUS protocol server using the callback module
%% 	`Module' listening on `Port'.
%% @see //kernel/gen_udp:open/2
%%
start(Module, Port, Opts) when is_atom(Module),
		is_integer(Port), is_list(Opts) ->
	{ok, EnvOpts} = application:get_env(radius, sock_opts),
	supervisor:start_child(radius, [[Module, Port, EnvOpts ++ Opts]]).

-spec start_link(Module, Port) ->
		{ok, Pid} | {error, Reason}
	when
		Module :: atom(),
		Port :: non_neg_integer(),
		Pid :: pid(),
		Reason :: already_present | {already_started, Pid} | term().
%% @doc Start a RADIUS protocol server as part of a supervision tree
%% 	 using the callback module `Module' listening on `Port'.
%%
start_link(Module, Port) ->
	case start(Module, Port) of
		{ok, Sup} ->
			link(Sup),
			{ok, Sup};
		{error, Reason} ->
			{error, Reason}
	end.

-spec start_link(Module, Port, Opts) ->
		{ok, Pid} | {error, Reason}
	when
		Module :: atom(),
		Port :: non_neg_integer(),
		Opts :: [{ip, inet:socket_address()} |
				{fd, non_neg_integer()} |
				inet:address_family() |
				{port, inet:port_number()} |
				gen_udp:option()],
		Pid :: pid(),
		Reason :: already_present | {already_started, Pid} | term().
%% @doc Start a RADIUS protocol server as part of a supervision tree
%% 	using the callback module `Module' listening on `Port'.
%% @see //kernel/gen_udp:open/2
%%
start_link(Module, Port, Opts) ->
	case start(Module, Port, Opts) of
		{ok, Sup} ->
			link(Sup),
			{ok, Sup};
		{error, Reason} ->
			{error, Reason}
	end.

-spec stop(Pid) ->
		ok
	when
		Pid :: pid().
%% @doc Stop a running RADIUS protocol server.
%%
stop(Pid) ->
	supervisor:terminate_child(whereis(radius), Pid).

-spec codec(In) -> Out
	when
		In :: binary() | #radius{},
		Out :: #radius{} | binary().
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

-spec authenticator() -> Authenticator
	when
		Authenticator :: binary().
%% @doc Return a 16 octet random number to use as a request Authenticator.
%% @private
authenticator() ->
	crypto:strong_rand_bytes(16).

-spec response(RadiusFsm, Response) -> ok
	when
		RadiusFsm :: pid(),
		Response :: {response, binary()} | {error, ignore}.
%% @doc Send a delayed response to a {@link //radius/radius_fsm. radius_fsm}.
response(RadiusFsm, {response, Response})
		when is_pid(RadiusFsm), is_binary(Response) ->
	gen_fsm:send_event(RadiusFsm, {response, Response});
response(RadiusFsm, {error, Reason}) when is_pid(RadiusFsm) ->
	gen_fsm:send_event(RadiusFsm, {error, Reason}).

%%----------------------------------------------------------------------
%%  The radius private API
%%----------------------------------------------------------------------

-spec port(Pid) -> Port
	when
	Pid :: pid(),
	Port :: pos_integer().
%% @doc Return the `Port' which the RADIUS server is listening on.
%% @private
port(Pid) ->
	Children = supervisor:which_children(Pid),
   {_Id, Server, _Type, _Modules} = lists:keyfind(radius_server, 1, Children),
	gen_server:call(Server, port).

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

