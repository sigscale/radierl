%%% radius_fsm.erl
%%%---------------------------------------------------------------------
%%% @copyright 2016-2021 SigScale Global Inc
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%%
%%% Copyright (c) 2016-2021, SigScale Global Inc
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
%%% @doc This {@link //stdlib/gen_statem. gen_statem} behaviour callback
%%% 	module implements a transaction state handler in the
%%% 	{@link //radius. radius} application.
%%% 	This transaction handler executes the handlers in the callback
%%% 	module and handles retransmission of responses, when retransmitted 
%%% 	requests are received, without calling the callback handler. 
%%% 
%%% 	The process communication is as depicted in <a href="messages.png">
%%% 	Figure-1.1</a>.
%%% 
%%% 	<img alt="Figure-1.1" src="../examples/doc/callbacks-request.png" />
%%%
-module(radius_fsm).
-copyright('Copyright (c) 2016-2021 SigScale Global Inc').
-author('vances@sigscale.org').

-behaviour(gen_statem).

%% export the radius_fsm API
-export([]).

%% export the callbacks needed for gen_statem behaviour
-export([init/1, handle_event/4, callback_mode/0,
			terminate/3, code_change/4]).
%% export the callbacks for gen_statem states.
-export([idle/3, wait_for_response/3]).

-include("radius.hrl").

-type state() :: idle | wait_for_response.

-record(statedata,
		{socket :: inet:socket(),
		module :: atom(),
		user_state :: term(),
		address :: inet:ip_address(),
		port :: pos_integer(),
		identifier :: non_neg_integer(),
		authenticator :: binary(),
		response :: ignore | undefined | term()}).
-type statedata() :: #statedata{}.

-define(WAITSTART,   4000).
-define(WAITRETRIES, 8000).

%%----------------------------------------------------------------------
%%  The radius_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The radius_fsm gen_statem call backs
%%----------------------------------------------------------------------

-spec callback_mode() -> Result
	when
		Result :: gen_statem:callback_mode_result().
%% @doc Set the callback mode of the callback module.
%% @see //stdlib/gen_statem:callback_mode/0
%% @private
%%
callback_mode() ->
	[state_functions].

-spec init(Args) -> Result
	when
		Args :: [term()],
		Result :: {ok, State, Data} | {ok, State, Data, Actions}
				| ignore | {stop, Reason},
		State :: state(),
		Data :: statedata(),
		Actions :: Action | [Action],
		Action :: gen_statem:action(),
		Reason :: term().
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_statem:init/1
%% @private
%%
init([Socket, Module, UserState, Address, Port, Identifier] = _Args) ->
	process_flag(trap_exit, true),
	StateData = #statedata{socket = Socket, module = Module,
			user_state = UserState, address = Address,
			port = Port, identifier = Identifier, authenticator = <<>>},
	{ok, idle, StateData, ?WAITSTART}.

-spec idle(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>idle</em> state.
%% @private
%%
idle(cast, <<_Code, Identifier, Authenticator:16/binary, _/binary>> = _Event,
		#statedata{identifier = Identifier, authenticator = Authenticator,
				response = ignore} = _StateData) ->
		{keep_state_and_data, ?WAITRETRIES};
idle(cast, <<_Code, Identifier, Authenticator:16/binary, _/binary>> = Event,
		#statedata{identifier = Identifier, authenticator = CachedAuthenticator,
		socket = Socket, module = Module, user_state = UserState,
		address = Address, port = Port} = StateData)
		when Authenticator /= CachedAuthenticator ->
	case Module:request(Address, Port, Event, UserState) of
		{ok, wait} ->
			NewStateData = StateData#statedata{authenticator = Authenticator},
			{next_state, wait_for_response, NewStateData, ?WAITRETRIES};
		{ok, RadiusResponse} ->
			NewStateData = StateData#statedata{authenticator = Authenticator,
					response = RadiusResponse},
			case gen_udp:send(Socket, Address, Port, RadiusResponse) of
				ok ->
					{keep_state, NewStateData, ?WAITRETRIES};
				{error, Reason} ->
					{stop, Reason, NewStateData}
			end;
		{error, ignore} ->
			NewStateData = StateData#statedata{authenticator = Authenticator,
					response = ignore},
			{keep_state, NewStateData, ?WAITRETRIES};
		{error, Reason} ->
			NewStateData = StateData#statedata{authenticator = Authenticator},
			{stop, Reason, NewStateData}
	end;
idle(cast, <<_Code, Identifier, Authenticator:16/binary, _/binary>> = _Event,
		#statedata{identifier = Identifier, authenticator = Authenticator,
				response = Response, socket = Socket,
				address = Address, port = Port} = StateData) ->
	case gen_udp:send(Socket, Address, Port, Response) of
		ok ->
			{keep_state_and_data, ?WAITRETRIES};
		{error, Reason} ->
			{stop, Reason, StateData}
	end;
idle(info, {'EXIT', _, shutdown}, _StateData) ->
	keep_state_and_data;
idle(state_timeout, _,
		#statedata{address = Address, port = Port,
		identifier = Identifier} = StateData) ->
	Id = {Address, Port, Identifier},
	{stop, {shutdown, Id}, StateData}.

-spec wait_for_response(EventType, EventContent, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(state()).
%% @doc Handles events received in the <em>wait_for_response</em> state.
%% @private
%%
wait_for_response(cast, <<_Code, Identifier,
		Authenticator:16/binary, _/binary>> = _Event,
		#statedata{identifier = Identifier,
				authenticator = Authenticator} = _StateData) ->
		{keep_state_and_data, ?WAITRETRIES};
wait_for_response(cast, {response, RadiusResponse}, #statedata{socket = Socket,
		address = Address, port = Port} = StateData) ->
	NewStateData = StateData#statedata{response = RadiusResponse},
	case gen_udp:send(Socket, Address, Port, RadiusResponse) of
		ok ->
			{next_state, idle, NewStateData, ?WAITRETRIES};
		{error, Reason} ->
			{stop, Reason, NewStateData}
	end;
wait_for_response(cast, {error, ignore}, StateData) ->
	NewStateData = StateData#statedata{response = ignore},
	{keep_state, NewStateData, ?WAITRETRIES};
wait_for_response(info, {'EXIT', _, shutdown}, _StateData) ->
	keep_state_and_data;
wait_for_response(timeout, _, #statedata{address = Address, port = Port,
		identifier = Identifier} = StateData) ->
	Id = {Address, Port, Identifier},
	{stop, {shutdown, Id}, StateData}.

-spec handle_event(EventType, EventContent, State, Data) -> Result
	when
		EventType :: gen_statem:event_type(),
		EventContent :: term(),
		State :: state(),
		Data :: statedata(),
		Result :: gen_statem:event_handler_result(State).
%% @doc Handles events received in any state.
%% @private
%%
handle_event(_EventType, _EventContent, _State, _Data) ->
	keep_state_and_data.

-spec terminate(Reason, State, Data) -> any()
	when
		Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: state(),
		Data ::  statedata().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_statem:terminate/3
%% @private
%%
terminate(_Reason, _State, _Data) ->
	ok.

-spec code_change(OldVsn, OldState, OldData, Extra) -> Result
	when
		OldVsn :: Version | {down, Version},
		Version ::  term(),
		OldState :: state(),
		OldData :: statedata(),
		Extra :: term(),
		Result :: {ok, NewState, NewData} |  Reason,
		NewState :: state(),
		NewData :: statedata(),
		Reason :: term().
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_statem:code_change/3
%% @private
%%
code_change(_OldVsn, OldState, OldData, _Extra) ->
	{ok, OldState, OldData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

