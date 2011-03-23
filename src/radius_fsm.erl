%%%---------------------------------------------------------------------
%%% @copyright 2011 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2011, Motivity Telecom
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
%%% @doc This {@link //stdlib/gen_fsm. gen_fsm} behaviour callback module
%%% 	implements a transaction state handler in the
%%% 	{@link //radius. radius} application.
%%%
-module(radius_fsm).
-copyright('Copyright (c) 2011 Motivity Telecom').
-author('vances@motivity.ca').
-vsn('$Revision$').

-behaviour(gen_fsm).

%% export the radius_fsm API
-export([]).

%% export the call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
			terminate/3, code_change/4]).

%% export the gen_fsm state call backs
-export([idle/2]).

-include("radius.hrl").

%% @type statedata() = #statedata{
%% 	socket = socket(),
%% 	module = atom(),
%% 	address = ip_address(),
%% 	port = integer(),
%% 	identifier = integer()}.
-record(statedata, {socket, module, address, port, identifier, response}).

-define(WAITSTART,   4000).
-define(WAITRETRIES, 8000).

%%----------------------------------------------------------------------
%%  The radius_fsm API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The radius_fsm gen_fsm call backs
%%----------------------------------------------------------------------

%% @spec (Args) -> Result
%% 	Args = list()
%% 	Socket = //kernel/gen_udp:socket()
%% 	Address = //kernel/gen_udp:ip_address()
%% 	Port = integer()
%% 	Identifier = integer()
%% 	Result = {ok, StateName, StateData}
%% 		| {ok, StateName, StateData, Timeout}
%% 		| {ok, StateName, StateData, hibernate}
%% 		| {stop,Reason} | ignore
%% 	StateName = atom()
%% 	StateData = statedata()
%% 	Timeout = integer() | infinity
%% 	Reason = term()
%% @doc Initialize the {@module} finite state machine.
%% @see //stdlib/gen_fsm:init/1
%% @private
%%
init([Socket, Module, Address, Port, Identifier] = _Args) ->
	process_flag(trap_exit, true),
	StateData = #statedata{socket = Socket, module = Module,
			address = Address, port = Port, identifier = Identifier},
	{ok, idle, StateData, ?WAITSTART}.

%% @spec (Event, StateData::statedata()) -> Result
%% 	Event = timeout | term()
%% 	Result = {next_state, NextStateName, NewStateData}
%% 	         | {next_state, NextStateName, NewStateData, Timeout}
%% 	         | {next_state, NextStateName, NewStateData, hibernate}
%% 	         | {stop, Reason, NewStateData}
%% 	NextStateName = atom()
%% 	NewStateData = statedata()
%% 	Timeout = integer() | infinity
%% 	Reason = normal | term()
%% @doc Handle events sent with {@link //stdlib/gen_fsm:send_event/2.
%% 	gen_fsm:send_event/2} in the <b>idle</b> state.
%% @@see //stdlib/gen_fsm:StateName/2
%% @private
%%
idle(<<_Code, Identifier, _/binary>> = _Event,
		#statedata{identifier = Identifier, response = ignore} = StateData) ->
		{next_state, idle, StateData, ?WAITRETRIES};
idle(<<_Code, Identifier, _/binary>> = Event,
		#statedata{identifier = Identifier, response = undefined,
		socket = Socket, module = Module, address = Address,
		port = Port} = StateData) ->
	case Module:request(Address, Port, Event) of
		{error, ignore} ->
			NewStateData = StateData#statedata{response = ignore},
			{next_state, idle, NewStateData, ?WAITRETRIES};
		{error, Reason} ->
			{stop, Reason, StateData};
		Response ->
			NewStateData = StateData#statedata{response = Response},
			case gen_udp:send(Socket, Address, Port, Response) of
				ok ->
					{next_state, idle, NewStateData, ?WAITRETRIES};
				{error, Reason} ->
					{stop, Reason, NewStateData}
			end
	end;
idle(<<_Code, Identifier, _/binary>> = _Event,
		#statedata{identifier = Identifier, response = Response,
		socket = Socket, address = Address, port = Port} = StateData) ->
	case gen_udp:send(Socket, Address, Port, Response) of
		ok ->
			{next_state, idle, StateData, ?WAITRETRIES};
		{error, Reason} ->
			{stop, Reason, StateData}
	end;
idle(timeout, #statedata{address = Address, port = Port,
		identifier = Identifier} = StateData) ->
	Id = {Address, Port, Identifier},
	{stop, {shutdown, Id}, StateData}.

%% @spec (Event::term(), StateName::atom(), StateData::statedata()) -> Result
%% 	Result = {next_state, NextStateName, NewStateData}
%% 	         | {next_state, NextStateName, NewStateData, Timeout}
%% 	         | {next_state, NextStateName, NewStateData, hibernate}
%% 	         | {stop, Reason, NewStateData}
%% 	NextStateName = atom()
%% 	NewStateData = statedata()
%% 	Timeout = integer() | infinity
%% 	Reason = normal | term()
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:send_all_state_event/2.
%% 	gen_fsm:send_all_state_event/2}.
%% @see //stdlib/gen_fsm:handle_event/3
%% @private
%%
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% @spec (Event::term(), From, StateName::atom(), StateData::statedata()) ->
%% 		Result
%% 	From = {pid(), Tag}
%% 	Tag = any()
%% 	Result = {reply, Reply, NextStateName, NewStateData}
%% 	         | {reply, Reply, NextStateName, NewStateData, Timeout}
%% 	         | {reply, Reply, NextStateName, NewStateData, hibernate}
%% 	         | {next_state, NextStateName, NewStateData}
%% 	         | {next_state, NextStateName, NewStateData, Timeout}
%% 	         | {next_state, NextStateName, NewStateData, hibernate}
%% 	         | {stop, Reason, Reply, NewStateData}
%% 	         | {stop, Reason, NewStateData}
%% 	Reply = term()
%% 	NextStateName = atom()
%% 	NewStateData = statedata()
%% 	Timeout = integer() | infinity
%% 	Reason = normal | term()
%% @doc Handle an event sent with
%% 	{@link //stdlib/gen_fsm:sync_send_all_state_event/2.
%% 	gen_fsm:sync_send_all_state_event/2,3}.
%% @see //stdlib/gen_fsm:handle_sync_event/4
%% @private
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

%% @spec (Info::term(), StateName::atom(), StateData::statedata()) -> Result
%% 	Result = {next_state, NextStateName, NewStateData}
%% 	         | {next_state, NextStateName, NewStateData, Timeout}
%% 	         | {next_state, NextStateName, NewStateData, hibernate}
%% 	         | {stop, Reason, NewStateData}
%% 	NextStateName = atom()
%% 	NewStateData = statedata()
%% 	Timeout = integer() | infinity
%% 	Reason = normal | term()
%% @doc Handle a received message.
%% @see //stdlib/gen_fsm:handle_info/3
%% @private
%%
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% @spec (Reason, StateName::atom(), StateData::statedata()) -> any()
%% 	Reason = normal | shutdown | term()
%% @doc Cleanup and exit.
%% @see //stdlib/gen_fsm:terminate/3
%% @private
%%
terminate({shutdown, _Id}, _StateName, _StateData) ->
	ok;
terminate(_Reason, _StateName, _StateData) ->
	ok.

%% @spec (OldVsn, StateName::atom(), StateData::statedata(), Extra::term()) ->
%% 		Result
%% 	OldVsn = Vsn | {down, Vsn}
%% 	Vsn = term()
%% 	Result = {ok, NextStateName, NewStateData}
%% 	NextStateName = atom()
%% 	NewStateData = statedata()
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_fsm:code_change/4
%% @private
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

