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
%%% @doc A {@link //stdlib/gen_server. gen_server} callback module
%%% 	for the {@link //radius. radius} application.
%%% 	This server handles all incoming packets on the socket and spawns
%%% 	a {@link //radius/radius_fsm. radius_fsm} process to handle a new
%%% 	transaction based on the source address, port and RADIUS Identifier.
%%% 	It first verifies that the RADIUS Code and Length are valid,
%%% 	otherwise it silently discards.
%%%
%%% 	The process communication is as depicted in <a href="messages.png">
%%% 	Figure-1.1</a>.
%%%
%%% 	<img alt="Figure-1.1" src="messages.png" />
%%%
-module(radius_server).
-copyright('Copyright (c) 2016-2018 SigScale Global Inc').
-author('vances@sigscale.org').

-behaviour(gen_server).

%% export the radius_server API
-export([]).

%% export the call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

-include("radius.hrl").

-record(state,
		{sup :: pid(),
		socket :: inet:socket(),
		address :: inet:ip_address(),
		port :: non_neg_integer(),
		module :: atom(),
		user_state :: term(),
		fsm_sup :: undefined | pid(),
		handlers = gb_trees:empty() :: gb_trees:tree(Key ::
				{Address :: inet:ip_address(), Port :: pos_integer(),
				Identifier :: non_neg_integer()}, Value :: (Fsm :: pid()))}).

%%----------------------------------------------------------------------
%%  The radius_server API
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%  The radius_server gen_server call backs
%%----------------------------------------------------------------------

-spec init(Args :: list()) -> Result :: {ok, State :: #state{}}
		| {ok, State :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {stop, Reason :: term()} | ignore.
%% @doc Initialize the {@module} server.
%% 	Args :: [Sup :: pid(), Module :: atom(), Port :: non_neg_integer(),
%% 	Opts :: list().
%% @see //stdlib/gen_server:init/1
%% @private
%%
init([Sup, Module, Port, Opts] = _Args) ->
	try
		{Address, PortOpened, Socket} = case gen_udp:open(Port,
				Opts ++ [{active, once}, binary]) of
			{ok, S} ->
				case inet:sockname(S) of
					{ok, {A, P}} ->
						{A, P, S};
					{error, Reason1} ->
						throw(Reason1)
				end;
			{error, Reason1} ->
				throw(Reason1)
		end,
		case Module:init(Address, PortOpened) of
			{ok, UserState} ->
				#state{sup = Sup, socket = Socket, address = Address,
						port = PortOpened, module = Module,
						user_state = UserState};
			{error, Reason2} ->
				throw(Reason2)
		end
	of
		State ->
			process_flag(trap_exit, true),
			{ok, State, 0}
	catch
		_:Error->
			{stop, Error}
	end.

-spec handle_call(Request :: term(), From :: {Pid :: pid(), Tag :: any()},
		State :: #state{}) ->
	Result :: {reply, Reply :: term(), NewState :: #state{}}
		| {reply, Reply :: term(), NewState :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {reply, Reply :: term(), NewState :: #state{}, hibernate}
		| {noreply, NewState :: #state{}}
		| {noreply, NewState :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {noreply, NewState :: #state{}, hibernate}
		| {stop, Reason :: term(), Reply :: term(), NewState :: #state{}}
		| {stop, Reason :: term(), NewState :: #state{}}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:call/2.
%% 	gen_server:call/2,3} or {@link //stdlib/gen_server:multi_call/2.
%% 	gen_server:multi_call/2,3,4}.
%% @see //stdlib/gen_server:handle_call/3
%% @private
%%
handle_call(shutdown, _From, State) ->
	{stop, normal, ok, State};
handle_call(port, _From, #state{port = Port} = State) ->
	{reply, Port, State};
handle_call(_Request, {Pid, _Tag}, State) ->
	exit(Pid, badarg),
	{noreply, State}.

-spec handle_cast(Request :: term(), State :: #state{}) ->
	Result :: {noreply, NewState :: #state{}}
		| {noreply, NewState :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {noreply, NewState :: #state{}, hibernate}
		| {stop, Reason :: term(), NewState :: #state{}}.
%% @doc Handle a request sent using {@link //stdlib/gen_server:cast/2.
%% 	gen_server:cast/2} or {@link //stdlib/gen_server:abcast/2.
%% 	gen_server:abcast/2,3}.
%% @see //stdlib/gen_server:handle_cast/2
%% @private
%%
handle_cast(_Request, State) ->
	{noreply, State}.

-spec handle_info(Info :: timeout | term(), State :: #state{}) ->
	Result :: {noreply, NewState :: #state{}}
		| {noreply, NewState :: #state{}, Timeout :: non_neg_integer() | infinity}
		| {noreply, NewState :: #state{}, hibernate}
		| {stop, Reason :: term(), NewState :: #state{}}.
%% @doc Handle a received message.
%% @see //stdlib/gen_server:handle_info/2
%% @private
%%
handle_info(timeout, #state{sup = Sup, fsm_sup = undefined} = State) ->
	Siblings = supervisor:which_children(Sup),
	{_Id, FsmSup, _Type, _Modules} = lists:keyfind(radius_fsm_sup, 1, Siblings),
	{noreply, State#state{fsm_sup = FsmSup}};
handle_info({udp, Socket, _Address, _Port, <<Code, _/binary>>},
		#state{socket = Socket} = State)
		when Code < ?AccessRequest ->
	case inet:setopts(Socket, [{active, once}]) of
		ok ->
			{noreply, State};
		{error, Reason} ->
			{stop, Reason, State}
	end;
handle_info({udp, Socket, _Address, _Port, <<Code, _/binary>>},
		#state{socket = Socket} = State)
		when Code > ?AccountingResponse, Code < ?AccessChallenge->
	case inet:setopts(Socket, [{active, once}]) of
		ok ->
			{noreply, State};
		{error, Reason} ->
			{stop, Reason, State}
	end;
handle_info({udp, Socket, _Address, _Port, <<Code, _/binary>>},
		#state{socket = Socket} = State)
		when Code > ?AccessChallenge, Code < ?DisconnectRequest ->
	case inet:setopts(Socket, [{active, once}]) of
		ok ->
			{noreply, State};
		{error, Reason} ->
			{stop, Reason, State}
	end;
handle_info({udp, Socket, _Address, _Port, <<Code, _/binary>>},
		#state{socket = Socket} = State)
		when Code > ?CoANak ->
	case inet:setopts(Socket, [{active, once}]) of
		ok ->
			{noreply, State};
		{error, Reason} ->
			{stop, Reason, State}
	end;
handle_info({udp, Socket, _Address, _Port,
		<<_Code, _Identifier, Length:16, _/binary>> = Packet},
		#state{socket = Socket} = State)
		when size(Packet) < Length ->
	case inet:setopts(Socket, [{active, once}]) of
		ok ->
			{noreply, State};
		{error, Reason} ->
			{stop, Reason, State}
	end;
handle_info({udp, Socket, Address, Port,
		<<_Code, Identifier, _/binary>> = Packet},
		#state{socket = Socket, handlers = Handlers} = State) ->
	Key = {Address, Port, Identifier},
	NewState = case gb_trees:lookup(Key, Handlers) of
		none ->
			start_fsm(State, Address, Port, Identifier, Packet);
		{value, Fsm} ->
			gen_fsm:send_event(Fsm, Packet),
			State
	end,
	case inet:setopts(Socket, [{active, once}]) of
		ok ->
			{noreply, NewState};
		{error, Reason} ->
			{stop, Reason, NewState}
	end;
handle_info({'EXIT', _Pid, {shutdown, Key}},
		#state{handlers = Handlers} = State) ->
	NewHandlers = gb_trees:delete(Key, Handlers),
	NewState = State#state{handlers = NewHandlers},
	{noreply, NewState};
handle_info({'EXIT', Fsm, _Reason},
		#state{handlers = Handlers} = State) ->
	Fdel = fun(_F, {Key, Pid, _Iter}) when Pid == Fsm ->
				Key;
			(F, {_Key, _Val, Iter}) ->
				F(F, gb_trees:next(Iter));
			(_F, none) ->
				none
	end,
	Iter = gb_trees:iterator(Handlers),
	case Fdel(Fdel, gb_trees:next(Iter)) of
		none ->
			{noreply, State};
		Key ->
			NewHandlers = gb_trees:delete(Key, Handlers),
			NewState = State#state{handlers = NewHandlers},
			{noreply, NewState}
	end.

-spec terminate(Reason :: normal | shutdown | term(),
		State :: #state{}) -> any().
%% @doc Cleanup and exit.
%% @see //stdlib/gen_server:terminate/3
%% @private
%%
terminate(Reason, #state{module = Module,
		user_state = UserState} = _State) ->
	Module:terminate(Reason, UserState).

-spec code_change(OldVsn :: (Vsn :: term() | {down, Vsn :: term()}),
		State :: #state{}, Extra :: term()) ->
	Result :: {ok, NewState :: #state{}}.
%% @doc Update internal state data during a release upgrade&#047;downgrade.
%% @see //stdlib/gen_server:code_change/3
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec start_fsm(State :: #state{}, Address :: inet:ip_address(),
		Port :: pos_integer(), Identifier :: non_neg_integer(),
		Packet :: binary()) ->
	NewState :: #state{}.
%% @doc Start a new {@link radius_fsm. radius_fsm} transaction state
%%% 	handler and forward the request to it.
%% @hidden
start_fsm(#state{socket = Socket, module = Module, user_state = UserState,
		fsm_sup = Sup, handlers = Handlers} = State,
		Address, Port, Identifier, Packet) ->
	ChildSpec = [[Socket, Module, UserState, Address, Port, Identifier], []],
	case supervisor:start_child(Sup, ChildSpec) of
		{ok, Fsm} ->
			link(Fsm),
			gen_fsm:send_event(Fsm, Packet),
			Key = {Address, Port, Identifier},
			NewHandlers = gb_trees:insert(Key, Fsm, Handlers),
			State#state{handlers = NewHandlers};
		{error, Error} ->
			error_logger:error_report(["Error starting transaction state handler",
					{error, Error}, {supervisor, Sup}, {socket, Socket},
					{address, Address}, {port, Port}, {identifier, Identifier}]),
			State
	end.

