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
%%% @doc This {@link //stdlib/application. application} 
%%% 		behaviour callback module implements the start functions for
%%% 		the {@link //radius. radius} application.
%%%
-module(radius_app).
-copyright('Copyright (c) 2016-2018 SigScale Global Inc').
-author('vances@sigscale.org').

-behaviour(application).

%% call backs needed for application behaviour
-export([start/2, prep_stop/1, stop/1, config_change/3]).

-include("radius.hrl").

-record(state, {}).

%%----------------------------------------------------------------------
%%  The radius_app aplication call backs
%%----------------------------------------------------------------------

-spec start(StartType :: normal | {takeover, Node :: node()}
		| {failover, Node :: node()}, StartArgs :: []) ->
	{ok, Pid :: pid(), #state{}}.
%% @doc Starts the application.
%% @private
%%
start(_StartType = normal, _StartArgs) ->
	case supervisor:start_link({local, radius}, radius_sup, []) of
		{ok, Supervisor} ->
			{ok, Supervisor, #state{}};
		Error ->
			Error
	end;
start({takeover, _Node}, _StartArgs) ->
	{error, not_implemented};
start({failover, _Node}, _StartArgs) ->
	{error, not_implemented}.

-spec prep_stop(State :: #state{}) -> #state{}.
%% @doc Called when the application is about to be shut down,
%% 		before any processes are terminated.
%% @private
%%
prep_stop(State = #state{}) ->
	State.

-spec stop(State :: #state{}) -> ok.
%%
%% @doc Called when the application is about to be shut down,
%% 		before any processes are terminated.
%% @private
%%
stop(_State = #state{}) ->
	ok.

-spec config_change(Changed :: [{Par :: atom(), Val :: term()}],
	New :: [{Par :: atom(), Val :: term()}], Removed :: [Par :: atom()]) -> ok.
%% @doc Called after a code  replacement, if there are any 
%% 		changes to the configuration  parameters.
%% @private
%%
config_change(_Changed, _New, _Removed) -> ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

