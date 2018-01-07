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
%%% @docfile "{@docsrc radius_sup.edoc}"
%%%
-module(radius_sup).
-copyright('Copyright (c) 2016-2018 SigScale Global Inc').
-author('vances@sigscale.org').

-behaviour(supervisor).

%% export the call back needed for supervisor behaviour
-export([init/1]).

%%----------------------------------------------------------------------
%%  The supervisor call back
%%----------------------------------------------------------------------

-spec init(Args :: []) ->
	Result :: {ok,{{RestartStrategy :: one_for_all | one_for_one
		| rest_for_one | simple_one_for_one,
		MaxR :: non_neg_integer(), MaxT :: pos_integer()},
		[ChildSpec :: supervisor:child_spec()]}} | ignore.
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init(_Args) ->
	StartMod = radius_server_sup,
	StartFunc = {supervisor, start_link, [StartMod]},
	ChildSpec = {StartMod, StartFunc, temporary, infinity,
			supervisor, [StartMod]},
	{ok, {{simple_one_for_one, 4, 3600}, [ChildSpec]}}.

