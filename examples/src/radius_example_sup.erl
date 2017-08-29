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
%%% @docfile "{@docsrc radius_example_sup.edoc}"
%%%
-module(radius_example_sup).
-copyright('Copyright (c) 2016-2017 SigScale Global Inc').
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
	ChildSpecs = [authentication(), accounting()],
	{ok, {{one_for_one, 4, 3600}, ChildSpecs}}.

%% @hidden
authentication() ->
	{ok, Port} = application:get_env(authentication_port),
	StartMod = radius_example_authentication_sup,
	StartFunc = {supervisor_bridge, start_link, [StartMod, [Port]]},
	{StartMod, StartFunc, permanent, 4000, supervisor, [StartMod]}.

%% @hidden
accounting() ->
	{ok, Port} = application:get_env(accounting_port),
	StartMod = radius_example_accounting_sup,
	StartFunc = {supervisor_bridge, start_link, [StartMod, [Port]]},
	{StartMod, StartFunc, permanent, 4000, supervisor, [StartMod]}.

