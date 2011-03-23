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
%%% @docfile "doc/radius_sup.edoc"
%%%
-module(radius_server_sup).
-copyright('Copyright (c) 2011 Motivity Telecom').
-author('vances@motivity.ca').
-vsn('$Revision$').

-behaviour(supervisor).

%% export the call back needed for supervisor behaviour
-export([init/1]).

%%----------------------------------------------------------------------
%%  The supervisor call back
%%----------------------------------------------------------------------

%% @spec (Args) -> Result
%% 	Args = list()
%% 	Result = {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
%% 	RestartStrategy = one_for_all | one_for_one | rest_for_one
%% 		| simple_one_for_one
%% 	MaxR = integer()
%% 	MaxT = integer()
%% @doc Initialize the {@module} supervisor.
%% @see //stdlib/supervisor:init/1
%% @private
%%
init([Module, Port, Address]) ->
	ChildSpecs = [fsm_sup(), server(Module, Port, Address)],
	{ok, {{one_for_all, 10, 3600}, ChildSpecs}}.

%% @hidden
fsm_sup() ->
	StartMod = radius_fsm_sup,
	StartFunc = {supervisor, start_link, [StartMod, []]},
	{StartMod, StartFunc, transient, infinity, supervisor, [StartMod]}.

%% @hidden
server(Module, Port, Address) ->
	StartMod = radius_server,
	StartArgs = [StartMod, [self(), Module, Port, Address], []],
	StartFunc = {gen_server, start_link, StartArgs},
	{StartMod, StartFunc, transient, 4000, worker, [StartMod]}.

