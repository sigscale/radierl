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
-module(radius_example_accounting_sup).
-copyright('Copyright (c) 2016-2017 SigScale Global Inc').
-author('vances@sigscale.org').

-behaviour(supervisor_bridge).

%% export the call back needed for supervisor behaviour
-export([init/1, terminate/2]).

-record(state, {sup}).

%%----------------------------------------------------------------------
%%  The supervisor_bridge callbacks
%%----------------------------------------------------------------------

-spec init(Args :: list()) ->
	Result :: {ok, Pid :: pid(), State :: #state{}}
		| ignore | {error, Error :: term()}.
%% @doc Initialize the {@module} supervisor_bridge.
%% 	Args :: [Port : pos_integer()].
%% @see //stdlib/supervisor_bridge:init/1
%% @private
%%
init([Port] = _Args) ->
	StartMod = radius_example_accounting,
	{ok, Pid} = radius:start_link(StartMod, Port),
	{ok, Pid, #state{sup = Pid}}.

-spec terminate(Reason :: shutdown | term(), State :: #state{}) -> any().
%% @doc This function is called when it is about to terminate.
terminate(_Reason, #state{sup = Sup} = _State) ->
	radius:stop(Sup).

