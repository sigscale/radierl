%%%---------------------------------------------------------------------
%%% @copyright 2016-2018 SigScale Global Inc
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%%
%%% Copyright (c) 2016-2018, SigScale Global Inc
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
%%% @doc Stub radius callback functions for test suites.
%%%--------------------------------------------------------------------
%%%
-module(radius_test_callback).

-export([init/2, request/4, terminate/2]).

-include("radius.hrl").

%%----------------------------------------------------------------------
%%  The radius callbacks
%%----------------------------------------------------------------------

%% @spec (Port, Opts) -> Result
%% 	Port = integer()
%% 	Opts = list()
%% 	Result = {ok, State} | {error, Reason}
%% 	State = term()
%% 	Reason = term()
%% @doc This callback function is called when a
%% 	{@link //radius/radius_server. radius_server} behaviour process
%% 	initializes.
%%
init(_Port, _Opts) ->
	{ok, ?MODULE}.

%% @spec (Address, Port, Packet, State) -> Result
%% 	Address = ip_address()
%% 	Port = integer()
%% 	Packet = binary()
%% 	State = term()
%% 	Result = binary() | {error, Reason}
%% 	Reason = ignore | term()
%% @doc This function is called when a request is received on the port.
%%
request(_Address, _Port, Packet, ?MODULE = _State) ->
	Secret = "xyzzy5461",
	case radius:codec(Packet) of
		#radius{code = ?AccessRequest, id = Id,
				authenticator = Authenticator,
				attributes = BinaryAttributes} ->
			try
				Attributes = radius_attributes:codec(BinaryAttributes),
				case radius_attributes:fetch(?UserName, Attributes) of
					"nemo" ->
						UserPassword  = radius_attributes:fetch(?UserPassword,
								Attributes),
						"arctangent" = radius_attributes:unhide(Secret,
								Authenticator, UserPassword),
						immediate;
					"walter" ->
						UserPassword  = radius_attributes:fetch(?UserPassword,
								Attributes),
						"white" = radius_attributes:unhide(Secret,
								Authenticator, UserPassword),
						delayed
				end
			of
				immediate ->
					accept(Id, Authenticator, Secret);
				delayed ->
					RadiusFsm = self(),
					{ok, Response} = accept(Id, Authenticator, Secret),
					timer:apply_after(200, radius, response, [RadiusFsm, {response, Response}]),
					{ok, wait}
			catch
				_:_ ->
					reject(Packet, Secret)
			end;
		_ ->
			{error, ignore}
	end.

%% @spec (Reason, State) -> ok
%% 	Reason = term()
%% 	State = term()
%% @doc This callback function is called just before the server exits.
%%
terminate(_Reason, ?MODULE = _State) ->
	ok.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

%% @hidden
accept(Id, RequestAuthenticator, Secret) ->
	ResponseAuthenticator = crypto:hash(md5, [<<?AccessAccept, Id, 20:16>>,
			RequestAuthenticator, Secret]), 
	Response = #radius{code = ?AccessAccept, id = Id,
			authenticator = ResponseAuthenticator, attributes = []},
	{ok, radius:codec(Response)}.

%% @hidden
reject(<<_Code, Id, _Len:16, Authenticator:16/binary, _/binary>>, Secret) ->
	ResponseAuthenticator = crypto:hash(md5, [<<?AccessReject, Id, 96:16>>,
			Authenticator, Secret]), 
	Response = #radius{code = ?AccessReject, id = Id,
			authenticator = ResponseAuthenticator, attributes = []},
	{ok, radius:codec(Response)}.

