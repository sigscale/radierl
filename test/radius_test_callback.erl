%%%---------------------------------------------------------------------
%%% @copyright 2011-2014 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2011-2014, Motivity Telecom
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
%%% @doc Stub radius callback functions for test suites.
%%%--------------------------------------------------------------------
%%%
-module(radius_test_callback).

-export([init/2, request/3, terminate/1]).

-include("../include/radius.hrl").

%%----------------------------------------------------------------------
%%  The radius callbacks
%%----------------------------------------------------------------------

%% @spec (Address, Port) -> Result
%% 	Address = ip_address()
%% 	Port = integer()
%% 	Result = ok | {error, Reason}
%% 	Reason = term()
%% @doc This callback function is called when a
%% 	{@link //radius/radius_server. radius_server} behaviour process
%% 	initializes.
%%
init(_Address, _Port) ->
	ok.

%% @spec (Address, Port, Packet) -> Result
%% 	Address = ip_address()
%% 	Port = integer()
%% 	Packet = binary()
%% 	Result = binary() | {error, Reason}
%% 	Reason = ignore | term()
%% @doc This function is called when a request is received on the port.
%%
request(_Address, _Port, Packet) ->
	Secret = "xyzzy5461",
	case radius:codec(Packet) of
		#radius{code = ?AccessRequest, id = Id,
				authenticator = Authenticator,
				attributes = BinaryAttributes} ->
			try
				Attributes = radius_attributes:codec(BinaryAttributes),
				"nemo" = radius_attributes:fetch(?UserName, Attributes),
				UserPassword  = radius_attributes:fetch(?UserPassword, Attributes),
				"arctangent" = radius_attributes:unhide(Secret,
						Authenticator, UserPassword)
			of
				_ ->
					accept(Id, Authenticator, Secret)
			catch
				_:_ ->
					reject(Packet, Secret)
			end;
		_ ->
			{error, ignore}
	end.

%% @spec (Reason) -> ok
%% 	Reason = term()
%% @doc This callback function is called just before the server exits.
%%
terminate(_Reason) ->
	ok.

%%----------------------------------------------------------------------
%%  Internal functions
%%----------------------------------------------------------------------

%% @hidden
accept(Id, RequestAuthenticator, Secret) ->
	ResponseAuthenticator = erlang:md5([<<?AccessAccept, Id, 20:16>>,
			RequestAuthenticator, Secret]), 
	Response = #radius{code = ?AccessAccept, id = Id,
			authenticator = ResponseAuthenticator, attributes = []},
	radius:codec(Response).

%% @hidden
reject(<<_Code, Id, _Len:16, Authenticator:16/binary, _/binary>>, Secret) ->
	ResponseAuthenticator = erlang:md5([<<?AccessReject, Id, 96:16>>,
			Authenticator, Secret]), 
	Response = #radius{code = ?AccessReject, id = Id,
			authenticator = ResponseAuthenticator, attributes = []},
	radius:codec(Response).

