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
%%% @doc This {@link //radius/radius. radius} behaviour module
%%% 	implements an example call back module for authentication
%%% 	in the {@link //radius. radius} application.
%%%
-module(radius_example_authentication).
-copyright('Copyright (c) 2016-2017 SigScale Global Inc').
-author('vances@sigscale.org').

-behaviour(radius).

%% export the radius behaviour callbacks
-export([init/2, request/4, terminate/2]).

%% @headerfile "radius.hrl"
-include_lib("radius/include/radius.hrl").

%%----------------------------------------------------------------------
%%  The radius callbacks
%%----------------------------------------------------------------------

-spec init(Address :: inet:ip_address(), Port :: pos_integer()) ->
	{ok, State :: term()} | {error, Reason :: term()}.
%% @doc This callback function is called when a
%% 	{@link //radius/radius_server. radius_server} behaviour process
%% 	initializes.
%%
init(_Address, _Port) ->
	{ok, []}.

-spec request(Address :: inet:ip_address(), Port :: pos_integer(),
		Packet :: binary(), State :: term()) ->
	{ok, Result :: binary()} | {error, Reason :: ignore | term()}.
%% @doc This function is called when a request is received on the port.
%%
request(Address, _Port, Packet, _State) ->
	case radius_example:find_client(Address) of
		{ok, Secret} ->
			request(Packet, Secret);
		error ->
			{error, ignore}
	end.
%% @hidden
request(Packet, Secret) ->
	try 
		#radius{code = ?AccessRequest, id = Id,
				authenticator = Authenticator,
				attributes = BinaryAttributes} = radius:codec(Packet),
		RequestAttributes = radius_attributes:codec(BinaryAttributes),
		UserNameV = radius_attributes:find(?UserName, RequestAttributes),
		NasIpAddressV = radius_attributes:find(?NasIpAddress, RequestAttributes),
		NasIdentifierV = radius_attributes:find(?NasIdentifier,
				RequestAttributes),
		Name = case {UserNameV, NasIpAddressV, NasIdentifierV} of
			{_, {error, not_found}, {error, not_found}} ->
				throw(reject);
			{{error, not_found}, {ok, NasIpAddress}, {error, not_found}} ->
				NasIpAddress;
			{{error, not_found}, {error, not_found}, {ok, NasIdentifier}} ->
				NasIdentifier;
			{{ok, UserName}, _, _} ->
				UserName
		end,
		UserPasswordV = radius_attributes:find(?UserPassword, RequestAttributes),
		ChapPasswordV = radius_attributes:find(?ChapPassword, RequestAttributes),
		StateV = radius_attributes:find(?State, RequestAttributes),
		case {UserPasswordV, ChapPasswordV, StateV} of
			{{error, not_found}, {error, not_found}, {error, not_found}} ->
				throw(reject);
			{{error, not_found}, {error, not_found}, {ok, _State}} ->
				% @todo Handle State?
				throw(not_implemented);
			{{ok, UserPassword}, {error, not_found}, _State} ->
				Password = radius_attributes:unhide(Secret,
						Authenticator, UserPassword),
				case radius_example:find_user(Name) of
					{ok, Password, UserAttributes} ->
						accept(Id, Authenticator, Secret, UserAttributes);
					{ok, _Password, _UserAttributes} ->
						throw(reject);
					error ->
						throw(reject)
				end;
			{{error, not_found}, {ok, {ChapId, ChapResponse}}, _State} ->
				Challenge = case radius_attributes:find(?ChapChallenge,
						RequestAttributes) of
					{ok, ChapChallenge} ->
						ChapChallenge;
					{error, not_found} ->
						Authenticator
				end,
				case radius_example:find_user(Name) of
					{ok, Password, UserAttributes} ->
						case binary_to_list(crypto:hash(md5, [ChapId, Password,
								Challenge])) of
							ChapResponse ->
								accept(Id, Authenticator, Secret, UserAttributes);
							_ ->
								reject(Packet, Secret)
						end;
					{error, not_found} ->
						throw(reject)
				end;
			{{ok, _UserPassword}, {ok, _ChapPassword}, _State} ->
				throw(reject)
		end
	catch
		exit:Reason2 ->
			{error, Reason2};
		throw:reject ->
			reject(Packet, Secret)
	end.

-spec terminate(Reason :: term(), State :: term()) -> ok.
%% @doc This callback function is called just before the server exits.
%%
terminate(_Reason, _State) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec reject(Request :: binary(), Secret :: string()) ->
	{ok, AccessReject :: binary()}.
%% @hidden
reject(<<_Code, Id, _Len:16, Authenticator:16/binary, _/binary>>, Secret) ->
	Attributes = [],
	Length = length(Attributes) + 20,
	ResponseAuthenticator = crypto:hash(md5, [<<?AccessReject, Id, Length:16>>,
			Authenticator, Secret]), 
	Response = #radius{code = ?AccessReject, id = Id,
			authenticator = ResponseAuthenticator, attributes = []},
	{ok, radius:codec(Response)}.

-spec accept(Id :: byte(), RequestAuthenticator :: [byte()],
		Secret :: string(), Attributes :: binary() | [byte()]) ->
	{ok, AccessAccept :: binary()}.
%% @hidden
accept(Id, RequestAuthenticator, Secret, AttributeList)
		when is_list(AttributeList) -> 
	Attributes = radius_attributes:codec(AttributeList),
	accept(Id, RequestAuthenticator, Secret, Attributes);
accept(Id, RequestAuthenticator, Secret, ResponseAttributes) 
		when is_binary(ResponseAttributes) -> 
	Length = size(ResponseAttributes) + 20,
	ResponseAuthenticator = crypto:hash(md5, [<<?AccessAccept, Id, Length:16>>,
			RequestAuthenticator, ResponseAttributes, Secret]), 
	Response = #radius{code = ?AccessAccept, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = ResponseAttributes},
	{ok, radius:codec(Response)}.

