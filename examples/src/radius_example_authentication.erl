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
%%% @doc This {@link //radius/radius. radius} behaviour module
%%% 	implements an example call back module for authentication
%%% 	in the {@link //radius. radius} application.
%%%
-module(radius_example_authentication).
-copyright('Copyright (c) 2011 Motivity Telecom').
-author('vances@motivity.ca').

-behaviour(radius).

%% export the radius behaviour callbacks
-export([init/2, request/3, terminate/1]).

%% @headerfile "../../include/radius.hrl"
-include("radius.hrl").

%%----------------------------------------------------------------------
%%  The radius callbacks
%%----------------------------------------------------------------------

-spec init(Address :: inet:ip_address(), Port :: pos_integer()) ->
	Result :: ok | {error, Reason :: term()}.
%% @doc This callback function is called when a
%% 	{@link //radius/radius_server. radius_server} behaviour process
%% 	initializes.
%%
init(_Address, _Port) ->
	ok.

-spec request(Address :: inet:ip_address(), Port :: pos_integer(),
		Packet :: binary()) ->
	Result :: binary() | {error, Reason :: ignore | term()}.
%% @doc This function is called when a request is received on the port.
%%
request(Address, _Port, Packet) ->
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
			{_, error, error} ->
				throw(reject);
			{error, {ok, NasIpAddress}, error} ->
				NasIpAddress;
			{error, error, {ok, NasIdentifier}} ->
				NasIdentifier;
			{{ok, UserName}, _, _} ->
				UserName
		end,
		UserPasswordV = radius_attributes:find(?UserPassword, RequestAttributes),
		ChapPasswordV = radius_attributes:find(?ChapPassword, RequestAttributes),
		StateV = radius_attributes:find(?State, RequestAttributes),
		case {UserPasswordV, ChapPasswordV, StateV} of
			{error, error, error} ->
				throw(reject);
			{error, error, {ok, _State}} ->
				% @todo Handle State?
				throw(not_implemented);
			{{ok, UserPassword}, error, _State} ->
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
			{error, {ok, {ChapId, ChapResponse}}, _State} ->
				Challenge = case radius_attributes:find(?ChapChallenge,
						RequestAttributes) of
					{ok, ChapChallenge} ->
						ChapChallenge;
					error ->
						Authenticator
				end,
				case radius_example:find_user(Name) of
					{ok, Password, UserAttributes} ->
						case binary_to_list(erlang:md5([ChapId, Password,
								Challenge])) of
							ChapResponse ->
								accept(Id, Authenticator, Secret, UserAttributes);
							_ ->
								reject(Packet, Secret)
						end;
					error ->
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

-spec terminate(Reason :: term()) -> ok.
%% @doc This callback function is called just before the server exits.
%%
terminate(_Reason) ->
	ok.

%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

-spec reject(Request :: binary(), Secret :: string()) ->
	AccessReject :: binary().
%% @hidden
reject(<<_Code, Id, _Len:16, Authenticator:16/binary, _/binary>>, Secret) ->
	Attributes = [],
	Length = length(Attributes) + 20,
	ResponseAuthenticator = erlang:md5([<<?AccessReject, Id, Length:16>>,
			Authenticator, Secret]), 
	Response = #radius{code = ?AccessReject, id = Id,
			authenticator = ResponseAuthenticator, attributes = []},
	radius:codec(Response).

-spec accept(Id :: byte(), RequestAuthenticator :: [byte()],
		Secret :: string(), Attributes :: binary() | [byte()]) ->
	AccessAccept :: binary().
%% @hidden
accept(Id, RequestAuthenticator, Secret, AttributeList)
		when is_list(AttributeList) -> 
	Attributes = radius_attributes:codec(AttributeList),
	accept(Id, RequestAuthenticator, Secret, Attributes);
accept(Id, RequestAuthenticator, Secret, ResponseAttributes) 
		when is_binary(ResponseAttributes) -> 
	Length = size(ResponseAttributes) + 20,
	ResponseAuthenticator = erlang:md5([<<?AccessAccept, Id, Length:16>>,
			RequestAuthenticator, ResponseAttributes, Secret]), 
	Response = #radius{code = ?AccessAccept, id = Id,
			authenticator = ResponseAuthenticator,
			attributes = ResponseAttributes},
	radius:codec(Response).

