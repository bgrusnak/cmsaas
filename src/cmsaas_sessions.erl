% ==========================================================================================================
% CMSaaS - Content Management System as a Service
%
% @copyright 2013 BulgaRus BG LTD
% @author Ilya Shlyakhovoy  <info@cmsaas.info>
% @version 0.3
% All rights reserved.
%
% CC-Attribution License
%
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================

-module(cmsaas_sessions).

-behaviour(gen_server).

% includes
-include("cmsaas.hrl").

%% API
-export([start_link/0, create/2, get/3, set/4, delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
	{ok, {}}.

make_session_rec(Host, Session) ->
	"sessions:"++ binary:bin_to_list(Host) ++":"++Session
.

findsession(Dbconfig, Table, SessionId) ->
	case mofun:find_one(Dbconfig, Table, {'session', SessionId}) of
		{} -> undefined;
		Res -> Res
	end
.

createsession(Host, SessionId) ->
% acquire access
	Config=cmsaas_conf:lookup(Host),
	Dbaccess=cmsaas_db:get_db(Config),
	CSessions = 'core.sessions',
	Expiration = treelists:get("memcached.expiration", Config),
	SessRec=make_session_rec(Host, SessionId),
% check, does session was cached
	case cmsaas_cache:get(Host, SessRec) of
% no cached session
		undefined ->
% check, does session was created before
			case findsession(Dbaccess#dbaccess.db#dbset.content, CSessions, SessionId) of
				undefined ->
% make a new session and write it to the DB
					mofun:insert(Dbaccess#dbaccess.db#dbset.content, CSessions,  {session, SessionId, accessed, erlang:universaltime(), user, "", data, {}}),
					User=undefined,
					Dt=#session{accessed= erlang:universaltime(), session=SessionId, user=User, data=[]}
				;
				Session_res ->
% change last access time for the session and write it back to the DB
					{User}=bson:lookup(user, Session_res),
					{UserData}=bson:lookup(data, Session_res),
					mofun:update(Dbaccess#dbaccess.db#dbset.content, CSessions, {session, SessionId}, {session, SessionId, accessed, erlang:universaltime(), user, User, data, mofun:proplist_to_tuple(UserData)}),
					Dt=#session{accessed= erlang:universaltime(), session=SessionId, user=User, data=UserData}
					
			end,
% store session in the cache with expiration, defined in config
			cmsaas_cache:set(Host, SessRec, Dt, [Host, sessions, user, User], Expiration)
		;
		Cached -> 
% make a new session from cached with current time and write it to the DB
			Dt=#session{accessed= erlang:universaltime(), session=SessionId, user=Cached#session.user, data= Cached#session.data},
			mofun:update(Dbaccess#dbaccess.db#dbset.content, CSessions, {session, SessionId}, {session, SessionId, accessed, erlang:universaltime(), user, Cached#session.user, data, mofun:proplist_to_tuple(Cached#session.data)}),
% store session in the cache with expiration, defined in config
			cmsaas_cache:set(Host, SessRec, Dt, [Host, sessions, user, Cached#session.user], Expiration)
	end
.

deletesession(Host, SessionId) ->
% acquire access
	Config=cmsaas_conf:lookup(Host),
	Dbaccess=cmsaas_db:get_db(Config),
	CSessions = 'core.sessions',
	SessRec=make_session_rec(Host, SessionId),
% check, does session was cached
	case cmsaas_cache:get(Host, SessRec) of
% no cached session
		undefined ->
			case findsession(Dbaccess#dbaccess.db#dbset.content, CSessions, SessionId) of
				undefined ->
					undefined;
				_ ->
					mofun:delete(Dbaccess#dbaccess.db#dbset.content, CSessions,  {session, SessionId})
			end
		;
		_ -> 
			mofun:delete(Dbaccess#dbaccess.db#dbset.content, CSessions,  {session, SessionId}),
			cmsaas_cache:remove(Host, SessRec)
	end
.


getvalue(Host, SessionId, Name) ->
	% acquire access
	Config=cmsaas_conf:lookup(Host),
	Dbaccess=cmsaas_db:get_db(Config),
	CSessions = 'core.sessions',
	SessRec=make_session_rec(Host, SessionId),
	Expiration = treelists:get("memcached.expiration", Config),
	% check, does session was cached
	case cmsaas_cache:get(Host, SessRec) of
	% no cached session
		undefined ->
			% check, does session was created before
			case findsession(Dbaccess#dbaccess.db#dbset.content, CSessions, SessionId) of
				undefined ->
% return undefined
					undefined
				;
				Session_res ->
% store session in the cache and in the DB with expiration, defined in config
					{User}=bson:lookup(user, Session_res),
					{UserData}=bson:lookup(data, Session_res),
					Dt=#session{accessed= erlang:universaltime(), session=SessionId, user=User, data=UserData},
					cmsaas_cache:set(Host, SessRec, Dt, [Host, sessions, user, User], Expiration),
					mofun:update(Dbaccess#dbaccess.db#dbset.content, CSessions, {session, SessionId}, {session, SessionId, accessed, erlang:universaltime(), user, User, data, mofun:proplist_to_tuple(UserData)}),
					case Name of 
						user -> User;
						userid -> User;
						_ -> 
							case lists:keyfind(Name, 1, UserData) of
								false ->
									undefined
								;
								{_, Val} -> Val
							end
					end
			end
		;
		Cached -> 
% make a new session from cached with current time and write it to the DB
			Dt=#session{accessed= erlang:universaltime(), session=SessionId, user=Cached#session.user, data= Cached#session.data},
			mofun:update(Dbaccess#dbaccess.db#dbset.content, CSessions, {session, SessionId}, {session, SessionId, accessed, erlang:universaltime(), user, Cached#session.user, data, mofun:proplist_to_tuple(Cached#session.data)}),
% store session in the cache with expiration, defined in config
			cmsaas_cache:set(Host, SessRec, Dt, [Host, sessions, user, Cached#session.user], Expiration),
			case Name of 
				user -> Cached#session.user;
				userid -> Cached#session.user;
				_ -> 
					case lists:keyfind(Name, 1, Cached#session.data) of
						false ->
							undefined
						;
						{_, Val} -> Val
					end
			end
	end
.

setvalue(Host, SessionId, Name, Value) ->
	% acquire access
	Config=cmsaas_conf:lookup(Host),
	Dbaccess=cmsaas_db:get_db(Config),
	CSessions = 'core.sessions',
	SessRec=make_session_rec(Host, SessionId),
	Expiration = treelists:get("memcached.expiration", Config),
% check, does session was cached
	case cmsaas_cache:get(Host, SessRec) of
	% no cached session
		undefined ->
			% check, does session was created before
			case findsession(Dbaccess#dbaccess.db#dbset.content, CSessions, SessionId) of
				undefined ->
% return undefined
					undefined
				;
				Session_res ->
% store session in the cache and in the DB with expiration, defined in config
					{User}=bson:lookup(user, Session_res),
					{UserData}=bson:lookup(data, Session_res),
					Newdata=case Name of 
						user -> UserData;
						userid -> UserData;
						_ -> 
							case lists:keyfind(Name, 1, UserData) of
								false ->
									lists:merge(UserData, [{Name, Value}])
								;
								{_, _} -> lists:keyreplace(Name, 1, UserData, {Name, Value})
							end
					end,
					NewUser= case Name of
						user -> Value;
						userid -> Value;
						_ -> User
					end,
					Dt=#session{accessed= erlang:universaltime(), session=SessionId, user=NewUser, data=Newdata},
					cmsaas_cache:set(Host, SessRec, Dt, [Host, sessions, user, NewUser], Expiration),
					mofun:update(Dbaccess#dbaccess.db#dbset.content, CSessions, {session, SessionId}, {session, SessionId, accessed, erlang:universaltime(), user, NewUser, data, mofun:proplist_to_tuple(Newdata)})
			end
		;
		Cached -> 
% make a new session from cached with current time and write it to the DB
			Newdata=case Name of 
				user -> Cached#session.data;
				userid -> Cached#session.data;
				_ -> 
					case lists:keyfind(Name, 1, Cached#session.data) of
						false ->
							lists:merge(Cached#session.data, [{Name, Value}])
						;
						{_, _} -> lists:keyreplace(Name, 1, Cached#session.data, {Name, Value})
					end
			end,
			NewUser= case Name of
				user -> Value;
				userid -> Value;
				_ -> Cached#session.user
			end,
			Dt=#session{accessed= erlang:universaltime(), session=SessionId, user=NewUser, data= Newdata},
			mofun:update(Dbaccess#dbaccess.db#dbset.content, CSessions, {session, SessionId}, {session, SessionId, accessed, erlang:universaltime(), user, NewUser, data, mofun:proplist_to_tuple(Newdata)}),
% store session in the cache with expiration, defined in config
			cmsaas_cache:set(Host, SessRec, Dt, [Host, sessions, user, NewUser], Expiration)			
	end
.




handle_call({create, Host, SessionId}, _From, State) ->
	createsession(Host, SessionId),
	{reply, ok, State}
;

handle_call({get, Host, SessionId, Name}, _From, State) ->
	Ret=getvalue(Host, SessionId, Name),
	{reply, Ret, State}
;

handle_call({set, Host, SessionId, Name, Value}, _From, State) ->
	setvalue(Host, SessionId, Name, Value),
	{reply, ok, State}
;

handle_call({delete, Host, SessionId}, _From, State) ->
	deletesession(Host, SessionId),
	{reply, ok, State}
.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

create(Host, SessionId) ->
	gen_server:call(?SERVER, {create, Host, SessionId}).

get(Host, SessionId, Name) ->
	gen_server:call(?SERVER, {get, Host, SessionId, Name}).

set(Host, SessionId, Name, Value) ->
	gen_server:call(?SERVER, {set, Host, SessionId, Name, Value}).

delete(Host, SessionId) ->
	gen_server:call(?SERVER, {delete, Host, SessionId}).
