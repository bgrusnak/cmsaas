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
-module(cmsaas_cache).

-behaviour(gen_server). 

% includes
-include("cmsaas.hrl").

%% API
-export([start_link/0, gen_key/2, get/2, set/3, set/4, remove/2, set_tag/3, remove_tag/3, get_tag/2, get_tags/2, flush_tag/2, clear_tagged/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

% value set description
-record(cached, {value, tags=[]}).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	Config=cmsaas_conf:lookup('@'),
	Host = treelists:get("memcached.host", Config),
	Port = treelists:get("memcached.port", Config),
	mcd:start_link(localmcd, [binary:bin_to_list(Host), Port]),
	{ok, {}}.

gen_key(List, Sym) ->
	list_to_atom(string:join(List, Sym))
.

make_tag(Domain, Tag) ->
	gen_key(["#tags", Domain, Tag], ":")
.

% add name key into tag list 
% tags are located with #tags parent key
add_key_in_tag(Domain, Tag, Key) ->
	Keytag=make_tag(Domain, Tag),
	Keys=case mcd:get(localmcd, Keytag) of
		{error,notfound} -> [Key];
		{ok, X} -> lists:merge(X, [Key]);
		Y -> lists:merge(Y, [Key])
	end,
	Skeys=lists:sort(Keys),
	{Uskeys, _} = lists:mapfoldl(fun(A, Acc) -> { case A==Acc of true->undefined; false->A end,A} end, "", Skeys),
	Fskeys=lists:filter(fun(A) -> A/=undefined end, Uskeys),
	mcd:set(localmcd, Keytag, Fskeys)
.

remove_key_from_tag(Tag, Key) ->
	[Domain|_]=	string:tokens(Key, ":"),
	Keytag=make_tag(Domain, Tag),
	Keys=case mcd:get(localmcd, Keytag) of 
		{error,notfound} -> [];
		{ok, X} -> X;
		Y -> Y		
	end,
	mcd:set(localmcd, Keytag, lists:delete(Key, Keys))
.

remove_key_from_tag(Domain, Tag, Key) ->
	Keytag=make_tag(Domain, Tag),
	Keys=case mcd:get(localmcd, Keytag) of 
		{error,notfound} -> [];
		{ok, X} -> X;
		Y -> Y		
	end,
	mcd:set(localmcd, Keytag, lists:delete(Key, Keys))
.

read(Key) ->
	case mcd:get(localmcd, Key) of
		{error,notfound} -> undefined;
		{badrecord,cached} -> undefined;
		{ok, X} -> X;
		Y ->Y
	end
.

read(Domain, Name) ->
	case mcd:get(localmcd, gen_key([Domain, Name], ":")) of
		{error,notfound} -> undefined;
		{badrecord,cached} -> undefined;
		{ok, X} -> X;
		Y ->Y
	end
.

insert(Domain, Name, Value, Tags) ->
    insert(Domain, Name, Value, Tags, 0)
.

insert(Domain, Name, Value, Tags, Expiration) ->
	case read(Domain, Name) of
		undefined -> undefined;
		_ ->delete(Domain, Name)
	end,
	Key=gen_key([Domain, Name], ":"),
	Val=#cached{value=Value, tags=Tags},
	mcd:set(localmcd, Key, Val, 0, Expiration),
	lists:map(fun(X) -> add_key_in_tag(Domain, X, Key), X end, Tags),
	Value
.

delete(Key) ->
	Val=read(Key),
	mcd:do(localmcd, delete, Key),
	case Val of
		undefined -> undefined;
		_ ->
			lists:map(fun(X) -> remove_key_from_tag(X, Key) end, Val#cached.tags),
			Val#cached.value
	end
.

delete(Domain, Name) ->
	Key=gen_key([Domain, Name], ":"),
	mcd:do(localmcd, delete, Key),
	Val=read(Domain, Name),
	case Val of
		undefined -> undefined;
		_ ->
			lists:map(fun(X) -> remove_key_from_tag(Domain, X, Key) end, Val#cached.tags),
			Val#cached.value
	end
.

handle_call({getval, Domain, Name}, _From, State) ->
	Val=read(Domain, Name),
	{reply, case Val of undefined->undefined; _ ->Val#cached.value end, State}
;

handle_call({gettags, Domain, Name}, _From, State) ->
	Val=read(Domain, Name),
	{reply, case Val of undefined->[]; _ ->Val#cached.tags end, State}
;

handle_call({settag, Domain, Name, _Tags}, _From, State) ->
	Tags_l=case is_atom(_Tags) of true -> [_Tags]; _ -> _Tags end,
	Key=gen_key([Domain, Name], ":"),
% add name into tags
	case Tags_l of
		[undefined] -> Tags=[];
		_ -> lists:map(fun(X) -> add_key_in_tag(Domain, X, Key) end, Tags_l), Tags=Tags_l
	end,
% add tag into list of tags
	Val=case mcd:get(localmcd, Key) of
		{error,notfound} -> #cached{};
		{ok, X} -> X;
		Y -> Y
	end,
	Val2=#cached{value=Val#cached.value, tags=lists:merge(Val#cached.tags, Tags)},
	mcd:set(localmcd, Key, Val2),
	{reply, Val2, State}
;

handle_call({removetag, Domain, Tag}, _From, State) ->
% get names list for tag
	Nametag=gen_key(["#tags", Domain, Tag], ":"),
	Names= case mcd:get(localmcd, Nametag) of
		{error,notfound} -> [];
		{ok, A} -> A;
		B -> B		
	end,
	lists:map(fun(X) -> Val=case mcd:get(localmcd, X) of 	{error,notfound} -> #cached{}; {ok, X} -> X;	Y -> Y end, mcd:set(localmcd, X, #cached{value=Val#cached.value, tags=lists:delete(Tag, Val#cached.tags)}) end, Names),
	mcd:do(localmcd, delete, Nametag),
	{reply, ok, State}
;

handle_call({gettag, Domain, Tag}, _From, State) ->
	Keytag=make_tag(Domain, Tag),
	{reply, case mcd:get(localmcd, Keytag) of {error,notfound} -> []; {ok, X} -> X; Y -> Y end, State}
;

handle_call({flushtag, Domain, Tag}, _From, State) ->
	Keytag=make_tag(Domain, Tag),
	Keys=case mcd:get(localmcd, Keytag) of
		{error,notfound} -> [];
		{ok, X} -> X;
		Y -> Y		
	end,
	lists:map(fun(X) -> mcd:do(localmcd, delete, X) end, Keys),
	mcd:do(localmcd, delete, Keytag),
	{reply, ok, State}
;

handle_call({cleartagged, Domain, Tag}, _From, State) ->
	Keytag=make_tag(Domain, Tag),
	Keys=case mcd:get(localmcd, Keytag) of
		{error,notfound} -> [];
		{ok, X} -> X;
		Y -> Y		
	end,
%	lists:map(fun(X) -> mcd:do(localmcd, delete, gen_key([Domain, X], ":")) end, Keys),
	lists:map(fun(X) -> delete(X) end, Keys),
	mcd:do(localmcd, delete, Keytag),
	{reply, ok, State}
;



handle_call({setval, Domain, Name, Value, Tags}, _From, State) ->
	{reply, insert(Domain, Name, Value, Tags), State}
;

handle_call({remval, Domain, Name}, _From, State) ->

	{reply, delete(Domain, Name), State}
.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

get(Domain, Name) ->
	gen_server:call(?SERVER, {getval, Domain, Name}).

get_tags(Domain, Name) ->
	gen_server:call(?SERVER, {gettags, Domain, Name}).

set(Domain, Name, Value) ->
	gen_server:call(?SERVER, {setval, Domain, Name, Value, []}).

set(Domain, Name, Value, Tags) ->
	gen_server:call(?SERVER, {setval, Domain, Name, Value, Tags}).

set_tag(Domain, Name, Tag) ->
	gen_server:call(?SERVER, {settag, Domain, Name, Tag}).

remove_tag(Domain, Name, Tag) ->
	gen_server:call(?SERVER, {removetag, Domain, Name, Tag}).

flush_tag(Domain, Tag) ->
	gen_server:call(?SERVER, {flushtag, Domain, Tag}).

get_tag(Domain, Tag) ->
	gen_server:call(?SERVER, {gettag, Domain, Tag}).

remove(Domain, Name) ->
	gen_server:call(?SERVER, {remval, Domain, Name}).

clear_tagged(Domain, Name) ->
	gen_server:call(?SERVER, {cleartagged, Domain, Name}).

