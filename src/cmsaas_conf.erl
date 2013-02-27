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

% This module is used for the accepting of the configurations from the /priv/config directory.
% All data, not passed in the config file, will be taken from "@" config (common)

-module(cmsaas_conf).

-behaviour(gen_server).

% includes
-include("cmsaas.hrl").



%% API
-export([start_link/0, read_conf/0, get_conf/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-export([lookup/1, update/3]).

-define(SERVER, ?MODULE).

-define(CONFIGPATH, "./priv/config").

-record(state, {conf, current}).

% TODO
% 1) Make a lazy config loading - only when domain is needed
% 2) Make a config unloading - unload all configs, unused in last time  

%  
%  name: read_conf
%  @param
%  @return list of all configurations from all files
%  
read_conf() ->
	lists:map(fun(Name) -> read_conf(Name) end, filelib:wildcard(filename:join(?CONFIGPATH, "*"))).

%  
%  name: read_conf
%  @param Name
%  @return {basename, sorted_list}
%  

read_conf(Name) ->
	Rt = file:consult(Name),
	Confdata=case Rt of
		{ok, Val} ->
			Val;
		{error, _} ->
			[]
	end,
	{filename:basename(Name), lists:keysort(1,Confdata)}.

%  Get value from the config
%  name: get_conf
%  @param Conf, Name
%  @return config value
%  

get_conf(Conf, Name) ->
	{_, Global} =lists:keyfind("@", 1, Conf),
		case lists:keyfind(Name, 1, Conf) of
			false ->
				Global;
			{_, Config} ->
				Current= case treelists:get("host.parent", Config) of 
					undefined -> Config;
					Parent ->	
						treelists:merge(get_conf(Conf, Parent), Config)
				end,
				treelists:merge(Global, Current)
		end
.


start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	Conf = read_conf(),
	{ok, #state{conf=Conf}}.

handle_call({lookup, Name}, _From, State) ->
	Reply = get_conf(State#state.conf, Name),
	{reply, Reply, State};

handle_call({update, {Name, Key, Value}}, _From, State) ->
	{ _, NewConfItem} = lists:keyfind(Name, 1, State#state.conf),
	UpdConfItem = case NewConfItem of
	false ->
			treelists:set(Key, Value);
	_ ->
		treelists:set(Key, Value, NewConfItem)
	end,
	NewConf = if NewConfItem  == false ->
			lists:keymerge(1, State#state.conf, [{Name, UpdConfItem}])
		;
		true ->
			lists:keyreplace(Name, 1, State#state.conf, {Name, UpdConfItem})
	end,
	file:write_file(filename:join( ?CONFIGPATH , Name), lists:foldl(fun(X, Sum) -> Sum ++ io_lib:print(X) ++ ".\n" end, "", UpdConfItem)),
	Reply = ok,
	{reply, Reply, State#state{conf=NewConf}};


handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

lookup(Name) ->
	gen_server:call(?SERVER, {lookup, Name}).

update(Name, Key, Value) ->
	gen_server:call(?SERVER, {update, {Name, Key, Value}}).
