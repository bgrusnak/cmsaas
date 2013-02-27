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
%@doc Module for run of chains of service callbacks. Calback function must return tuple with first element from set [error, ok]. Pass - if all ok, but next callback must be applied. Error - if problem was occured, but next callback must be applied. Ok - if all ok, and chain must be finished.
%@doc Callbacks are defined in the directory "callback" in files, which name, consist of numbers, are representing runlevel of callbacks.
%@doc In each file can be only one callback processing function for each callback.
%@doc I.e. if we have three functions for one callback, it must defined in three files - "10", "20", "30", for example.

-module(cmsaas_chaingenerator).

% includes
-include("cmsaas.hrl").

%% API
-export([generate/0, generate/1, add/4, delete/4, get_chain/2, read_callbacks/0]).


get_chain(Callbacks, Name) ->
	lists:append(get_subchain(Callbacks, Name))
.

get_subchain([], _Name) -> [] ;
get_subchain([H|T], Name) ->
	{_, Nl }  = H,
	Found=lists:filter(fun(X) -> {Callback, _, _}=X, Name==Callback end, Nl),
	lists:append([Found], get_subchain(T, Name))
.

add(Event, Runlevel, Module, Function) ->
	add_chain(read_callbacks(), Event, Runlevel, Module, Function)
.

delete(Event, Runlevel, Module, Function) ->
	del_chain(read_callbacks(), Event, Runlevel, Module, Function)
.

generate() ->
	generate(read_callbacks())
.

make_chain_call([]) ->
	{<<"">>, <<"">>}
;

make_chain_call(Chain) ->
	{Action, _, _} = lists:nth(1, Chain),
	Callstring=io_lib:format("call(Callback, Data) when Callback == ~p ->\n\tchain_~p(Data, 1)\n", [Action, Action]),
	{Subcalls, _}=lists:mapfoldl(fun(X, A) -> make_subchain_call(X, {A, length(Chain)}) end, 1, Chain),
	Folds=string:join(Subcalls, ";\n") ++ ".\n",
	{Callstring, Folds}
.

make_subchain_call(El, {Pos, Len}) ->
	{Action, Module, Function} = El,
	Out = if 
		(Pos<Len) ->
			io_lib:format("chain_~p(Data, Order) when Order==~w ->\n\tRet=~p:~p(Data),\n\tcase Ret of\n\t\t{error, Newdata} ->\n\t\t\tchain_~p(Newdata, Order+1);\n\t\t{pass, Newdata} ->\n\t\t\tchain_~p(Newdata, Order+1);\n\t\t{ok, _} ->\n\t\t\tRet\n\tend\n",[Action, Pos, Module, Function, Action, Action]);
		true ->
			io_lib:format("chain_~p(Data, Order) when Order==~w ->\n\t~p:~p(Data)\n",[Action, Pos, Module, Function])
	end,
	{Out, Pos+1}
.  

generate(Callbacks) ->
	Flatcalls=lists:flatten(lists:map(fun(X) -> {_Rl, Ls } = X, Ls end, Callbacks)),
	All=lists:foldl(fun(X, Acc) -> {Action, _M, _F}=X, case lists:keyfind(Action, 1, Acc) of {Action} -> Acc; false -> lists:merge(Acc, [{Action}]) end end, [], Flatcalls),
	Chain="% This module is automatically generated. \n-module(" ++ ?CALLBACK_MODULE ++ ").\n-export([call/2]).\n",
	{Subs, Calls}=lists:mapfoldl(fun(X, Acc) -> {Cname} = X, Clist=get_chain(Callbacks, Cname), {Call, Sub} = make_chain_call(Clist), {Sub, lists:merge(Acc, [Call])} end, [], All),
	Filetext=binary:list_to_bin(Chain ++ string:join(Subs, "\n") ++ string:join(Calls, ";\n") ++  ";\ncall(_Callback, Data) ->\n\tData\n."),
	file:write_file(filename:join("./src", ?CALLBACK_MODULE ++ ".erl"), Filetext, [binary]),
	compile:file(filename:join("./src", ?CALLBACK_MODULE), [{outdir,"./ebin"}])
.

add_chain(Callbacks, Event, Runlevel, Module, Function) ->
	Chain=lists:keyfind(Runlevel, 1, Callbacks),
	if 
		Chain == false ->
			Newchain=[{Event, Module, Function}],
			Newlist=lists:append(Callbacks, [{Runlevel, Newchain}]);
		true ->
			{_, Nc } = Chain, 
			Newchain=lists:append(Nc, [{Event, Module, Function}]),
			Newlist=lists:keyreplace(Runlevel, 1, Callbacks, {Runlevel, Newchain})
	end,
	file:write_file(filename:join(?CALLBACK_PATH, integer_to_list(Runlevel)), lists:foldl(fun(X, Sum) -> Sum ++ io_lib:print(X) ++ ".\n" end, "", Newchain)),
	generate(Newlist)
.

del_chain(Callbacks, Event, Runlevel, Module, Function) ->
	Chain=lists:keyfind(Runlevel, 1, Callbacks),
	if 
		Chain == false ->
			ok;
		true ->
			{_, Nc } = Chain,
			Newchain=lists:filter(fun(X) -> {Cevent, Cmodule, Cfunction} = X, (Cevent /= Event) or (Cmodule /= Module) or (Cfunction /=Function) end, Nc), 
			file:write_file(filename:join(?CALLBACK_PATH, integer_to_list(Runlevel)), lists:foldl(fun(X, Sum) -> Sum ++ io_lib:print(X) ++ ".\n" end, "", Newchain)),
			Newlist=lists:keyreplace(Runlevel, 1, Callbacks, {Runlevel, Newchain}),
io:format("~p~n", [Newlist]),			
			generate(Newlist)
	end
.

read_callbacks() ->
	lists:keysort(1, lists:map(fun(Name) -> {ok, Chain} = file:consult(Name), {list_to_integer(filename:basename(Name)), Chain} end, filelib:wildcard(filename:join(?CALLBACK_PATH , "*"))))
.
