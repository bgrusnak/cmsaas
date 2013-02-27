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

-module(mofun).

% includes
-include("cmsaas.hrl").

-export([
	insert/3,
	update/4,
	update/5,
	update/6,
	delete/3,
	delete_one/3,
	find_one/3,
	find_one/4,
	find_one/5,
	find/3,
	find/4,
	find/5,
	find/6,
	count/3,
	count/4,
	ensure_index/3,
	command/2,
	build_conditions/1,
	build_conditions/2,
	proplist_to_tuple/1,
	tuple_to_proplist/1,
	dec2hex/1,
	hex2dec/1
]).

insert(Dbconfig, Coll, Doc) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:insert(Coll, Doc) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

update(Dbconfig, Coll, Selector, Doc) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:update(Coll, Selector, Doc) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

update(Dbconfig, Coll, Selector, Doc, Upsert) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:update(Coll, Selector, Doc, Upsert) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

update(Dbconfig, Coll, Selector, Doc, Upsert, MultiUpdate) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:update(Coll, Selector, Doc, Upsert, MultiUpdate) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

delete(Dbconfig, Collection, Selector) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:delete(Collection, Selector) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

delete_one(Dbconfig, Collection, Selector) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:delete_one(Collection, Selector) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.


find_one(Dbconfig, Collection, Selector) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:find_one(Collection, Selector) end) of
		{} -> {};
		{X} ->X ;
		X -> X
	end,
	Rt
.

find_one(Dbconfig, Collection, Selector, Projector) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:find_one(Collection, Selector, Projector) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

find_one(Dbconfig, Collection, Selector, Projector, Skip) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:find_one(Collection, Selector, Projector, Skip) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

find(Dbconfig, Collection, Selector) ->
	mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:count(Collection, Selector),
		Cursor = mongo:find(Collection, Selector),
        Result = mongo_cursor:rest(Cursor),
        mongo_cursor:close(Cursor),
        Result
	end)
.

find(Dbconfig, Collection, Selector, Projector) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:find(Collection, Selector, Projector) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

find(Dbconfig, Collection, Selector, Projector, Skip) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:find(Collection, Selector, Projector, Skip) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

find(Dbconfig, Collection, Selector, Projector, Skip, BatchSize) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:find(Collection, Selector, Projector, Skip, BatchSize) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

count(Dbconfig, Collection, Selector) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:count(Collection, Selector) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

count(Dbconfig, Collection, Selector, Limit) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:count(Collection, Selector, Limit) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

ensure_index(Dbconfig, Collection, IndexSpec) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:ensure_index(Collection, IndexSpec) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

command(Dbconfig, Command) ->
	 Rt=case mongo:do(safe, master, Dbconfig#dbconfig.connector, Dbconfig#dbconfig.database, fun() ->
		mongo:command(Command) end) of
		{} -> {};
		{X} -> X;
		X -> X
	end,
	Rt
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functions below based on <https://github.com/evanmiller/boss_db>
%%
%% Copyright (c) 2012 Evan Miller
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Number of seconds between beginning of gregorian calendar and 1970
-define(GREGORIAN_SECONDS_1970, 62167219200).

% JavaScript expression formats to query MongoDB
-define(CONTAINS_FORMAT, "this.~s.indexOf('~s') != -1").
-define(NOT_CONTAINS_FORMAT, "this.~s.indexOf('~s') == -1").


%%
%% Query generation
%%

build_conditions(Conditions) ->
    proplist_to_tuple(build_conditions1(Conditions, [])).

build_conditions(Conditions, OrderBy) ->
    {'query', build_conditions(Conditions), orderby, OrderBy}.

build_conditions1([], Acc) ->
    Acc;

build_conditions1([{Key, Operator, Value}|Rest], Acc) ->

    Condition = case {Operator, Value} of
        {'not_matches', "*"++Value} ->
            [{Key, {'$not', {regex, list_to_binary(Value), <<"i">>}}}];
        {'not_matches', Value} ->
            [{Key, {'$not', {regex, list_to_binary(Value), <<"">>}}}];
        {'matches', "*"++Value} ->
            [{Key, {regex, list_to_binary(Value), <<"i">>}}];
        {'matches', Value} ->
            [{Key, {regex, list_to_binary(Value), <<"">>}}];
        {'contains', Value} ->
            WhereClause = where_clause(
                ?CONTAINS_FORMAT, [Key, Value]),
            [{'$where', WhereClause}];
        {'not_contains', Value} ->
            WhereClause = where_clause(
                ?NOT_CONTAINS_FORMAT, [Key, Value]),
            [{'$where', WhereClause}];
        {'contains_all', ValueList} ->
            WhereClause = multiple_where_clauses(
                ?CONTAINS_FORMAT, Key, ValueList, "&&"),
            [{'$where', WhereClause}];
        {'not_contains_all', ValueList} ->
            WhereClause = "!(" ++ multiple_where_clauses_string(
                ?CONTAINS_FORMAT, Key, ValueList, "&&") ++ ")",
            [{'$where', erlang:iolist_to_binary(WhereClause)}];
        {'contains_any', ValueList} ->
            WhereClause = multiple_where_clauses(
                ?CONTAINS_FORMAT, Key, ValueList, "||"),
            [{'$where', WhereClause}];
        {'contains_none', ValueList} ->
            WhereClause = multiple_where_clauses(
                ?NOT_CONTAINS_FORMAT, Key, ValueList, "&&"),
            [{'$where', WhereClause}];
        {'equals', Value} when is_list(Value) ->
				[{Key, list_to_binary(Value)}];
        {'not_equals', Value} when is_list(Value) ->
            [{Key, {'$ne', list_to_binary(Value)}}];
        {'equals', {{_,_,_},{_,_,_}} = Value} ->
            [{Key, datetime_to_now(Value)}];
        {'equals', Value} ->
            [{Key, Value}];
        {Operator, {{_,_,_},{_,_,_}} = Value} ->
            [{Key, {boss_to_mongo_op(Operator), datetime_to_now(Value)}}];
        {'in', [H|T]} ->
            [{Key, {'$in', lists:map(list_pack_function(Key), [H|T])}}];
        {'in', {Min, Max}} ->
            [{Key, {'$gte', Min}}, {Key, {'$lte', Max}}];
        {'not_in', [H|T]} ->
            [{Key, {'$nin', lists:map(list_pack_function(Key), [H|T])}}];
        {'not_in', {Min, Max}} ->
            [{'$or', [{Key, {'$lt', Min}}, {Key, {'$gt', Max}}]}];
        {Operator, Value} ->
            [{Key, {boss_to_mongo_op(Operator), Value}}]
    end,
    build_conditions1(Rest, lists:append(Condition, Acc)).

list_pack_function(Key) ->
	fun(Value) when is_list(Value) ->
			list_to_binary(Value);
	   (Value) ->
			Value
    end.

where_clause(Format, Params) ->
    erlang:iolist_to_binary(
                io_lib:format(Format, Params)).

multiple_where_clauses_string(Format, Key, ValueList, Operator) ->
    ClauseList = lists:map(fun(Value) ->
                lists:flatten(io_lib:format(Format, [Key, Value]))
        end, ValueList),
        string:join(ClauseList, " " ++ Operator ++ " ").

multiple_where_clauses(Format, Key, ValueList, Operator) ->
    erlang:iolist_to_binary(multiple_where_clauses_string(Format, Key,
            ValueList, Operator)).


% Value conversions
pack_value({{_, _, _}, {_, _, _}} = Val) ->
    datetime_to_now(Val);
pack_value(V) when is_binary(V) -> pack_value(binary_to_list(V));
pack_value([H|T]) when is_integer(H) -> list_to_binary([H|T]);
pack_value({integers, List}) -> List;
pack_value(V) -> V.

unpack_value(_AttrName, [H|T], _ValueType) when is_integer(H) ->
    {integers, [H|T]};
unpack_value(AttrName, Value, ValueType) ->
            boss_record_lib:convert_value_to_type(Value, ValueType)
    .

id_type_from_foreign_key(ForeignKey) ->
    Tokens = string:tokens(atom_to_list(ForeignKey), "_"),
    NameTokens = lists:filter(fun(Token) -> Token =/= "id" end,
        Tokens),
    string:join(NameTokens, "_").


% Operators
boss_to_mongo_op('not_equals') -> '$ne';
boss_to_mongo_op('gt') -> '$gt';
boss_to_mongo_op('ge') -> '$gte';
boss_to_mongo_op('lt') -> '$lt';
boss_to_mongo_op('le') -> '$lte';
boss_to_mongo_op('in') -> '$in';
boss_to_mongo_op('not_in') -> '$nin'.


% Sort clauses
pack_sort_order(ascending) -> 1;
pack_sort_order(descending) -> -1.


%%
%% Generic data structure conversions
%%

% The mongodb driver uses "associative tuples" which look like:
% {key1, Value1, key2, Value2}
tuple_to_proplist(Tuple) ->
    List = tuple_to_list(Tuple),
    Ret = lists:reverse(list_to_proplist(List, [])),
    Ret.

proplist_to_tuple(PropList) ->
    ListOfLists = lists:reverse([[K,V]||{K,V} <- PropList]),
    list_to_tuple(lists:foldl(
            fun([K, V], Acc) ->
                    [K,V|Acc]
            end, [], ListOfLists)).

list_to_proplist([], Acc) -> Acc;
list_to_proplist([K,V|T], Acc) ->
    list_to_proplist(T, [{K, V}|Acc]).

datetime_to_now(DateTime) ->
    GSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    ESeconds = GSeconds - ?GREGORIAN_SECONDS_1970,
    {ESeconds div 1000000, ESeconds rem 1000000, 0}.


%%
%% Decimal to hexadecimal conversion
%%
%% Functions below copied from emongo <https://github.com/boorad/emongo>
%% 
%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com> 
%% Jacob Perkins <japerk@gmail.com> 
%% Belyaev Dmitry <rumata-estor@nm.ru> 
%% François de Metz <fdemetz@af83.com>
%%

dec2hex(Dec) when is_list(Dec) ->
	case string:to_integer(Dec) of
		{error, _} -> "";
		{Val, _} -> dec2hex(<<>>, Val)
	end
;

dec2hex(Dec) ->
    dec2hex(<<>>, Dec).

dec2hex(N, <<I:8,Rem/binary>>) ->
    dec2hex(<<N/binary, (hex0((I band 16#f0) bsr 4)):8, (hex0((I band 16#0f))):8>>, Rem);
dec2hex(N,<<>>) ->
    N.

hex2dec(Hex) when is_list(Hex) ->
    hex2dec(list_to_binary(Hex));

hex2dec(Hex) ->
    hex2dec(<<>>, Hex).

hex2dec(N,<<A:8,B:8,Rem/binary>>) ->
    hex2dec(<<N/binary, ((dec0(A) bsl 4) + dec0(B)):8>>, Rem);
hex2dec(N,<<>>) ->
    N.

dec0($a) -> 10;
dec0($b) -> 11;
dec0($c) -> 12;
dec0($d) -> 13;
dec0($e) -> 14;
dec0($f) -> 15;
dec0(X) -> X - $0.

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 + I.  
