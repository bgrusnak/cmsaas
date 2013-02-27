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
%@doc Module for extract, delete and put data into tree lists - lists of tuples, which contains inside name and another list or data.
-module (treelists).


-export ([get/2, set/2, set/3, unset/2, merge/2]).


% check Data quality
guardList(Data) ->
% if data is a tuple with two elements, take second element and use it as a data, else return undefined
	D=case is_tuple(Data) of
	true->
		case tuple_size(Data) of
		2 ->
			element(2, Data);
		_ ->
			undefined
		end;
	_ ->
		Data
	end,
% if data length equals 0, return undefined
%	case is_list(D) and (length(D)>0) of
	case is_list(D) of
	true ->
		D;
	_ ->
		undefined
	end
.
%  Getting the variable from the tree list by the path like "my.favorite.path"
% Can be used signs * for the any number of characters and ? for the single character 
%  name: get
%  @param
%  @return
%  

get(Name, Data) ->
	Items=guardList(Data),
	if Items=:=undefined ->
		undefined;
	true ->
% 		split Name into dot-separated chunks {Chead, Tail}l
		[Chead | Tail ] = string:tokens(Name, "."),
% 		replace all "*" to ".*"
		Caster=re:replace(Chead,"\\\*",".*",[{return,list}, global]),
% 		replace all "?" to the "."
		Current=re:replace(Caster,"\\\?",".",[{return,list}, global]),
% 		extract from Data all elements, regexp-matched with Current, and put it all into Founds
		{ok, Reg} = re:compile("^" ++ Current ++ "$", [unicode, ungreedy]),
		Founds=lists:filter(fun(A) -> 
% get first item from current item as list
			{Chars, _} = A,
			re:run(atom_to_list(Chars), Reg) /= nomatch
		end, Items),
% 		if Tail_name have length > 0
		Rets=case length(Tail) of
		0 ->
			Founds;
		_ ->
% 			for each of Founds set value to getValue(Tailname, Element)
			Tailname = string:join(Tail, "."),
			lists:filter(fun(A) ->  A/=undefined end, lists:map(fun(A) -> treelists:get(Tailname, A) end, Founds))
		end,
		Last=list_to_atom(lists:last(string:tokens(Name, "."))),
		case Rets of
			[{Last, A}] ->
				A;
			[B] ->
				B;
			{_, Val} ->
				Val;
			_ ->
				undefined
		end
	end
.

set(Name, Value) ->
	set(Name, Value, [])
.

set(Name, Value, Data) ->
% split Name into dot-separated chunks [Head, _Tail]
	LName = case is_atom(Name) of 
	true -> atom_to_list(Name);
	_ -> Name
	end,
	[LHead | Tail ] = string:tokens(LName, "."),
	Head = list_to_atom(LHead),
% if Data undefined - make Data as empty list
% search Data for Head key
	case lists:keyfind(Head, 1, Data) of 
% if not found
	false ->
		case length(Tail) of
%	if length(Tail) == 0
		0 ->
%		add tuple {Head, Value}
			lists:merge(Data, [{Head, Value}]);
%	else
		_ ->
%		add tuple {Head, set(Tail, Value, [])}
			lists:merge(Data, [{Head, set(string:join(Tail, "."), Value)}])
%	endif
		end;
% else
	{_, _Founds} ->
		case length(Tail) of
%	if length(Tail) =0
%		replace founded to Data
		0 ->
			lists:keyreplace(Head, 1, Data, {Head, Value});
%	else
%		replace founded to set(Tail, Value, Founded)
		_ ->
			lists:keyreplace(Head, 1, Data, {Head, set(string:join(Tail, "."), Value, _Founds)})
%	endif
		end
%endif
	end
% return new list
.

unset(Name, Data) ->
	LName = case is_atom(Name) of 
	true -> atom_to_list(Name);
	_ -> Name
	end,
	[LHead | Tail ] = string:tokens(LName, "."),
	Head = list_to_atom(LHead),
	case lists:keyfind(Head, 1, Data) of 
	false ->
		Data;
	{_, _Founds} ->
		case length(Tail) of
		0 ->
			lists:keydelete(Head, 1, Data);
		_ ->
			lists:keyreplace(Head, 1, Data, {Head, unset(string:join(Tail, "."), _Founds)})
		end
	end
.


merge_map(F, A, [H|T]) -> [F(A, H)|merge_map(F, A, T)];
merge_map(_F, _A, [])	-> [].

merge(Global, Local) ->
	Fillfunc = fun(Local_all, Glob)-> 
		Glob_key=element(1, Glob),
		Glob_val=element(2, Glob),
		Test=lists:keyfind(Glob_key, 1, Local_all),
		case Test of
			false ->
				{Glob_key,  Glob_val};
			{_loc_key, _loc_value} when is_list(Glob_val) and is_list(_loc_value) ->
				{_loc_key, merge(Glob_val, _loc_value)};
			{_loc_key, _loc_value} ->
					{_loc_key, _loc_value};
			_Else ->
				Test
		end
	end,
	if 
	is_list(Global) and is_list(Local) ->
		[Glob_first | _ ] = Global,
		[Loc_first | _ ] = Local,
		if 
		is_tuple(Glob_first) and is_tuple(Loc_first) ->
			merge_map(Fillfunc, Local, lists:ukeymerge(1, lists:keysort(1, Global), lists:keysort(1, Local)));
		true ->
			Local
		end;
	true ->
		Local
	end
.
