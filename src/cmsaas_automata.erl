-module(cmsaas_automata).
-export([execute/1, do_string/2]).
-include("cmsaas.hrl").
-include("./deps/cowboy/include/http.hrl").


execute(Page) ->
	execute (Page, 1)
.

execute(Page, Order) ->
	Rawpath=Page#page.req#http_req.raw_path,
	case mofun:find_one(Page#page.db#dbaccess.db#dbset.content, 'data.submit', {order, Order, '$where', <<"function() { var expr=new RegExp(\"^\"+this.path+\".*\"); return expr.test(\"",  Rawpath/binary, "\")}">>}) of
		{} -> Page;
		Worker ->
			{Page2, Order2}=process(bson:at(type, Worker), Page, Worker, Order),
			case Order2 of
				undefined ->
					Page2
				;
				_ -> execute(Page2, Order2)
			end
	end
.

do_string(String, Data) ->
	Spl=re:split(String, "[\{\}]", [{return, list}]),
	string:join(pass_string(Spl, Data), "")
.

pass_string([[]],_Data) -> 
	[]
;
pass_string([[]|Tail], Data) ->
	[Varname|Lst]=Tail,
	{_, Fnd}=lists:keyfind(binary:list_to_bin(Varname),1, Data),
	lists:append([Fnd], pass_string(Lst, Data))
;

pass_string([Txt], _Data) ->
	[Txt]
;

pass_string([Txt|Tail], Data) ->
	[Varname|Lst]=Tail,
	{_, Fnd}=lists:keyfind(binary:list_to_bin(Varname),1, Data),
	lists:append([Txt, Fnd], pass_string(Lst, Data))
.

process(Type, Page, Worker, Order) when Type== <<"set">> ->
	Val=do_string(bson:at('value', Worker), Page#page.data),
	Key=do_string(bson:at('variable', Worker), Page#page.data),
	Page2=Page#page{data=lists:keystore(Key, 1, Page#page.data, {Key, Val})},
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page2, Neworder}
;

process(Type, Page, _Worker, _Order) when Type== <<"stop">> ->
	{Page, undefined}
;

process(Type, Page, Worker, Order) when Type== <<"test">> ->
	Val=do_string(bson:at('value', Worker), Page#page.data),
	NewOrder=case (Val == lists:keyfind(bson:at('variable', Worker), 1, Page#page.data)) of
		true -> bson:at('jump', Worker);
		_ -> Order+1
	end,
	{Page, NewOrder}
;

process(Type, Page, Worker, Order) when Type== <<"copy">> ->
	Varname=do_string(bson:at('variable', Worker), Page#page.data),
	Toname=case bson:lookup('toname', Worker) of
		{} -> Varname;
		{X} -> do_string(X, Page#page.data)
	end,
	{_, PVal}=lists:keyfind(Varname, 1, Page#page.data),
	Val=do_string(PVal, Page#page.data),
	Page2=case bson:at('to', Worker) of
		<<"GET">> ->
			Page#page{get=lists:keystore(Toname, 1, Page#page.get, {Toname, Val})}
		;
		<<"POST">> ->
			Page#page{post=lists:keystore(Toname, 1, Page#page.post, {Toname, Val})}
		;
		<<"SESSION">> ->
			cmsaas_sessions:set(Page#page.host, Page#page.session, Toname, Val),
			Page
		;
		_ -> 
			Page#page{data=lists:keystore(Varname, 1, Page#page.data, {Toname, Val})}
	end,
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page2, Neworder}
;

process(Type, Page, Worker, Order) when Type== <<"function">> ->
	Params = case bson:lookup('fields', Worker) of
		{} -> [];
		{X} -> 
			lists:map(fun(E) ->  {_, V}=lists:keyfind(E, 1, Page#page.data), {E, do_string(V, Page#page.data)} end, X)
	end,
	[Module, Function]=string:tokens(bson:at('function', Worker), ":"),
	Rt=try apply(binary:bin_to_atom(Module, utf8), binary:bin_to_atom(Function, utf8), Params) of
		Value -> Value
	catch error:_ -> undefined
	end,
	Page2=case bson:lookup('resultfield', Worker) of
		{} -> Page;
		{Rf} -> 
			Prf=do_string(Rf, Page#page.data),
			Page#page{data=lists:keystore(Rf, 1, Page#page.data, {Prf, Rt})}
	end,
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page2, Neworder}
;

process(Type, Page, Worker, Order) when Type== <<"redirect">> ->	
	Uri=do_string(bson:at('uri', Worker), Page#page.data),
	{Status, Headers}=cmsaas_headers:redirect(Uri, Page#page.headers),
	Page2=Page#page{headers=Headers, httpcode=Status},
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page2, Neworder}

;

process(Type, Page, Worker, Order) when Type== <<"validate">> ->	
	Validator=do_string(bson:at('validator', Worker), Page#page.data),
	Params = case bson:lookup('params', Worker) of
		{} -> [];
		{X} -> 
			lists:map(fun(E) ->  {_, V}=lists:keyfind(E, 1, Page#page.data), {E, do_string(V, Page#page.data)} end, X)
	end,
	Val=validate:value(do_string(bson:at('variable', Worker), Page#page.data), Validator, Params),
	Page2=case bson:at('resultfield', Worker) of
		undefined ->
			Page
		;
		Toname -> 
			Page#page{data=lists:keystore(Toname, 1, Page#page.data, {Toname, Val})}
	end,
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page2, Neworder}
;

process(Type, Page, Worker, Order) when Type== <<"file">> ->	
	case list:keysearch(bson:at('variable', Worker), #datafile.name, Page#page.files) of
		false -> false;
		File ->
			file:rename(File#datafile.tempname, file:join(["./priv/hosts", Page#page.host, "upload", filename:basename(File#datafile.tempname)])),
			savefile(Page#page.db#dbaccess.db#dbset.content, filename:basename(File#datafile.tempname), File#datafile.size, File#datafile.contentType, filename:basename(File#datafile.originalName))
	end,
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page, Neworder}	
;

process(Type, Page, Worker, Order) when Type== <<"create">> ->	
	Coll=list_to_atom(do_string(bson:at('collection', Worker), Page#page.data)),
	Params = case bson:lookup('fields', Worker) of
		{} -> [];
		{X} -> 
			lists:map(fun(E) ->  {_, V}=lists:keyfind(E, 1, Page#page.data), {E, do_string(V, Page#page.data)} end, X)
	end,
	Val=case do_string(bson:at('db', Worker), Page#page.data) of
		<<"content">> ->
			mofun:insert(Page#page.db#dbaccess.db#dbset.content, Coll, bson:document(Params))
		;
		<<"system">> ->
			mofun:insert(Page#page.db#dbaccess.db#dbset.system, Coll, bson:document(Params))
		;
		<<"billing">> ->
			mofun:insert(Page#page.db#dbaccess.db#dbset.billing, Coll, bson:document(Params))
		;
		_ ->
			mofun:insert(Page#page.db#dbaccess.db#dbset.content, Coll, bson:document(Params))
	end,
	Page2=case bson:at('resultfield', Worker) of
		undefined ->
			Page
		;
		Toname -> 
			Page#page{data=lists:keystore(Toname, 1, Page#page.data, {Toname, Val})}
	end,
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page2, Neworder}
;

process(Type, Page, Worker, Order) when Type== <<"update">> ->	
	Coll=list_to_atom(do_string(bson:at('collection', Worker), Page#page.data)),
	Params = case bson:lookup('fields', Worker) of
		{} -> [];
		{X} -> 
			lists:map(fun(E) ->  {_, V}=lists:keyfind(E, 1, Page#page.data), {E, do_string(V, Page#page.data)} end, X)
	end,
	Filter = case bson:lookup('filter', Worker) of
		{} -> [];
		{Y} -> 
			lists:map(fun(E) ->  {_, V}=lists:keyfind(E, 1, Page#page.data), {E, do_string(V, Page#page.data)} end, Y)
	end,
	Val=case do_string(bson:at('db', Worker), Page#page.data) of
		<<"content">> ->
			mofun:update(Page#page.db#dbaccess.db#dbset.content, Coll, bson:document(Filter), bson:document(Params), true)
		;
		<<"system">> ->
			mofun:insert(Page#page.db#dbaccess.db#dbset.system, Coll, bson:document(Filter), bson:document(Params), true)
		;
		<<"billing">> ->
			mofun:insert(Page#page.db#dbaccess.db#dbset.billing, Coll, bson:document(Filter), bson:document(Params), true)
		;
		_ ->
			mofun:insert(Page#page.db#dbaccess.db#dbset.content, Coll, bson:document(Filter), bson:document(Params), true)
	end,
	Page2=case bson:at('resultfield', Worker) of
		undefined ->
			Page
		;
		Toname -> 
			Page#page{data=lists:keystore(Toname, 1, Page#page.data, {Toname, Val})}
	end,
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page2, Neworder}
;

process(Type, Page, Worker, Order) when Type== <<"read">> ->	
	Coll=list_to_atom(do_string(bson:at('collection', Worker), Page#page.data)),
	Filter = case bson:lookup('filter', Worker) of
		{} -> [];
		{Y} -> 
			lists:map(fun(E) ->  {_, V}=lists:keyfind(E, 1, Page#page.data), {E, do_string(V, Page#page.data)} end, Y)
	end,
	Val=case do_string(bson:at('db', Worker), Page#page.data) of
		<<"content">> ->
			mofun:find(Page#page.db#dbaccess.db#dbset.content, Coll, bson:document(Filter))
		;
		<<"system">> ->
			mofun:find(Page#page.db#dbaccess.db#dbset.system, Coll, bson:document(Filter))
		;
		<<"billing">> ->
			mofun:find(Page#page.db#dbaccess.db#dbset.billing, Coll, bson:document(Filter))
		;
		_ ->
			mofun:find(Page#page.db#dbaccess.db#dbset.content, Coll, bson:document(Filter))
	end,
	Page2=case bson:at('resultfield', Worker) of
		undefined ->
			Page
		;
		Toname -> 
			Page#page{data=lists:keystore(Toname, 1, Page#page.data, {Toname, Val})}
	end,
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page2, Neworder}

;

process(Type, Page, Worker, Order) when Type== <<"delete">> ->	
	Coll=list_to_atom(do_string(bson:at('collection', Worker), Page#page.data)),
	Filter = case bson:lookup('filter', Worker) of
		{} -> [];
		{Y} -> 
			lists:map(fun(E) ->  {_, V}=lists:keyfind(E, 1, Page#page.data), {E, do_string(V, Page#page.data)} end, Y)
	end,
	Val=case do_string(bson:at('db', Worker), Page#page.data) of
		<<"content">> ->
			mofun:delete(Page#page.db#dbaccess.db#dbset.content, Coll, bson:document(Filter))
		;
		<<"system">> ->
			mofun:delete(Page#page.db#dbaccess.db#dbset.system, Coll, bson:document(Filter))
		;
		<<"billing">> ->
			mofun:delete(Page#page.db#dbaccess.db#dbset.billing, Coll, bson:document(Filter))
		;
		_ ->
			mofun:delete(Page#page.db#dbaccess.db#dbset.content, Coll, bson:document(Filter))
	end,
	Page2=case bson:at('resultfield', Worker) of
		undefined ->
			Page
		;
		Toname -> 
			Page#page{data=lists:keystore(Toname, 1, Page#page.data, {Toname, Val})}
	end,
	Neworder=case bson:lookup(next, Worker) of
		{} -> Order+1;
		{Pos} -> Pos
	end,
	{Page2, Neworder}
.

savefile(Dbset, Name, Length, ContentType, Original) ->
	case mofun:find_one(Dbset, 'data.files', {'name', Name}) of
		{} ->
			mofun:insert(Dbset, 'data.files', 'name', Name, 'length', Length, contenttype, ContentType, original, Original);
		_ ->
			savefile(Dbset, Name, Length, ContentType, Original,1)
	end
.

savefile(Dbset, Name, Length, ContentType, Original, Order) ->
	Nn=binary:bin_to_list(Name),
	Extension=finaname:extension(Nn),
	Bn=filename:basename(Nn, Extension),
	Os=integer_to_list(Order),
	NewName= <<Bn, "(", Os, ")", Extension >>,
	case mofun:find_one(Dbset, 'data.files', {'name', NewName}) of
		{} ->
			mofun:insert(Dbset, 'data.files', 'name', NewName, 'length', Length, contenttype, ContentType, original, Original);
		_ ->
			savefile(Dbset, Name, Length, ContentType, Original, Order+1)
	end
.
