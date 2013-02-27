-module(cmsaas_items).
-export([execute/3]).
-include("cmsaas.hrl").
-include("./deps/cowboy/include/http.hrl").



execute (Item, Dbaccess, Req) ->
	Content=bson:at(content, Item),
	case bson:at('contenttype',  Item) of
		<<"single">> ->
			Single=bson:at(content,  Item),
			Coll=bson:at('$ref',  Single),
			Id=bson:at('$id',  Single),
			mofun:find_one(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Coll, utf8), {'_id', Id})
		;
		<<"reference">> ->
			
			{Pointer}=bson:lookup('pointer',  Content),
			{Collection}=bson:lookup('collection',  Content),
			Key = case bson:lookup('key',  Content) of
				{Ky} -> Ky;
				{} -> 'id'
			end,
			Test=case binary:bin_to_list(Pointer) of
				"#" ->
					lists:last(Req#http_req.path)
				;
				["#"|Tail] ->
					try lists:last(Req#http_req.path) of
						[Tt|Tail] -> Tt
					catch error:_ -> ""
					end
				;
				Get ->
					{Tt, _} = cowboy_req:qs_val(Get, Req),
					Tt
			end,
			Row=case bson:lookup('db',  Content) of
				{Db} ->
					case Db of 
						<<"content">> ->
							mofun:find_one(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Collection, utf8), {Key, Test})
						;
						<<"system">> ->
							mofun:find_one(Dbaccess#dbaccess.db#dbset.system, binary:bin_to_atom(Collection, utf8), {Key, Test})
						;
						<<"billing">> ->
							mofun:find_one(Dbaccess#dbaccess.db#dbset.billing, binary:bin_to_atom(Collection, utf8), {Key, Test})
						;
						_ ->
							mofun:find_one(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Collection, utf8), {Key, Test})
					end;
				{} ->
					mofun:find_one(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Collection, utf8), {Key, Test})
			end,
			case bson:lookup(childref, Content) of
				{Childref} ->
					Childkey=case bson:lookup(childkey, Content) of
						{Ckey} -> Ckey;
						{} ->'_id'
					end,
					Childpointer=case bson:lookup(childpointer, Content) of
						{Cpf} ->
							case bson:lookup(binary:bin_to_atom(Cpf, utf8), Row) of
								{Cpv} -> Cpv;
								{} ->""
							end
						;
						{} ->""
					end,
					case bson:lookup('childdb',  Content) of
						{Cdb} ->
							case Cdb of 
								<<"content">> ->
									mofun:find_one(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Childref, utf8), {Childkey, Childpointer})
								;
								<<"system">> ->
									mofun:find_one(Dbaccess#dbaccess.db#dbset.system, binary:bin_to_atom(Childref, utf8), {Childkey, Childpointer})
								;
								<<"billing">> ->
									mofun:find_one(Dbaccess#dbaccess.db#dbset.billing, binary:bin_to_atom(Childref, utf8), {Childkey, Childpointer})
								;
								_ ->
									mofun:find_one(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Childref, utf8), {Childkey, Childpointer})
							end;
						{} ->
							mofun:find_one(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Childref, utf8), {Childkey, Childpointer})
					end
				;
				{} -> Row
			end
		;
		<<"list">> ->
			Content=bson:at(content, Item),
			Collection=bson:at('collection',  Content),
			Query=bson:at('query',  Content),
			Order = bson:lookup('order',  Content),
			Pagelimit = case bson:lookup('pagelimit',  Content) of
				{Pl} -> Pl;
				{} -> 10
			end,
			Paginator = case bson:lookup('paginator',  Content) of
				{Pg} -> cowboy_req:qs_val(Pg, Req);
				{} -> 0
			end,
			case bson:lookup('db',  Content) of
				{Db} ->
					case Db of 
						<<"content">> ->
							mofun:find(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Collection, utf8), {'query', Query, orderby, Order}, [], Paginator, Pagelimit)
						;
						<<"system">> ->
							mofun:find(Dbaccess#dbaccess.db#dbset.system, binary:bin_to_atom(Collection, utf8), {'query', Query, orderby, Order}, [], Paginator, Pagelimit)
						;
						<<"billing">> ->
							mofun:find(Dbaccess#dbaccess.db#dbset.billing, binary:bin_to_atom(Collection, utf8), {'query', Query, orderby, Order}, [], Paginator, Pagelimit)
						;
						_ ->
							mofun:find(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Collection, utf8), {'query', Query, orderby, Order}, [], Paginator, Pagelimit)
					end;
				{} ->
					mofun:find(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Collection, utf8), {'query', Query, orderby, Order}, [], Paginator, Pagelimit)
			end
		;
		<<"tree">> ->
			Tree=bson:at(tree, Content),
			Depth = case  bson:lookup(depthcounter, Content) of
				{Dv} -> cowboy_req:qs_val(Dv, Req);
				{} -> -1
			end,
			case bson:at(mode, Content) of
				<<"single">> ->
					Locator=binary:bin_to_list(cowboy_req:qs_val(bson:at(locator, Content), Req)),
					lists:map(fun({_,X}) ->  process_single_tree(X, Dbaccess, Req, Locator, Depth) end, bson:fields(Tree))
				;
				_ -> lists:map(fun({_,X}) ->  process_tree(X, Dbaccess, Req, Depth) end, bson:fields(Tree))
			end
		;
		<<"function">> ->
			Function=bson:at('function', Content),
			Module=bson:at('module', Content),
			Params= case bson:lookup('params', Content) of
				{Pp} -> 
					{_, Pr}=lists:unzip(Pp),
					Pr;
				{} -> []
			end,
			try apply(binary:bin_to_atom(Module, utf8), binary:bin_to_atom(Function, utf8), Params) of
				Value -> Value
			catch error:_ -> undefined
			end
		;
		_ ->
			bson:at(content,  Item)
	end	
.

process_single_tree(Element, Dbaccess, Req, Locator, Depth) ->
	Item=bson:at(item, Element), 
	Ref=bson:at('$ref', Item), 
	Id=bson:at('$id', Item), 
	Val=mofun:find_one(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Ref, utf8), {'_id', Id}),
	{Tree}=bson:lookup(childs, Element),
	Childs=case string:tokens(Locator, ".") of
		[Ss] -> case string:to_integer(Ss) of
				{error, _} -> [];
				{Nm, _} ->
					case (length(Tree)>Nm*2) of
						true -> case Depth of
							0 -> [];
							_ -> 
								process_single_tree(lists:keyfind(Nm, 1, bson:fields(Tree)), Dbaccess, Req, "", Depth-1)
							end
						;
						false ->[]
					end
				
			end
		;
		[Ps|Tail] ->case string:to_integer(Ps) of
				{error, _} -> [];
				{Nt, _} ->
					case (length(Tree)>Nt*2) of
						true -> case Depth of
								0 -> [];
								_ -> 
									process_single_tree(lists:keyfind(Nt, 1, bson:fields(Tree)), Dbaccess, Req, string:join(Tail, "."), Depth-1)
							end
						;
						false ->[]
					end
			end
		;

		[] -> []
	end,
	{treeitem, Val, Childs}
.


process_tree(Element, Dbaccess, Req, Depth) ->
	Item=bson:at(item, Element), 
	Ref=bson:at('$ref', Item), 
	Id=bson:at('$id', Item), 
	Val=mofun:find_one(Dbaccess#dbaccess.db#dbset.content, binary:bin_to_atom(Ref, utf8), {'_id', Id}),
	{Tree}=bson:lookup(childs, Element),
	Childs=case Depth of
		0 -> [];
		_ ->lists:map(fun({_,X}) ->  process_tree(X, Dbaccess, Req, Depth-1) end, bson:fields(Tree))
	end,
	{treeitem, Val, Childs}
.
