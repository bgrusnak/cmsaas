-module(cmsaas_pages).
-export([get_page/2, get_page/3, get_page/4]).
-include("cmsaas.hrl").
-include("./deps/cowboy/include/http.hrl").

get_doc(Dbaccess, Path, Version, Master) ->
	Params=[{'$where', <<"function() { var expr=new RegExp(\"^\"+this.path+\".*\"); return expr.test(\"",  Path/binary, "\")}">>}, {orderby, {'path', -1, 'master', 1, 'version', -1}}],
	Params2=case Version of
		undefined ->
			 Params;
		_ -> 
			lists:merge(Params, [{'version', Version}])
	end,
	Params3=case Master of
		undefined ->
			 Params2;
		_ -> 
			lists:merge(Params2, [{'master', Master}])
	end,	
	case mofun:find_one(Dbaccess#dbaccess.db#dbset.content, 'core.paths', bson:document(Params3)) of
		{} -> undefined;
		Pp ->
			case bson:at('strict', Pp) of
				true ->
					case (Path == bson:at('path', Pp)) of
						true ->
							Pp
						;
						false ->
							undefined
					end
				;
				false ->
					Pp
			end
	end
.

get_page(Dbaccess, Path) ->
	get_page(Dbaccess, Path, undefined, 0)
.

get_page(Dbaccess, Path, Version) ->
	get_page(Dbaccess, Path, Version, 0)
.

get_page(Dbaccess, Path, Version, Master) ->
	case get_doc(Dbaccess, Path, Version, Master) of
		undefined ->
			undefined;
		Doc ->
			process_page(Dbaccess, bson:at('document', Doc))
	end
.

process_page(Dbaccess, Pageid) ->
	case mofun:find_one(Dbaccess#dbaccess.db#dbset.content, 'data.pages', {'_id', Pageid}) of
		{} -> undefined;
		Page ->
			case bson:lookup('template', Page) of
				{} -> bson:fields(Page);
				{Tid} -> 
					case get_page(Dbaccess, Tid) of
						{} -> bson:fields(Page);
						Template ->
							Design = case bson:lookup('design', Page) of
								{} -> 
									{Xtd} = bson:lookup('design', Template),
									Xtd;
								{Xd} -> Xd
							end,
							Title = case bson:lookup('title', Page) of
								{} -> 
									{Xtt} = bson:lookup('title', Template),
									Xtt;
								{Xt} -> Xt
							end,
							Meta = case bson:lookup('meta', Page) of
								{} -> 
									{Xtm} = bson:lookup('meta', Template),
									Xtm;
								{Xm} -> Xm
							end,
							{PageItems}=bson:lookup('items', Page),
							{TemplateItems}=bson:lookup('templates', Page),
							Items=lists:ukeymerge(1, lists:keysort(1, bson:fields(PageItems)), lists:keysort(1, bson:fields(TemplateItems))),
							[{'design', Design}, {'title', Title}, {'meta', Meta}, {'items', Items}]
					end
			end
	end
.

