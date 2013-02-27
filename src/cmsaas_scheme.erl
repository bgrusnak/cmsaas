-module(cmsaas_scheme).
-export([get_scheme/2, add_field/5, del_field/3, create_scheme/2, del_scheme/2, make_by_scheme/3]).
-include("cmsaas.hrl").
-include("./deps/cowboy/include/http.hrl").


get_scheme(Name, Dbaccess) ->
	mofun:find_one(Dbaccess#dbaccess.db#dbset.content, 'data.scheme', {'name', binary:list_to_bin(Name)})
.

add_field(Name, Dbaccess, Fieldname, Type, Default) ->
	mofun:update(Dbaccess#dbaccess.db#dbset.content, 'data.scheme', {'name', binary:list_to_bin(Name)}, {'$addToSet', {fields, {name, binary:list_to_bin(Fieldname), type, binary:list_to_bin(Type), default, binary:list_to_bin(Default)}}}, true)
.

del_field(Name, Dbaccess, Fieldname) ->
	mofun:update(Dbaccess#dbaccess.db#dbset.content, 'data.scheme', {'name', binary:list_to_bin(Name)}, {'$pull', {fields, {name, binary:list_to_bin(Fieldname)}}}, true)
.

create_scheme(Name, Dbaccess) ->
	mofun:insert(Dbaccess#dbaccess.db#dbset.content, 'data.scheme', {'name', binary:list_to_bin(Name), fields, []})
.

del_scheme(Name, Dbaccess) ->
	mofun:delete_one(Dbaccess, 'data.scheme', {'name', binary:list_to_bin(Name)})
.

make_by_scheme(Name, Dbaccess, Values) ->
	Scheme=get_scheme(Name, Dbaccess),
	Values=bson:document(lists:map(fun({_,X}) -> {bson:at(name, X), case lists:keyfind(bson:at(name, X), 1, Values) of {_, Val} -> Val; false -> bson:at(default, X) end } end, bson:fields(bson:at(fields, Scheme)))),
	mofun:insert(Dbaccess#dbaccess.db#dbset.content, list_to_atom("data."++Name), Values)
.
