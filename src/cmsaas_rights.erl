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

-module(cmsaas_rights).


% includes
-include("cmsaas.hrl").

%% API
-export([
	authenify/5, 
	make_hash/4, 
	find_user/2,
	can_user/5,
	can_group/5,
	set_user_rights/6,
	set_group_rights/6,
	unset_user_rights/5,
	unset_group_rights/5
]).




find_user(Connect, User) ->
	Bin_user=binary:list_to_bin(User),
	Uc=mofun:dec2hex(User),
	case mofun:find_one(Connect, 'core.users',  {'$or', [{'_id', ?IF (Uc=="", false, Uc) }, {login, Bin_user}, {email, Bin_user}, {'_id', Bin_user}]}) of
		{} -> undefined;
		Ret -> Ret
	end
.

find_group(Connect, Group) ->
	Bin_group=binary:list_to_bin(Group),
	Gc=mofun:dec2hex(Group),
	case mofun:find_one(Connect, 'core.users',  {'$or', {'_id', ?IF (Gc=="", false, Gc), name, Bin_group}}) of
		{} -> undefined;
		Ret -> Ret
	end
.

make_cache_user_rec(Host, User, Resource, Right) ->
	"userrights:"++ Host ++ ":" ++ User ++ ":" ++ Resource ++ ":" ++ Right
.


make_cache_group_rec(Host, Group, Resource, Right) ->
	"grouprights:"++ Host ++ ":" ++ Group ++ ":" ++ Resource ++ ":" ++ Right
.



can_group(Dbaccess, Host, Group, Resource, Action) ->
	Cr=make_cache_group_rec(binary:bin_to_list(Host), Group, Resource, Action),
	case cmsaas_cache:get(binary:bin_to_list(Host), Cr) of
		undefined ->
			Ret=case find_group(Dbaccess#dbaccess.db#dbset.content, Group) of
				false -> false;
				undefined -> false;
				Group_res -> 
					{{Groupid}}=bson:lookup('_id',  Group_res),
					Gid=mofun:dec2hex(Groupid),
					Rt=case mofun:find_one(Dbaccess#dbaccess.db#dbset.content, 'core.rights', {'query', {'groups',  Gid, '$or', {list_to_atom(Action), true, list_to_atom(Action), false}, '$where', binary:list_to_bin("function() { var expr=new RegExp(\"^\"+this.path+\".*\"); return expr.test(\""++Resource++"\")}")}, orderby, {'path', -1, 'master', 1, 'version', -1, '_id', -1}}) of
						{} -> false;
						Rot -> 
							{R}=bson:lookup (list_to_atom(Action), Rot),
							R
					end,
					cmsaas_cache:set(Host, Cr, Rt, [rights, group, resource, Group, Resource, Action]),
					Rt
			end;
		Cached -> Cached
	end	
.

can_user(Dbaccess, Host, User, Resource, Action) ->
	Cr=make_cache_user_rec(binary:bin_to_list(Host), User, Resource, Action),
	case cmsaas_cache:get(binary:bin_to_list(Host), Cr) of
		undefined ->
			Ret=case find_user(Dbaccess#dbaccess.db#dbset.content, User) of
				false -> false;
				undefined -> false;
				User_res -> 
% get userid		
					{{Userid}}=bson:lookup('_id',  User_res),
					Uid=mofun:dec2hex(Userid),
%get list of groups for user		
					Groupsrt=mofun:find(Dbaccess#dbaccess.db#dbset.content, 'core.groups', {'users', Uid}),
					Groups=lists:map(fun(X) -> {R}=bson:lookup ('_id', X), R end, Groupsrt),
					Rt=case mofun:find_one(Dbaccess#dbaccess.db#dbset.content, 'core.rights', {'query', {'$or',  [{'users', Uid}, {'users', <<"*">>}, {'groups', Groups}], '$or', {list_to_atom(Action), true, list_to_atom(Action), false}, '$where', binary:list_to_bin("function() { var expr=new RegExp(\"^\"+this.path+\".*\"); return expr.test(\""++Resource++"\")}")}, orderby, {'path', -1, 'master', 1, 'version', -1, '_id', -1}}) of
						{} -> false;
						Rot -> 
							{R}=bson:lookup (list_to_atom(Action), Rot),
							R
					end,
					cmsaas_cache:set(Host, Cr, Rt, [rights, user, resource, User, Resource, Action]),
					Rt
			end;
			Cached -> Cached
	end
.

set_user_rights(Dbaccess, Host, User, Resource, Action, State) ->
	cmsaas_cache:clear_tagged(Host, Resource),
	mofun:update(Dbaccess#dbaccess.db#dbset.content, 'core.rights', {path, Resource, list_to_atom(Action), State}, {'$addToSet', {users, User}}, true)
.

set_group_rights(Dbaccess, Host, Group, Resource, Action, State) ->
	cmsaas_cache:clear_tagged(Host, Resource),
	mofun:update(Dbaccess#dbaccess.db#dbset.content, 'core.rights', {path, Resource, list_to_atom(Action), State}, {'$addToSet', {groups, Group}}, true)
.

unset_user_rights(Dbaccess, Host, User, Resource, Action) ->
	cmsaas_cache:clear_tagged(Host, Resource),
	mofun:update(Dbaccess#dbaccess.db#dbset.content, 'core.rights', {path, Resource, '$or', [{list_to_atom(Action), true}, {list_to_atom(Action), false}]}, {'$pull', {users, User}}, true)
.

unset_group_rights(Dbaccess, Host, Group, Resource, Action) ->
	cmsaas_cache:clear_tagged(Host, Resource),
	mofun:update(Dbaccess#dbaccess.db#dbset.content, 'core.rights', {path, Resource, '$or', [{list_to_atom(Action), true}, {list_to_atom(Action), false}]}, {'$pull', {groups, Group}}, true)
.

make_hash(Host, Login, Password, Salt) ->
	lists:flatmap(fun(X)->integer_to_list(X, 16) end, binary_to_list(erlang:md5(string:join([Host, Login, Salt, Password], "|"))))
.


authenify(Dbaccess, Config, Host, Login, Password) ->
	Hash=make_hash(binary:bin_to_list(Host), binary:bin_to_list(Login), binary:bin_to_list(Password), treelists:get("host.salt", Config)),
% try to find user with login
	case mofun:find_one(Dbaccess#dbaccess.db#dbset.content, 'core.users',  {login, Login}) of
% user not found
		{} -> #authenify{state=error};
% user found, check password
		X -> 
			{Password}=bson:lookup('password', X),
			case (Password==Hash) of
% found -> return user id
				true -> 
					{Id}=bson:lookup('_id', X),
					Uid=mofun:dec2hex(Id),
%get list of groups for user		
					Groupsrt=mofun:find(Dbaccess#dbaccess.db#dbset.content, 'core.groups', {'users', Uid}),
					Groups=lists:map(fun(Y) -> {R}=bson:lookup ('_id', Y), R end, Groupsrt),
					#authenify{state=ok, id=Uid, groups=Groups};
% not found -> wrong password
				false -> #authenify{state=wrong}
			end
	end
.
