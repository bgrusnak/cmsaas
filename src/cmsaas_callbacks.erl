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

-module(cmsaas_callbacks).

% includes
-include("cmsaas.hrl").
-include("./deps/cowboy/include/http.hrl").

-export([start/1, language/1, ids/1, auth/1, sanity/1, l10n/1, page/1, menu/1, template/1, i18n/1, final/1, process/1]).

start(Args) ->
	Args#page{headers=[{<<"content-type">>, <<"text/html">>}, {<<"content-encoding">>, <<"utf-8">>}]}
.

language(Args) ->
	Args
.

ids(Args) ->   
	Args
.

post_auth(Req, Dbaccess, Config, Host) ->
	case Req#http_req.buffer of
		<<>> -> undefined;
		_ ->	{ok, PostVals, _} = cowboy_req:body_qs(Req),
			Login = proplists:get_value(treelists:get("auth.login", Config), PostVals),
			Password = proplists:get_value(treelists:get("auth.password", Config), PostVals),
			process_auth(Dbaccess, Config, Host, Login, Password)
	end
.

get_auth(Req, Dbaccess, Config, Host) ->
	{Login, _} = cowboy_req:qs_val(treelists:get("auth.login", Config), Req),
	{Password, _} = cowboy_req:qs_val(treelists:get("auth.password", Config), Req),
	process_auth(Dbaccess, Config, Host, Login, Password)
.

process_auth(Dbaccess, Config, Host, Login, Password) ->
	case Login of 
		undefined -> undefined;
		_ -> case Password of
			undefined -> undefined;
			_ -> cmsaas_rights:authenify(Dbaccess, Config, Host, Login, Password)
		end
	end
.


auth(Args) ->
io:format("Call Auth", []),
	Authenify=case treelists:get("auth.method", Args#page.config) of
		<<"POST">> -> post_auth(Args#page.req, Args#page.db, Args#page.config, Args#page.host);
		<<"GET">> -> get_auth(Args#page.req, Args#page.db, Args#page.config, Args#page.host);
		_ -> post_auth(Args#page.req, Args#page.db, Args#page.config, Args#page.host)
	end,
	Args#page{auth=Authenify}
.

sanity(Args) ->
	Args
.

l10n(Args) ->
	Args
.

page(Args) ->
	Args
.

menu(Args) ->
	Args
.

process(Args) ->
io:format("Call Process~n", []),
	cmsaas_automata:execute(Args)
.

exec_template(Path, Modulename, Params) ->
	Name=binary:bin_to_atom(<< Modulename/binary, "_dtl" >>, utf8),
	erlydtl_compiler:compile(Path, Name),
	apply(Name, render, [Params])
.

global_404(Args) ->
io:format("Call Global 404~n", []),
	Design="404",
	try exec_template(file:join(["./priv/hosts/.default", "templates", Design]), Design, Args#page.data) of
				{ok, Body} -> 
					Args#page{html=Body, httpcode=404}
	catch error:_ -> 
		Args#page{html=io_lib:format("<html><head><title>404 error</title></head><body><h1>404 error</h1>Path <strong>~s</strong> in domain <strong>~s</strong> not found</body></html>", [Args#page.req#http_req.raw_path, Args#page.req#http_req.raw_host]), httpcode=404}
	end
.


template_404(Args) ->
io:format("Call 404~n", []),
	Design="404",
	try exec_template(file:join(["./priv/hosts", Args#page.host, "templates", Design]), Design,  Args#page.data) of
				{ok, Body} -> 
					Args#page{html=Body, httpcode=404}
	catch error:_ -> 
		global_404(Args)
	end
.

template(Args) ->
io:format("Call Template~n", []),
	Page=cmsaas_pages:get_page(Args#page.db, Args#page.req#http_req.raw_path),
io:format("Page ~n~p~n", [Page]),	
%	case lists:keyfind('design', Page) of
	case Page of
		undefined -> template_404(Args);
		Designlist ->
			Design=bson:at('design', Designlist),
			try exec_template(file:join(["./priv/hosts", Args#page.host, "templates", Design]), Design, lists:ukeymerge(1, lists:keyfind('items', Page), Args#page.data)) of
				{ok, Body} -> 
					Args#page{html=Body}
			catch error:_ -> 
				template_404(Args)
			end
	end
.


i18n(Args) ->
	Args
.

final(Args) ->
	Args
.
