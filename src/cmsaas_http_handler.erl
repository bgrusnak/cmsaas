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
-module(cmsaas_http_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("cmsaas.hrl").
-include("deps/cowboy/include/http.hrl").

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

	
handle(Req, State) ->
	erlydtl_compiler:compile("./templates/layout.dtl", layout_dtl),
	PreHost=Req#http_req.raw_host,
	PreConfig=cmsaas_conf:lookup(PreHost),
	Covers=treelists:get("covers", PreConfig),
	Path=Req#http_req.path,
	Jp= case Path of
		[] ->
			"/*";
		_ ->
		filename:join(filename:join(Path), "*")
	end,
	At=lists:foldl(fun(X, Acc) -> 
		{Atop, _}=X, 
		Ap=atom_to_list(Atop), 
		case re:run(Jp, "^" ++ Ap) of
			nomatch ->
				Acc
			;
			{match, [{_From, _Cnt}]} ->
				?IF(length(Ap)>length(Acc), Ap, Acc)
		end
	end, "", Covers),
	Host=case At of
		"" ->
			PreHost;
		Ac ->
			Pp = treelists:get("covers." ++ Ac, PreConfig),
			Pp
	end,
	Config=case At of
		"" ->
			PreConfig;
		_ ->
			cmsaas_conf:lookup(Host)
	end,
	{Session, _} = cowboy_session:from_req(Req),
	SessionId = case Session of
    	undefined ->
			<<"No session">>;
		_ ->
			cowboy_session_server:session_id(Session)
	end,
	Db=cmsaas_db:get_db(Config),
	{Method, _} = cowboy_http_req:method(Req),
	{Get, _} =cowboy_http_req:qs_vals(Req),
	{HasBody, _} = cowboy_http_req:has_body(Req),
	Post=case Method of 
		<<"POST">> ->
			case HasBody of
				true ->
					{PostVals, _} = cowboy_http_req:body_qs(Req),
					PostVals
				;
				false ->
					[]
			end;
		_ ->
			[]
	end,
%	{Files, _}
	Files=case Req#http_req.buffer of
		<<>> -> {ok, []};
		_ -> cowboy_multipart_uploader:files_qs(Req, [{max_file_size, 50000000}])
	end,
	Prepage=#page{host=Host, session=SessionId, req=Req, config=Config, preconfig=PreConfig, db=Db, get=Get, post=Post, files=Files},
	Page=lists:foldl(fun(Callback, Data) -> cmsaas_chain:call(Callback, Data) end, Prepage, treelists:get("callbacks", Config)),
	
	{ok, Req2} = cowboy_http_req:reply(Page#page.httpcode, Page#page.headers , Page#page.html, Req),
	{ok, Req2, State}.	

terminate(_Req, _State) ->
	ok.
