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

-module(cmsaas_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_cowboy(),
    cmsaas_sup:start_link().

stop(_State) ->
    ok.

%% Internal

start_cowboy() ->
% start config server
	cmsaas_conf:start_link(),
% start DB server	
	cmsaas_db:start_link(),
% start cache manager	
	cmsaas_cache:start_link(),
% start sessions manager	
	cmsaas_sessions:start_link(),
	Dispatch = [
	 {'_', [
		{[<<"static">>, '...'], cmsaas_http_static,
		[{directory, {host_dir, "./priv/hosts", "static"}},
%			{mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
		  {mimetypes,  [
			  {<<".css">>, [<<"text/css">>]}
			  , {<<".js">>, [<<"application/javascript">>]}
			  , {<<".eot">>, [<<"application/vnd.ms-fontobject">>]}
			  , {<<".svg">>, [<<"image/svg+xml">>]}
			  , {<<".ttf">>, [<<"application/x-font-ttf">>]}
			  , {<<".woff">>, [<<"application/x-font-woff">>]}
			  , {<<".png">>, [<<"image/png">>]}
			  , {<<".jpg">>, [<<"image/jpeg">>]}
			  , {<<".jpeg">>, [<<"image/jpeg">>]}
			  , {<<".gif">>, [<<"image/gif">>]}
			  , {<<".ico">>, [<<"image/x-icon">>]}
			  , {<<".html">>, [<<"text/html">>]}
			  , {<<".htm">>, [<<"text/html">>]}
		  ]}]}			
		, {'_', cmsaas_http_handler, []}
		]}	
	],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(cmsaas_http_listener, 100,
                          cowboy_tcp_transport, [{port, 8010}],
                          cowboy_http_protocol, [{dispatch, Dispatch},
                                                 {onrequest, fun cowboy_session:on_request/1}
                                                ]
                         ).
