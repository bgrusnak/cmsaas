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

-module(cmsaas).
-behaviour(application).
-export([start/0, start/2,  stop/1]).

%start() ->
%	application:start(sync),
%	application:start(mimetypes),
%%	,
%%	gettext_server:start(),
%%	LoadPo = 
%%            fun(Lang)->
%%                {_, Bin} = file:read_file("./lang/default/"++ Lang ++"/gettext.po"),
%%                gettext:store_pofile(Lang, Bin)
%%            end,
%%        lists:map(LoadPo, ["es","en"]),
%	application:start(crypto),
%	application:start(public_key),
%	application:start(ssl),
%	application:start(emongo),
%	application:start(cowboy),
%	start_app(cmsaas).
%
%


start() ->
	start_app(sync),
	start_app(mimetypes),
	start_app(crypto),
	start_app(public_key),
	start_app(ssl),
%	start_app(mongo),
%	start_app(emongo),
	start_app(mongodb),
	start_app(cowboy),
    start_app(cmsaas).

start(_Type, _Args) ->
	start()
.
    
start_app(App) ->
    case application:start(App) of
        {error, {not_started, Dep}} ->
            start_app(Dep),
            start_app(App);
        Other ->
            Other
    end.

stop(_State) ->
	ok.
