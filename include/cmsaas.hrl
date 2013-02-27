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

% database access configuration record
-record(dbconfig, {database, connector, user, password}).
% db sets record
-record(dbset, {content, system, billing}).

% global db configuration
-record(dbaccess, {host= <<"localhost">>, port=27017, workers=100, db=#dbset{}}).

% rights definition
-record(rights, {create=undefined, read=undefined, update=undefined, delete=undefined}).

% rightset definition
-record(rightset, {'_id', host, domain, groups=[], users=[], path= <<"">>, rights=#rights{}}).

% group definition
-record(group, {'_id', host, domain, name, users=[]}).

% user definition
-record(user, {'_id', host, name, login, password, email, data=[], groups=[]}).

% session structure
-record(session, {'_id', accessed, user, session, data=[]}).

% callback state definition
-record(callstate, {callbacks, current}).

% callback declaration structure
-record(callback, {id, callback, module, function}).

% authenification state
-record(authenify, {state, id=undefined, groups=[]}).

% page data
-record(page, {host, session, req, headers=[], db, auth=undefined, data=[], html= <<"">>, languages=[], errors=[], attacklevel=0,  l10n=undefined, config, preconfig, httpcode=200, near=[], version=0, get=[], post=[], files=[]}).


% IF macro for ternary  COND ? A : B  operator

-define(IF(Cond,E1,E2), (case (Cond) of true -> (E1); false -> (E2) end)).

% Callback directory
-define(CALLBACK_PATH, "./priv/callback").
-define(CALLBACK_MODULE, "cmsaas_chain").


-record(filestate,{
	  headers = [],
	  max_file_size = 5000,
	  max_files =  unlimited,
	  tmp_folder = "/tmp",
	  tmp_filename = "",
	  files_cnt = 0
	 }).

-record(datafile,{name = <<"">> :: binary(),
tempname = <<"">> :: binary(),
		  size = 0 :: non_neg_integer(),
		  'contentType' :: binary(),
		  'originalName' :: binary()
		 }).
