% This module is automatically generated. 
-module(cmsaas_chain).
-export([call/2]).
chain_authenify(Data, Order) when Order==1 ->
	cmsaas_callbacks:auth(Data)
.

chain_start(Data, Order) when Order==1 ->
	cmsaas_callbacks:start(Data)
.

chain_template(Data, Order) when Order==1 ->
	cmsaas_callbacks:template(Data)
.

chain_process(Data, Order) when Order==1 ->
	cmsaas_callbacks:process(Data)
.

call(Callback, Data) when Callback == authenify ->
	chain_authenify(Data, 1)
;
call(Callback, Data) when Callback == start ->
	chain_start(Data, 1)
;
call(Callback, Data) when Callback == template ->
	chain_template(Data, 1)
;
call(Callback, Data) when Callback == process ->
	chain_process(Data, 1)
;
call(_Callback, Data) ->
	Data
.
