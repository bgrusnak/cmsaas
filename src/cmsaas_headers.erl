-module(cmsaas_headers).

-export([redirect/2]).


redirect(Url, Headers) ->
	Headers2=lists:keystore(<<"location">>, 1, Headers, {<<"location">>, Url}),
	{301, Headers2}
.
