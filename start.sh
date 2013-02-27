#!/bin/sh
erl -sname cmsaas -pa ebin -pa deps/*/ebin -s cmsaas \
	-eval "io:format(\"* Point your browser into: http://localhost:8010~n\")." 

