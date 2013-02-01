#!/bin/sh

exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
	-sname galerlxy \
	-eval 'application:start(galerlxy).' \
	+K true \
	+A 5 \
	-config config/galerlxy.config

