#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s crypto -s mnesia -s reversi_app -s reversi_rest -name reversi@127.0.0.1
