#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname pqd_dev \
    -s pqd \
    -s reloader
