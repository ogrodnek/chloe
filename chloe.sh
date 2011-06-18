#!/bin/bash

ROOT=`dirname $0`

erl -config chloe -pa $ROOT/deps/mochiweb/ebin $ROOT/ebin -s chloe