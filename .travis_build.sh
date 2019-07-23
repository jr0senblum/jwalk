#!/bin/bash

#wget https://s3.amazonaws.com/rebar3/rebar3

chmod a+x ./rebar3
make clean
make compile
make eunit

