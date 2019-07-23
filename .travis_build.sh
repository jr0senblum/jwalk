#!/bin/bash

wget https://s3.amazonaws.com/rebar3/rebar3
it clone https://github.com/erlang/rebar3.git
 cd rebar3
 ./bootstrap
cp rebar3 ../
cd ..

chmod a+x ./rebar3
make clean
make compile
make eunit

