#jwalk
##Helper module for working with Erlang Proplist, EEP18, Mochi-style and Map representations of JSON

[![Build Status](https://travis-ci.org/jr0senblum/jwalk.svg)](https://travis-ci.org/jr0senblum/jwalk)
[![hex.pm version](https://img.shields.io/hexpm/v/jwalk.svg)](https://hex.pm/packages/jwalk)

This work is inspired by [ej](https://github.com/seth/ej), but handles map, eep-18, mochijson-style 
and proplist representations of JSON - the types returned by
[jsone](https://github.com/sile/jsone) or [jiffy](https://github.com/davisp/jiffy),
for example. 
Anything good about this is probably due to the contributors and maintainers of 
ej, anything bad or awkward is completely my fault.

###Dependencies
The source code is a single file, and there are no dependencies other than:

* Erlang 17.0 +
* Rebar3, for building

Hex package manager is used for package management.


###QuickStart
##### clone
$ git clone git://github.com/jr0senblum/jwalk.git

$ cd jwalk

#### compile
$ make compile

#### run tests
$ make eunit

#### dialyze
$ make dialyze

#### Erlang shell
$ make start

1> jwalk:get({"one"},#{<<"one">> => 1}).

1


##Functions
Functions always take at least two parameters: a first parameter which is a
tuple of elements representing a Path into a JSON Object, and a second 
parameter which is expected to be a valid JSON representation (map, proplist, etc.).


* ``jwalk:delete(Path, Object)`` - Removes the value at the location 
specified by Path from Object and returns a new structure
* ``jwalk:get(Path, Object)``, ``jwalk:get(Path, Object, Default)``  - Returns the 
Value at the specificed Path from Object, or undefined or Default if not found
* ``jwalk:set(Path, Object, Value)`` - Sets a Value in Object at the specified
Path and returns the new structure
* ``jwalk:set_p(Path, Object, Value)`` - Sets a Value in an Object at the specified
Path creating intermediate nodes as necessary and returns the new structure

###Paths
Paths into JSON Objects are expressed using a tuple of Path elements, a 
representation of a javascript-like path: i.e.,
``Obj.weapons.edged.distance``  would be expressed as 
``{"weapons","edged","distance"}``. 
Path elements representing JSON Member Names can be strings or binary, they 
will be internally converted to binary regardless.

In addition to the string/binary representations of Member Names, a Path element can 
be

* An integer index, or the atoms `first` and `last`, which will select 
an element out of a JSON Array.
* ``{select, {"name","value"|value}}`` which will select a subset of JSON objects 
from an Array that have a Member ``{"Name": "Value"|Value}`` 
* `new` for set/2 and set_p/2, when the final element of a Path is the atom 
`new`, the supplied value is added to the stucture as the first element of
an Array, the Array is created if necessary

Path, string elements can be binary or not

Examples follow.

###Usage Examples
    Weapons = [{<<"edged">>, [[{<<"type">>, <<"swords">>}, {<<"distance">>, <<"medium">>}],   
                              [{<<"type">>, <<"bayonets">>},  {<<"distance">>, <<"medium">>}],
                              [{<<"type">>, <<"daggers">>}, {<<"distance">>, <<"close">>}]                            
                            ]                            
              }].
              

then
	
    1> jwalk:get({"edged", {select, {"distance","medium"}}},Weapons).
    [[{<<"type">>,<<"swords">>},{<<"distance">>,<<"medium">>}],
    [{<<"type">>,<<"bayonets">>},{<<"distance">>,<<"medium">>}]]

    2> jwalk:get({"edged",{select,{"distance","medium"}},1},Weapons).
    [{<<"type">>,<<"swords">>},{<<"distance">>,<<"medium">>}]

    3> jwalk:get({"edged",{select,{"distance","medium"}},1,"type"},Weapons).
    <<"swords">>
	
    % setting an element ...
    4> W2 = jwalk:set({"edged",3, "distance"}, Weapons, <<"very close">>).
    [{<<"edged">>,
     [[{<<"type">>,<<"swords">>},{<<"distance">>,<<"medium">>}],
      [{<<"type">>,<<"bayonets">>},{<<"distance">>,<<"medium">>}],
      [{<<"type">>,<<"daggers">>},{<<"distance">>,<<"very close">>}]]}]
    
    5> jwalk:get({"edged","distance"},W2).
    [<<"medium">>,<<"medium">>,<<"very close">>]

Given a map:

    Obj = #{<<"widget">> => 
            #{<<"debug">> => <<"on">>,
              <<"image">> => 
                  #{<<"alignment">> => <<"center">>,
                    <<"hOffset">> => 250,
                    <<"name">> => <<"sun1">>,
                    <<"src">> => <<"Images/Sun.png">>,
                    <<"vOffset">> => 250},
              <<"keys">> => [],
              <<"text">> => 
                  #{<<"alignment">> => <<"center">>,
                    <<"data">> => <<"Click Here">>,
                    <<"hOffset">> => 250,
                    <<"name">> => <<"text1">>,
                    <<"onMouseUp">> => <<"sun1.opacity = (sun1.opacity / 100) * 90;">>,
                    <<"size">> => 36,
                    <<"style">> => <<"bold">>,
                    <<"vOffset">> => 100},
              <<"values">> => [1,2,3,4,5],
              <<"version">> => <<"1">>,
              <<"window">> => 
                  #{<<"height">> => 500,
                    <<"name">> => <<"main_window">>,
                    <<"title">> => <<"Sample Konfabulator Widget">>,
                    <<"width">> => 500}}}.

then
    

    1> jwalk:get({"widget","debug"},Obj).
    <<"on">>
	
	2> jwalk:get({"widget","text"},Obj).
	#{<<"alignment">> => <<"center">>,
  	  <<"data">> => <<"Click Here">>,
  	  <<"hOffset">> => 250,
  	  <<"name">> => <<"text1">>,
  	  <<"onMouseUp">> => <<"sun1.opacity = (sun1.opacity / 100) * 90;">>,
  	  <<"size">> => 36,
  	  <<"style">> => <<"bold">>,
  	  <<"vOffset">> => 100}
      
set_p creates intermediary nodes: 

	1> jwalk:set_p({"users", {select, {"name", "sebastian"}}, "location"}, #{}, <<"Germany">>).
    #{<<"users">> => [#{<<"location">> => <<"Germany">>,<<"name">> => <<"sebastian">>}]}

	2> jwalk:set_p({"users", {select, {"name", "sebastian"}}, "location"}, [{}],    <<"Germany">>).	[{<<"users">>,
	  [[{<<"name">>,<<"sebastian">>},
            {<<"location">>,<<"Germany">>}]]}]

    3> jwalk:set_p({"users", {select, {"name", "sebastian"}}, "location"}, {[]},    <<"Germany">>).
	{[{<<"users">>,
   	  [{[{<<"name">>,<<"sebastian">>},
             {<<"location">>,<<"Germany">>}]}]}]}

    4> jwalk:set_p({"users", {select, {"name", "sebastian"}}, "location"}, {struct,[]}, <<"Germany">>).
    {struct,[{<<"users">>,
        [{struct,[{<<"name">>,<<"sebastian">>},
                  {<<"location">>,<<"Germany">>}]}]}]}
