#jwalk
##Helper module for working with Erlang Proplist, EEP18 and Map representations of JSON

[![Build Status](https://travis-ci.org/jr0senblum/jwalk.svg)](https://travis-ci.org/jr0senblum/jwalk)
[![hex.pm version](https://img.shields.io/hexpm/v/jwalk.svg)](https://hex.pm/packages/jwalk)

This work is somewhat of a re-write of [ej](https://github.com/seth/ej) but 
focuses on Map, EEP18 and Proplist representations of JSON - the type returned by
[jsone](https://github.com/sile/jsone) or [jiffy](https://github.com/davisp/jiffy)
for example. Anything that is good about this is due to the contributors and 
maintainers of ej, anything bad is completely my fault.

###Dependencies
Other then the below, there are no dependencies. The source code is a single 
file which includes eunit test cases.

* Erlang 17.0 +
* Builds with rebar3 and uses the Hex package manager

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

1> jwalk:get({"one"},#{<<"one">>=>1}).

1


##Functions
Functions always take at least two parameters: a first parameter which is a
tuple of elements representing a Path into a JSON Object, and a second 
parameter which is expected to be a proplist, map or eep18 representation of 
a JSON structure.

* ``jwalk:delete(Path, Obj)`` - Remove the value from Obj, at the location 
specified by Path, and return a structure
* ``jwalk:get(Path, Obj)``, ``jwalk:get(Path, Obj, Default)``  - Return the 
value from Obj, at the specificed Path, or undefined or Default
* ``jwalk:set(Path, Obj, Val)`` - Set a value in an Object, at the specified
path, and return the new structure
* ``jwalk:set_p(Path, Obj, Val)`` - Set a value in an Object, at the specified
path, creating intermediate nodes as necessary and returning the new strcuture

##Paths
Paths into JSON Objects are expressed using a tuple of Path elements, a 
representation of a javascript-like path: i.e.,
``Obj.cars.make.model``  would be expressed as ``{"cars","make","model"}``. 
Path elements representing JSON Member Names can be strings or binary, they 
will be internally converted to binary regardless.

In addition to string/binary representations of Member Names, a Path element can 
be

* An integer index, or the atoms ``first`` and ``last``, which will select 
elements of a JSON Array.
* ``{select, {"name","value"}}`` which will select a subset of JSON objects 
from an Array that have a Member ``{"Name": "Value"}`` 

Examples follow.

###Usage Examples
    Cars = [{<<"cars">>, [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
                           [{<<"color">>, <<"red">>},  {<<"age">>, <<"old">>}],
                           [{<<"color">>, <<"blue">>}, {<<"age">>, <<"new">>}]
                         ]
           }].

 Then 
       
    1> jwalk:get({"cars", {select, {"age", "old"}}}, Cars).
    [[{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
    [{<<"color">>, <<"red">>},   {<<"age">>, <<"old">>}]]

    2> jwalk:get({"cars", {select, {"age", "old"}}, 1}, Cars).
    [{<<"color">>,<<"white">>},{<<"age">>,<<"old">>}]

    3> jwalk:get({"cars", {select, {"age", "old"}},first,"color"}, Cars).
    <<"white">>


Given:

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

or 

    Obj2 = [{<<"menu">>,
         [{<<"id">>,<<"file">>},
          {<<"value">>,<<"File">>},
          {<<"popup">>,
           [{<<"menuitem">>,
             [[{<<"value">>,<<"New">>},{<<"onclick">>,<<"CreateNewDoc()">>}],
              [{<<"value">>,<<"Open">>},{<<"onclick">>,<<"OpenDoc()">>}],
              [{<<"value">>,<<"Close">>},
               {<<"onclick">>,<<"CloseDoc()">>}]]},
            {<<"titleitem">>, []}]}]}].

then: using jwalk:get(Paths, Object)
    
    1> jwalk:get({"widget"}, Obj).
    {<<"debug">> => <<"on">>,
     <<"image">> => #{<<"alignment">> => <<"center">>,
                  <<"hOffset">> => 250,
                  <<"name">> => <<"sun1">>,
                  <<"src">> => <<"Images/Sun.png">>,
                  <<"vOffset">> => 250},
	 <<"keys">> => [],
	 <<"text">> => #{<<"alignment">> => <<"center">>,
    	             <<"data">> => <<"Click Here">>,
        	         <<"hOffset">> => 250,
            	     <<"name">> => <<"text1">>,
                	 <<"onMouseUp">> => <<"sun1.opacity = (sun1.opacity / 100) * 90;">>,
             	    <<"size">> => 36,
                	 <<"style">> => <<"bold">>,
                 	<<"vOffset">> => 100},
 	 <<"values">> => [1,2,3,4,5],
	 <<"version">> => <<"1">>,
 	 <<"window">> => #{<<"height">> => 500,
     	              <<"name">> => <<"main_window">>,
        	           <<"title">> => <<"Sample Konfabulator Widget">>,
            	       <<"width">> => 500}}


	2> jwalk:get({"widget", "values"},Obj).
	[1,2,3,4,5]


	3> jwalk:get({"widget", "values", last},Obj).
	5

	4> jwalk:get({"menu", "popup","menuitem",{select,{"value","New"}}},Obj2).
	[[{<<"value">>,<<"New">>}, {<<"onclick">>,<<"CreateNewDoc()">>}]]

	5> jwalk:get({"menu", "popup","menuitem",{select,{"value","New"}},"onclick"},Obj2).
	[<<"CreateNewDoc()">>]

        6> Path = {"widget","image","newOffset"}.
        {"widget","image","newOffset"}

        7> Widget = 
        #{<<"widget">> => #{<<"debug">> => <<"on">>,
          <<"image">> => #{<<"alignment">> => <<"center">>,
          <<"hOffset">> => 250,
          <<"name">> => <<"sun1">>,
          <<"src">> => <<"Images/Sun.png">>,
          <<"vOffset">> => 250},
          <<"keys">> => [],
          <<"text">> => #{<<"alignment">> => <<"center">>,
          <<"data">> => <<"Click Here">>,
          <<"hOffset">> => 250,
          <<"name">> => <<"text1">>,
          <<"onMouseUp">> => <<"sun1.opacity = (sun1.opacity / 100) * 90;">>,
          <<"size">> => 36,
          <<"style">> => <<"bold">>,
          <<"vOffset">> => 100},
          <<"values">> => [1,2,3,4,5],
          <<"version">> => <<"1">>,
          <<"window">> => #{<<"height">> => 500,
          <<"name">> => <<"main_window">>,
          <<"title">> => <<"Sample Konfabulator Widget">>,
          <<"width">> => 500}}}.

          8> Value = <<"YYY">>.
          <<"YYY">>
          
          9> jwalk:set(Path, Widget, Value).
          #{<<"widget">> => #{<<"debug">> => <<"on">>,
          <<"image">> => #{<<"alignment">> => <<"center">>,
          <<"hOffset">> => 250,
          <<"name">> => <<"sun1">>,
          <<"newOffset">> => <<"YYY">>
          <<"src">> => <<"Images/Sun.png">>,
          <<"vOffset">> => 250},
          <<"keys">> => [],
          <<"text">> => #{<<"alignment">> => <<"center">>,
          <<"data">> => <<"Click Here">>,
          <<"hOffset">> => 250,
          <<"name">> => <<"text1">>,
          <<"onMouseUp">> => <<"sun1.opacity = (sun1.opacity / 100) * 90;">>,
          <<"size">> => 36,
          <<"style">> => <<"bold">>,
          <<"vOffset">> => 100},
          <<"values">> => [1,2,3,4,5],
          <<"version">> => <<"1">>,
          <<"window">> => #{<<"height">> => 500,
          <<"name">> => <<"main_window">>,
          <<"title">> => <<"Sample Konfabulator Widget">>,
          <<"width">> => 500}}}.

          10> jwalk:set_p({"users", {select, {"name", "sebastian"}}, "location"}, [{}],       
          <<"Germany">>).
          [{<<"users">>,
            [[{<<"name">>,<<"sebastian">>},
              {<<"location">>,<<"Germany">>}]]}]

         11> jwalk:set_p({"users", {select, {"name", "sebastian"}}, "location"}, #{}, 
         <<"Germany">>).
          #{<<"users">> => [#{<<"location">> => <<"Germany">>,<<"name">> => <<"sebastian">>}]}
