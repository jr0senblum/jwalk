#jwalk
##Helper module for working with Erlang Proplist and Map representations of JSON

[![Build Status](https://travis-ci.org/jr0senblum/jwalk.svg)](https://travis-ci.org/jr0senblum/jwalk)

This work is a partial re-wrte of [ej](https://github.com/seth/ej) but focuses 
on Map and Proplist representations of JSON - the type returned by
[jsone](https://github.com/sile/jsone), for example. Anything that is good about
this is due to him, anything bad is completely my fault.
###Dependencies
Erlang 17.0 +

Builds with rebar3 or Hex

No other dependencies, the src code is a single file.

###Functions
The following functions are implemented where Path is a tuple representation of a 
javascript-like path (see below) and Obj is a Map or Proplist representation of JSON:

* ``jwalk:delete/(Path, Obj)``, ``jwalk:delete(Path, Obj, proplist)`` - Remove the value 
from Obj, at the location specified by Path, and return a new Map or Proplist.
* ``jwalk:get(Path, Obj)``, ``jwalk:get(Path, Obj, Default)``  - Return the value from Obj
at the specificed Path, or undefined (or Default).
* ``jwalk:set(Path, Obj, Val)``, ``jwalk:set(Path, Obj, Val, proplist)`` - Set a value 
in an Object.
* ``jwalk:p_set(Path, Obj, Val)``, ``jwalk:_set/(Path, Obj, Val, proplist)`` - Set a 
value in an Object creating intermediate nodes as necessary.

Notice that delete/3, set/3, set_p/4 take a final parameter, the atom 
'proplist', and return Proplist representations. This is necessary because certain
certain uses of these functions are ambiguous with respect to whether Map or Proplist representations are being contemplated by the user. The sister functions/ delete/2, 
set/2 and set_p/3, assume Map representations.

###Paths
In jwalk, paths into JSON objects are expressed using a tuple of keys or Path elements.

The Path elements can be thought of as a tuple represention of a javascript-like 
path: i.e.,

``Obj.cars.make.model``  would be expressed as ``{"cars","make","model"}`` , as in
``jwalk:get({"cars","make","model"}, Obj)``.

In addition to Names, a Path element can be

* An integer index, or the atoms ``first`` and ``last`` which will select elements of a 
JSON ARRAY 
* ``{select, {"name","value"}}`` which will select a subset of JSON objects from an
Array having a Member ``{"Name": Value}`` 

For example

	Cars = [{<<"cars">>, [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
                           [{<<"color">>, <<"red">>},  {<<"age">>, <<"old">>}],
                           [{<<"color">>, <<"blue">>}, {<<"age">>, <<"new">>}]
                         ]
           }].

 Then 

	1> jwalk:get({"cars", {select {"age", "old"}}}, Cars).
	 [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
       [{<<"color">>, <<"red">>},   {<<"age">>, <<"old">>}]
     ]

###Usage Examples
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

          10> jwalk:set_p({"users", {select, {"name", "sebastian"}}, "location"}, [], <<"Germany">>, proplist).
          [{<<"users">>,
            [[{<<"name">>,<<"sebastian">>},
              {<<"location">>,<<"Germany">>}]]}]

          11> jwalk:set_p({"users", {select, {"name", "sebastian"}}, "location"}, #{}, <<"Germany">>).
          #{<<"users">> => [#{<<"location">> => <<"Germany">>,<<"name">> => <<"sebastian">>}]}





