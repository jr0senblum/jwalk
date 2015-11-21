# jwalk
Helper module for working with Erlang proplist and map representations of JSON

This work is a partial re-wrte of [[https://github.com/seth/ej][ej] but focuses 
on Map and Proplist representations of JSON like what is returned by
[[https://github.com/sile/jsone][jsone]]. Quite frankly, I have lifted a lot of
his verbiage and tried to follow his API.

Currently, =jwalk:get/2= has been implemented and supports both proplists and maps.
In jwalk, paths into JSON objects are expressed using a tuple of keys
like so:

| Javascript              | jwalk                                      |
| =Obj.author.name.first= | =jwalk:get({"author", "name", "first"}, Obj)= |

Given:
#+BEGIN_SRC erlang
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

#+END_SRC            

#+BEGIN_SRC txt
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

#+END_SRC




