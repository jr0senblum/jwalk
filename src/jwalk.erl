%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@prodigy.net>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%% The jwalk modue is intended to make it easy to work with the Erlang
%%% encodings of JSON which return either maps or property lists. 
%%%
%%% Currently get/2 is supported which allows one to walk an object and return a
%%% particular value.
%%%
%%% Get takes a Path list and a strcture representing the encoded JSON.
%%%
%%% The Path list can consist of tuples representing a javascript-like path:i.e.
%%%
%%% Obj.cars.make.model would be expressed as {"cars","make","model"}, as in </br>
%%% jwalk:get({"cars","make","model"}, Obj).
%%%
%%% Additionally, the Path tuple can contain: </br>
%%% Elements of a JSON array can be accessed by using the atoms `` 'first' '' and
%%% `` 'last' '' or an integer index </br>
%%%
%%% A subset of JSON objects in an array can be selected using {select, {"name","value"}}
%%% For example
%%% ```
%%% Cars = [{<<"cars">>, [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
%%%                       [{<<"color">>, <<"red">>},  {<<"age">>, <<"old">>}],
%%%                       [{<<"color">>, <<"blue">>}, {<<"age">>, <<"new">>}]
%%%                     ]}].
%%% '''
%%% Then 
%%
%%% ```
%%% jwalk:get({"cars", {select {"age", "old"}}}, Cars).
%%%
%%%  [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
%%%    [{<<"color">>, <<"red">>},   {<<"age">>, <<"old">>}]
%%% ]
%%% '''
%%%
%%% This work is really a rip-off of https://github.com/seth/ej
%%% @end
%%% Created : 20 Nov 2015 by Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%%-----------------------------------------------------------------------------


%%%===================================================================
%%% API
%%%===================================================================

%%%
% Property lists are ordinary lists containing entries in the form of either 
% tuples, whose first elements are keys used for lookup and insertion, or 
% atoms, which work as shorthand for tuples {Atom, true}.
%
% Jsone libraries that convert VALID JSON will result in property lists that are
% of the form </br>
% [{_,_}...] </br>
%

-module(jwalk).

-export([get/2]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(IS_SELECT(K), is_tuple(K) andalso element(1, K) == select).
-define(IS_INDEX(K), K == first orelse K == last orelse is_integer(K) orelse ?IS_SELECT(K)).




get(Keys, Obj) ->
    try 
        walk(to_binary_list(Keys), Obj)
    catch
        throw:R ->
            error(R)
    end.



% If you make it to the end, couldn't find it, its undefined.
walk(_, []) ->
    undefined;

% Target = Object: Name matches the first element of a tuple, continue with
% the rest of the Names and the found Value.
walk([N|Ns], [Target|_]) when is_tuple(Target), N == element(1, Target) ->
    Value = element(2, Target),
    case Ns of
        [] -> 
            Value;
        _More -> 
            walk(Ns, Value)
    end;

% Key is an array selector (index), and Target is not an object. Anything
% else (assuming valid JSON) would be indicitive of an Array: Array, null,
% number, string, true or false.
walk([I|Ns], [Elt|_]=Ts) when (not is_tuple(Elt)), ?IS_INDEX(I) ->
    Element = index_to_element(I, Ts),
    case Ns of
        [] ->
            Element;
        _More ->
            walk(Ns, Element)
    end;

% Key is an array selector (index), but Target is not even a list.
walk([I|_], null) when ?IS_INDEX(I) ->
    undefined;
walk([I|_], T) when ?IS_INDEX(I) ->
    throw({index_for_non_list, T});

% We have a Name, and an Array, Name could be located in any object within the 
% Array.
walk([K|Ks], [Elt|_]=Ts) when (not is_tuple(Elt)) ->
    Values = make_array(all_successes([K], Ts)),
    case Ks of
        [] ->
            Values;
        _More ->
            walk(Ks, Values)
    end;
                

walk(_, null) -> undefined;

% Path could not be satisfied by first element of array, try the next.
walk(Keys, [_|Tl]) ->
    walk(Keys, Tl).

% Result is supposed to be objects subsetted out of an Array, so
% need to make sure that we have a legitimate array of results.

make_array([]) ->
    undefined;

make_array(H) ->
    A = lists:flatten(H),
    case A of
        [{_,_}|_]=Objs ->
            [Objs];
        _ ->
            A
    end.

all_successes(_Ks, []) ->
    [];
all_successes(Ks, [H|Tl]) ->
    try walk(Ks, H) of
        undefined -> [undefined|all_successes(Ks, Tl)];
        Result -> [Result|all_successes(Ks, Tl)]
    catch 
        _:_ -> all_successes(Ks, Tl)
    end.
                  

            


index_to_element({select, {K,V}}, L) ->
    F = fun(PList) -> 
                proplists:lookup(K, PList) == {K, V} end,
    lists:filter(F, L);
index_to_element(first, L) ->
    hd(L);
index_to_element(last, L) ->
    lists:last(L);
index_to_element(N, L)  ->
    lists:nth(N, L).


make_binary(K) when is_binary(K); is_number(K) -> K;
make_binary(K) when is_list(K) -> list_to_binary(K);
make_binary(K) when is_atom(K) -> K;
make_binary({select, {K, V}}) ->
    {select, {make_binary(K), make_binary(V)}}.

to_binary_list(Keys) ->
    L = tuple_to_list(Keys),
    lists:map(fun(K) -> make_binary(K) end, L).

                 
    
    
    

