%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@prodigy.net>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%% The jwalk modue is intended to make it easy to work with the Erlang
%%% encodings of JSON which return either MAPS or PROPERTY lists. 
%%%
%%% This work is really a rip-off of https://github.com/seth/ej
%%% @end
%%% Created : 20 Nov 2015 by Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% ----------------------------------------------------------------------------
-module(jwalk).

-export([get/2]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(IS_INDEX(K), 
        ((K == first) orelse
        (K == last) orelse 
        (is_integer(K)) orelse
        (is_tuple(K) andalso (element(1, K) == select)))).


-type name()  :: binary() | string().
-type value() :: binary() | string().
-type key()   :: {name(), value()} | 'first' | 'last' | non_neg_integer().
-type keys()  :: [key()].
-type obj()   :: map() | list().
-type jwalk_return() :: map() | list() | undefined | [] | no_return().


%% ----------------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% @doc Currently get/2 is supported which allows one to walk an object and 
%% return a particular value.
%%
%% Get takes a Path list and a strcture representing the encoded JSON.
%%
%% The Path list can consist of tuples representing a javascript-like 
%% path: i.e.,
%%
%% Obj.cars.make.model would be expressed as {"cars","make","model"}, as in </br>
%% jwalk:get({"cars","make","model"}, Obj).
%%
%% Additionally, the Path tuple can contain: </br>
%% Elements of a JSON array can be accessed by using the atoms `` 'first' '' and
%% `` 'last' '' or an integer index </br>
%%
%% A subset of JSON objects in an array can be selected using {select, {"name","value"}}
%% For example
%% ```
%% Cars = [{<<"cars">>, [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
%%                       [{<<"color">>, <<"red">>},  {<<"age">>, <<"old">>}],
%%                       [{<<"color">>, <<"blue">>}, {<<"age">>, <<"new">>}]
%%                     ]}].
%% '''
%% Then 
%%
%% ```
%% jwalk:get({"cars", {select {"age", "old"}}}, Cars).
%%
%%  [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
%%    [{<<"color">>, <<"red">>},   {<<"age">>, <<"old">>}]
%% ]
%% '''
%%
-spec get(keys(), obj()) -> jwalk_return().

get(Keys, Obj) ->
    try 
        walk(to_binary_list(Keys), Obj)
    catch
        throw:R ->
            error(R)
    end.



%% -----------------------------------------------------------------------------
%%                INTERNAL FUNCTIONS
%% -----------------------------------------------------------------------------


% Selecting a subset of an empty list is an empty list...
walk([{select, {_,_}}|_], []) ->
    [];

% otherwise, it means we are udefined
walk(_, []) ->
    undefined;

% Target = Object: Name matches the first element of a tuple, continue with
% the rest of the Names and the found Value.
walk([N|Keys], [Target|_]) when is_tuple(Target), N == element(1, Target) ->
    Value = element(2, Target),
    case Keys of
        [] -> 
            Value;
        _More -> 
            walk(Keys, Value)
    end;


% Target = Object: Name matches the first element of a tuple, continue with
% the rest of the Names and the found Value.
walk([N|Keys], #{}=M) when (not ?IS_INDEX(N)) ->
    case maps:get(N, M, undefined) of
        undefined -> undefined;
        Value ->
            case Keys of
                [] -> 
                    Value;
                _More -> 
                    walk(Keys, Value)
            end
    end;




% Key is an array selector (index), and the first element isn't {name, value}, so
% assuming valid JSON, we would have an Array, not an Object.
walk([I|Keys], [Elt|_]=Target) when (not is_tuple(Elt)),  ?IS_INDEX(I) ->
    Element = index_to_element(I, Target),
    case Keys of
        [] ->
            Element;
        _More ->
            walk(Keys, Element)
    end;

% Key is an array selector (index), Target is either an Object or not a list.
walk([I|_], T) when ?IS_INDEX(I) ->
    throw({index_for_non_list, T});

% Key is an array selector (index), but Target is null (missing value).
walk([I|_], null) when ?IS_INDEX(I) ->
    undefined;


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
                
% Any selecter applied to null is undefined.
walk(_, null) -> undefined;

% Path could not be satisfied by first element of list, try the next.
walk(Keys, [_|Tl]) ->
    walk(Keys, Tl).


% Used to make sure we signal undefined or have an Array - used when trying to
% get a subset of an Array of Objects, so that better be an Array.
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

% Get all elements from an Array that have the target indicated by the Keys.
all_successes(_Ks, []) ->
    [];
all_successes(Ks, [H|Tl]) ->
    try walk(Ks, H) of
        undefined -> [undefined|all_successes(Ks, Tl)];
        Result -> [Result|all_successes(Ks, Tl)]
    catch 
        _:_ -> all_successes(Ks, Tl)
    end.
                  

            
index_to_element({select, {K,V}}, [#{}|_]=L) ->
    F = fun(Map) -> 
                maps:get(K, Map, false) == V end,
    lists:filter(F, L);
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

                 
    
    
    

