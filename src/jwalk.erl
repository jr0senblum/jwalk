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

-export([get/2, get/3]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(IS_SELECTER(K), 
        ((K == first) orelse
         (K == last) orelse 
         is_integer(K) orelse
         (is_tuple(K) andalso (element(1, K) == select)))).


-type name()    :: binary() | string().
-type value()   :: binary() | string().
-type select()  :: {select, {name(), value()}}.
-type keylist() :: name() | select() | 'first' | 'last' | non_neg_integer().
-type keys()    :: {keylist()}.
-type obj()     :: map() | list().

-type jwalk_return() :: map() | list() | undefined | [] | no_return().
-export_type ([jwalk_return/0]).

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
%% Obj.cars.make.model would be expressed as {"cars","make","model"}, as in 
%%
%% jwalk:get({"cars","make","model"}, Obj).
%%
%% Additionally, the Path tuple can contain:
%%
%% Elements of a JSON array can be accessed by using the atoms `` 'first' '' and
%% `` 'last' '' or an integer index
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
%% @doc Same as {@link get/2. get}, but returns default if undefined.
%%
-spec get(keys(), obj(), any()) -> jwalk_return().

get(Keys, Obj, Default) ->
    case get(Keys, Obj) of
        undefined ->
            Default;
        Found ->
            Found
    end.




%% -----------------------------------------------------------------------------
%%                INTERNAL FUNCTIONS
%% -----------------------------------------------------------------------------


% Selecting a subset of an empty list is an empty list.
walk([{select, {_,_}}|_], []) ->
    [];

% Applying a Path to an empty list means the Path doesn't exist; thus undefined.
walk(_, []) ->
    undefined;

% Any key applied to null is undefined.
walk(_, null) -> undefined;

% Target = [{k,v}..] = OBJECT: if Name matches Object's first Member's Name, 
% continue with the rest of the keys and the found Value.
walk([Name|Keys], [{Name, Value}|_]) when not ?IS_SELECTER(Name)->
    case Keys of
        [] -> 
            Value;
        _More -> 
            walk(Keys, Value)
    end;

% Target = map = OBJECT: if Name is a map-key, continue with the rest of the 
% keys and the found Value.
walk([Name|Keys], #{}=Object) when not ?IS_SELECTER(Name) ->
    case maps:get(Name, Object, undefined) of
        undefined -> undefined;
        Value ->
            case Keys of
                [] -> 
                    Value;
                _More -> 
                    walk(Keys, Value)
            end
    end;

% Target is an OBJECT, but we have a selecter wanting an Array.
walk([S|_], [{_,_}|_]=T) when ?IS_SELECTER(S)->
    throw({index_for_non_list, T});

% Target is a proplist representation of an OBJECT, but Keys could not be 
% satisfied by Objects first Member, try the next Member. We don't have this 
% use-case with Map representations because of the nature of maps:get/3.
walk(Keys, [{_,_}|Tl]) ->
    walk(Keys, Tl);

% Target is an ARRAY, access it via selector.
walk([S|Keys], [_|_]=Array) when ?IS_SELECTER(S) ->
    Element = selector_to_element(S, Array),
    case Keys of
        [] ->
            Element;
        _More ->
            walk(Keys, Element)
    end;

% Target is something other than an Array, but we have a selector.
walk([S|_], T) when ?IS_SELECTER(S) ->
    throw({index_for_non_list, T});

% Target is an ARRAY, key is a Name, return the subset of Objects from the 
% Array that has a Member with the given Name.
walk([Name|Keys], [_|_]=Ts) ->
    Values = having_element(Name, Ts),
    case Keys of
        [] ->
            Values;
        _More ->
            walk(Keys, Values)
    end.
                

% Get all Elements from an Array that have a Member with the Name indicated by 
% the Key.
having_element(Key, Array) ->
    Elements = [walk([Key], Obj) ||
        Obj <- Array, not is_atom(Obj), not is_number(Obj)],
    case Elements of
        [] -> undefined;
        _ -> dont_nest(Elements)
    end.


% Make sure that serial {select, {_,_}} don't give us nested lists of returns.
dont_nest(H) -> 
    A = lists:flatten(H),
    case A of
        [{_,_}|_]=Objs ->
            [Objs];
        _ ->
            A
    end.

            
selector_to_element({select, {K,V}}, [#{}|_]=L) ->
    F = fun(Map) -> maps:get(K, Map, false) == V end,
    lists:filter(F, L);

selector_to_element({select, {K,V}}, L) ->
    F = fun(PList) -> proplists:lookup(K, PList) == {K, V} end,
    lists:filter(F, L);

selector_to_element(first, L) ->
    hd(L);

selector_to_element(last, L) ->
    lists:last(L);

selector_to_element(N, L)  ->
    lists:nth(N, L).



to_binary_list(Keys) ->
    L = tuple_to_list(Keys),
    lists:map(fun(K) -> make_binary(K) end, L).


make_binary(K) when is_binary(K); is_number(K) -> K;
make_binary(K) when is_list(K) -> list_to_binary(K);
make_binary(K) when is_atom(K) -> K;
make_binary({select, {K, V}}) -> 
    {select, {make_binary(K), make_binary(V)}}.



                 
    
    
    

