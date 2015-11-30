%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@prodigy.net>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%% The jwalk modue is intended to make it easier to work with Erlang
%%% encodings of JSON - either MAPS or PROPERTY LISTS. 
%%%
%%% This work is really a rip-off of https://github.com/seth/ej.
%%%
%%% Functions take a tuple, where the elements of the tuple represent a Path 
%%% into a JSON Object, and an encoded JSON structure (Proplist or Map) and return or 
%%% delete the values indicated by the Path.
%%%
%%% The Path components are tuples representing a javascript-like, path: i.e.,
%%%
%%% Obj.cars.make.model would be expressed as {"cars","make","model"}
%%%
%%% Additionally, a Path element may contain:
%%%
%%% The atoms `` 'first' '' and `` 'last' '' or an integer index indicating an 
%%% element from a JSON array; or,
%%%
%%% {select, {"name","value"}} which will return a subset of JSON objects in 
%%% an Array. For example
%%% ```
%%% Cars = [{<<"cars">>, [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
%%%                       [{<<"color">>, <<"red">>},  {<<"age">>, <<"old">>}],
%%%                       [{<<"color">>, <<"blue">>}, {<<"age">>, <<"new">>}]
%%%                     ]}].
%%% '''
%%% Then 
%%%
%%% ```
%%% jwalk:get({"cars", {select {"age", "old"}}}, Cars).
%%%
%%%  [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
%%%    [{<<"color">>, <<"red">>},   {<<"age">>, <<"old">>}]
%%% ]
%%% '''
%%% @end
%%% Created : 20 Nov 2015 by Jim Rosenblum <jrosenblum@prodigy.net>
%%% ----------------------------------------------------------------------------
-module(jwalk).

-export([delete/2, delete/3,
         get/2, get/3,
         set/3, set/4,
         set_p/3, set_p/4]).



-define(IS_OBJ(X), (is_map(X) orelse (is_list(X) andalso is_tuple(hd(X))))).
-define(IS_PL(X), (X == [] orelse (is_list(X) andalso is_tuple(hd(X))))).

-define(IS_J_TERM(X), 
        (?IS_OBJ(X) orelse 
        is_number(X) orelse 
        (X == true) orelse
        (X == false) orelse 
         is_binary(X) orelse
        (X == null))).

-define(IS_SELECTOR(K), 
        ((K == first) orelse
         (K == last) orelse 
         (K == new) orelse
         is_integer(K) orelse
         (is_tuple(K) andalso (element(1, K) == select)))).



-type name()     :: binary() | string().
-type value()    :: binary() | string().
-type select()   :: {select, {name(), value()}}.
-type path_elt() :: name() | select() | 'first' | 'last' | non_neg_integer().
-type path()     :: {path_elt()}.
-type obj()      :: map() | list(tuple()) | [].


-type jwalk_return() :: map() | obj() | [map(),...] | [obj(),...] | undefined.


-export_type ([jwalk_return/0]).



%% ----------------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% @doc Remove the value at the location specified by `Path' and return the
%% new Map representation.
-spec delete(path(), obj()) -> jwalk_return().

delete(Path, Obj) ->
    do_set(to_binary_list(Path), Obj, delete, [], false, true).



%% -----------------------------------------------------------------------------
%% @doc Remove the value at the location specified by `Path' and return the
%% new Propertylist representation.
-spec delete(path(), obj(), proplist) -> jwalk_return().

delete(Path, Obj, proplist) ->
    do_set(to_binary_list(Path), Obj, delete, [], false, false).



%% -----------------------------------------------------------------------------
%% @doc Return a value from 'Obj'.
%% 
-spec get(path(), obj()) -> jwalk_return().

get(Path, Obj) ->
    try 
        walk(to_binary_list(Path), Obj)
    catch
        throw:Error ->
            error(Error)
    end.



%% -----------------------------------------------------------------------------
%% @doc Same as {@link get/2. get/2}, but returns default if `Path' does not
%% exist in `Obj'.
%%
-spec get(path(), obj(), any()) -> jwalk_return().

get(Path, Obj, Default) ->
    case get(Path, Obj) of
        undefined ->
            Default;
        Found ->
            Found
    end.



%% -----------------------------------------------------------------------------
%% @doc Set a value in `Obj', assumes Obj is (or will be) a Map.
%%
%% Replaces the value at the specified `Path' with `Value' and
%% returns the new structure. If the final element of the Path, does not exist,
%% it will be created. 
%%
%% The atom, `new', applied to an ARRAY, will create the Element as the first
%% element in the Array.
%%
-spec set(path(), obj(), value()) -> jwalk_return().

set(Path, Obj, Element) ->
    do_set(to_binary_list(Path), Obj, Element, [], false, true).


%% -----------------------------------------------------------------------------
%% @doc Set a value in `Obj', Obj is (or will be) Proplist representation.
%%
-spec set(path(), obj(), value(), proplist) -> jwalk_return().

set(Path, Obj, Element, proplist) ->
    do_set(to_binary_list(Path), Obj, Element, [], false, false).



%% -----------------------------------------------------------------------------
%% @doc Set a value in `Obj'. Same as set/3, but creates intermediary elements 
%% if necessary. Assumes Obj is (or will be) a Map
%%
-spec set_p(path(), obj(), value()) -> jwalk_return().

set_p(Path, Obj, Element) ->
    do_set(to_binary_list(Path), Obj, Element, [], true, true).


%% -----------------------------------------------------------------------------
%% @doc Set a value in `Obj'. Same as set/3, but creates intermediary elements 
%% if necessary. Assumes Obj is (or will be) a Proplist.
%%
-spec set_p(path(), obj(), value(), proplist) -> jwalk_return().

set_p(Path, Obj, Element, proplist) ->
    do_set(to_binary_list(Path), Obj, Element, [], true, false).



do_set(Path, Obj,Element, Acc, P, IsMap) ->
    try 
        set_(Path, Obj, Element, Acc, P, IsMap)
    catch
        throw:Error ->
            error(Error)
    end.



%% -----------------------------------------------------------------------------
%%                WALK, and SET INTERNAL FUNCTIONS
%% -----------------------------------------------------------------------------


% Selecting a subset of an empty list is an empty list.
walk([{select, {_,_}}|_], []) ->
    [];

% Applying a Path to an empty list means the Path doesn't exist; thus undefined.
walk(_, []) ->
    undefined;

% Any Path applied to null is undefined.
walk(_, null) -> undefined;

% Target = [{k,v}..] = OBJECT: if Member exists in Object continue with that
% Members Value.
walk([Name|Path], [{_, _}|_]=Object) when not ?IS_SELECTOR(Name)->
    case lists:keyfind(Name, 1, Object) of 
        false ->
            undefined;
        {Name, Value} when Path /= []->
            walk(Path, Value);
        {Name, Value} ->
            Value
    end;

% Target = map = OBJECT: if Member exists in Object continue with that Members
% Value.
walk([Name|Path], #{}=Object) when not ?IS_SELECTOR(Name) ->
    case maps:get(Name, Object, undefined) of
        undefined -> undefined;
        Value when Path /= [] ->
            walk(Path, Value);
        Value ->
            Value
    end;

% Target is an OBJECT, but we have a selector wanting an Array.
walk([S|_], Obj) when ?IS_SELECTOR(S), ?IS_OBJ(Obj)->
    throw({index_for_non_list, Obj});

% Target = ARRAY, access it via selector and continue with the rest of the Path
% and the returned Object or Array.
walk([S|Path], [_|_]=Array) when ?IS_SELECTOR(S) ->
    case selector_to_element(S, Array) of
        Element when Path /=[] ->
            walk(Path, Element);
        Element ->
            Element
    end;

% Target = ARRAY, Path element is a Member Name and is used to collect all
% Values from Members of Objects in the Array with name = Name.
walk([Name|Path], [_|_]=Ts) ->
    case elements_with(Name, Ts) of
        Values when Path /= [] ->
            walk(Path, Values);
        Values ->
            Values
    end;

% Target is something other than an ARRAY, but we have a selector.
walk([S|_], T) when ?IS_SELECTOR(S) ->
    throw({index_for_non_list, T}).



% Final Path element: if it existis in the OBJECT delete it.
set_([Name], Obj, delete, _Acc, _IsP, _IsMap) when not ?IS_SELECTOR(Name) andalso ?IS_PL(Obj) ->
    lists:keydelete(Name, 1, Obj);

% Final Path element: if it existis in the OBJECT replace or create it.
set_([Name], Obj, Val, _Acc, _IsP, false) when not ?IS_SELECTOR(Name) andalso ?IS_PL(Obj) ->
    lists:keystore(Name, 1, Obj, {Name, Val});

% Find targetd OBJECT's Member, replace it with recursive call.
set_([Name|Ks]=Path, [{N, V}|Ms], Val, Acc, IsP, false) when not ?IS_SELECTOR(Name)->
    case Name of
        N ->
            NewVal = set_(Ks, V, Val, [], IsP, false),
            lists:append(lists:reverse(Acc),[{N, NewVal}|Ms]);
        _Other ->
            set_(Path, Ms, Val, [{N,V}|Acc], IsP, false)
    end;

% Intermediate Path element does not exist either create it or throw.
set_([Name|Ks], [], Val, Acc, IsP, false) when not ?IS_SELECTOR(Name) ->
    case IsP of
        true -> 
            lists:append(lists:reverse(Acc), 
                         [{Name, set_(Ks,[], Val, [], IsP, false)}]);
        _ -> 
            throw({no_path, Name})
    end;

% Final Path element: if it existis in the OBJECT delete it.
set_([Name], Map, delete, _Acc, _IsP, _IsMap) when not ?IS_SELECTOR(Name) andalso is_map(Map)->
    maps:remove(Name, Map);

% Last part of the Path found, if it exists in the OBJECT replace or create it.
set_([Name], Map, Val, _Acc, _P, true) when not ?IS_SELECTOR(Name) andalso is_map(Map)->
    maps:put(Name, Val, Map);

% Find Object's Member with Path Name, that Members' Value will be thre result
% of the recursive call of set_ on the Value with the ballance of the Path. If 
% not there, either create it or throw.
set_([Name|Ks], Map, Val, _Acc, P, true) when not ?IS_SELECTOR(Name) andalso is_map(Map)->
    case maps:get(Name, Map, not_found) of
        not_found -> 
            case P of
                true ->
                    maps:put(Name, set_(Ks, #{}, Val, [], P, true), Map);
                _ -> 
                    throw({no_path, Name})
            end;
        Value -> 
            maps:put(Name, set_(Ks, Value, Val, [], P, true), Map)
    end;

% When final Path elemenet is NEW, just return the Element in an Array.
set_([new], #{}, Element, _Acc, _P, _IsMap) when ?IS_J_TERM(Element) ->
    [Element];

set_([new], [], Element, _Acc, _P, _IsMap)  when ?IS_J_TERM(Element) ->
    [Element];

set_([S|_], Obj, _V, _A, _P, _IsMap) when ?IS_SELECTOR(S) andalso ?IS_OBJ(Obj)->
    throw({selector_used_on_object, S, Obj});
    

% ALL OBJECT CASES HANDLED ABOVE %

% The atom, new, applied to an ARRAY creates Element as the first element in 
% Array.
set_([new], Array, Element, _Acc, _P, _IsMap) when is_list(Array) ->
    [Element|Array];

set_([{select,{_,_}}=S], Array, delete, _Acc, _IsP, _IsMap) ->
    Found = selector_to_element(S, Array),
    remove(Array, Found);

% The final path element is a 'select by member' applied to an ARRAY. Set the 
% selected Objects with theTarget
set_([{select,{K,V}}=S], Array, Target, _Acc, _P, IsMap) when ?IS_OBJ(Target), is_list(Array) ->
    Found = selector_to_element(S, Array),
    Replace = case Found of
                  [] when IsMap ->
                       [maps:merge(#{K =>V}, Target)];
                  []  -> 
                      [[{K,V} | Target]];
                  Found ->
                      merge_members(Found, Target)
              end,
    lists:append(remove(Array, Found), Replace);
    


set_([{select,{_,_}}], _Array, Target, _Acc, _P, _IsMap) ->
    throw({replacing_object_with_value, Target});

% A path element is a select_by_member applied to an ARRAY. Replace the selected
% Objects with the recursive call on the rest of the path and the selected 
% elements.
set_([{select,{K,V}}=S|Ks], Array, Target, _Acc, P, IsMap) ->
    Found = selector_to_element(S, Array),
    Objects = case Found of
                  [] when P andalso IsMap ->
                       [#{K =>V}];
                  [] when P ->
                      [[{K,V}]];
                  [] -> 
                    throw({no_path, S});
                  _ ->
                      Found
              end,
    Replaced = set_(Ks, Objects, Target, [], P, IsMap),
    lists:append(remove(Array, Found), Replaced);


% Path component is index, either recurse on the selected Object, or replace it.
set_([S|_], Array, _Element, _Acc, _P, _IsMap) when not is_list(Array), ?IS_SELECTOR(S) ->
    throw({selector_used_on_non_array, S, Array});

set_([S|Path], Array, Element, _Acc, P, IsMap) when is_integer(S); S == last; S == first ->
    N = index_to_n(Array, S),

    case Path of
        [] ->
            lists:sublist(Array, 1, min(1, N-1)) ++
                [Element] ++  
                lists:sublist(Array, N + 1, length(Array));
        _More when P ; N =<length(Array) ->
            lists:sublist(Array, 1, min(1, N-1)) ++
                [set_(Path, lists:nth(N, Array), Element, [], P, IsMap)] ++
                lists:sublist(Array, N + 1, length(Array));
        _More ->
            throw({no_path, S})
    end;


% Final Path component is a Name, target is an ARRAY, Delete selected Objects
% pulled from the Array.
set_([Name], [_|_]=Array, delete, _Acc, _IsP, _IsMap) ->
    F = fun(#{}=Map)      -> maps:remove(Name, Map);
           ([{_,_}|_]=Pl) -> lists:keydelete(Name, 1, Pl)
        end,

    [F(O) || O <- Array];


% Final Path component is a Name, target is an ARRAY, replace/add Member to all
% selected Objects pulled from the Array.
set_([Name], [_|_]=Array, Element, _Acc, _P, IsMap) ->
    case found_elements(Name, Array) of
        undefined when IsMap -> 
             merge_members(Array, #{Name => Element});
        undefined -> 
            merge_members(Array, [{Name, Element}]);
        ObjectSet -> 
            case ?IS_OBJ(Element) of 
                true ->
                    merge_members(remove(Array, ObjectSet), Element);
                false ->
                    throw({replacing_object_with_value, Element})
            end
    end;

% Path component is a Name, target is an ARRAY,  Set will recursively process
% the selected objects containing a Member with name Name, with the ballance
% ballan%% ce of the Path. 
set_([Name|Keys], Array, Element, _Acc, P, IsMap) ->
    case found_elements(Name, Array) of
        undefined ->
            throw({no_path, Name});
        ObjectSet -> 
            NewObjSet = set_(Keys, ObjectSet, Element, [], P, IsMap),
            merge_members(remove(Array,ObjectSet), NewObjSet)
                
    end.



%% ----------------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%% ----------------------------------------------------------------------------


found_elements(Name, Array) ->
    case elements_with(Name, Array) of
        undefined -> undefined;
        EList -> 
            case lists:filter(fun(R) -> R /= undefined end, EList) of
                [] -> undefined;
                Elements ->
                    Elements
            end
    end.
             

merge_members([#{}|_] = Maps, Target) ->
    [maps:merge(M, Target) || M <- Maps];

merge_members(Objects, M) ->
    [merge_pl(O, M) || O <- Objects].

merge_pl(P1, [{K,V}|Ts]) ->
    merge_pl(lists:keystore(K, 1, P1, {K,V}), Ts);
merge_pl(P1, []) ->
    P1.


remove([], Remove) -> Remove;
remove(Objects, []) -> Objects;
remove(Objects, Remove) -> 
    lists:reverse(ordsets:to_list(
                    ordsets:subtract(ordsets:from_list(Objects),
                                     ordsets:from_list(Remove)))).


% Return an Array of Elements, from Array, where the Element exists at Path.
elements_with(Path, Array) ->
    Elements = [walk([Path], Obj) || Obj <- Array, ?IS_OBJ(Obj)],
    case Elements of
        [] -> undefined;
        _ -> dont_nest(Elements)
    end.


% Make sure that we always return an Array of Objects.
dont_nest(H) -> 
    A = lists:flatten(H),
    case A of
        [{_,_}|_]=Obj ->
            [Obj];
        {} = Obj ->
            [Obj];
        _ ->
            A
    end.


% Select out a subset of Objects
selector_to_element({select, {K,V}}, [{_,_}|_]=L) ->
    case proplists:lookup(K, L) of
        {K,V} -> [L];
        _ -> []
    end;

selector_to_element({select, {K,V}}, #{}=L) ->
    case maps:get(K, L, jwalk_false) of
        V -> [L];
        _ -> []
    end;

selector_to_element({select, {K,V}}, Array) ->
    F = fun(Obj) when ?IS_OBJ(Obj) -> 
                selector_to_element({select, {K,V}}, Obj) /= [];
           (_) -> false
        end,
    lists:filter(F, Array);

selector_to_element(first, L) ->
    hd(L);

selector_to_element(last, L) ->
    lists:last(L);

selector_to_element(N, L)  ->
    lists:nth(N, L).


index_to_n(_Array, first) -> 1;
index_to_n(Array, last) -> length(Array);
index_to_n(_Array, Integer) -> Integer.


to_binary_list(Keys) ->
    L = tuple_to_list(Keys),
    lists:map(fun(K) -> make_binary(K) end, L).


make_binary(K) when is_binary(K); is_number(K) -> K;
make_binary(K) when is_list(K) -> list_to_binary(K);
make_binary(K) when is_atom(K) -> K;
make_binary({select, {K, V}}) -> 
    {select, {make_binary(K), make_binary(V)}}.


