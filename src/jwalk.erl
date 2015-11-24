%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@prodigy.net>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%% The jwalk modue is intended to make it easier to work with Erlang
%%% encodings of JSON - either MAPS or PROPERTY lists. 
%%%
%%% This work is really a rip-off of https://github.com/seth/ej
%%% Currently get/2, get/3 and set/3 are supported.
%%% @end
%%% Created : 20 Nov 2015 by Jim Rosenblum <jrosenblum@prodigy.net>
%%% ----------------------------------------------------------------------------
-module(jwalk).

-export([get/2, get/3,
         set/3,
         set_p/3]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-define(IS_OBJ(X), is_map(X) orelse (is_list(X) andalso is_tuple(hd(X)))).
-define(IS_PL(X), X == [] orelse (is_list(X) andalso is_tuple(hd(X)))).
-define(IS_SELECTOR(K), 
        ((K == first) orelse
         (K == last) orelse 
         is_integer(K) orelse
         (is_tuple(K) andalso (element(1, K) == select)))).


-type name()    :: binary() | string().
-type value()   :: binary() | string().
-type select()  :: {select, {name(), value()}}.
-type keylist() :: name() | select() | 'first' | 'last' | non_neg_integer().
-type keys()    :: {keylist()}.
-type obj()     :: map() | list(tuple()).

-type jwalk_return() :: map() | obj() | [map()] | [obj()] | undefined.

-export_type ([jwalk_return/0]).

%% ----------------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% @doc Return a value from 'Obj'.
%% Get takes a tuple, where the elements represent a Path into a  JSON Object,
%% and an encoded JSON structure (proplist or map) and returns the value 
%% indicated by the Path.
%%
%% The Path components are tuples representing a javascript-like, path: i.e.,
%%
%% Obj.cars.make.model would be expressed as {"cars","make","model"}, as in 
%%
%% jwalk:get({"cars","make","model"}, Obj).
%%
%% Additionally, a Path element may contain:
%%
%% The atoms `` 'first' '' and `` 'last' '' or an integer index indicating an 
%% elements from a JSON array; or,
%%
%% A subset of JSON objects in an Array can be selected using {select, {"name","value"}}
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
%% @doc Same as {@link get/2. get/2}, but returns default if undefined.
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
%% @doc Set a value in `Obj'.
%% Replaces the value at the path specified by `Keys' with `Value' and
%% returns the new structure. If the final element of the Path, does not exist,
%% it will be created. The atom, new, applied to an ARRAY, will create the Element 
%% as the first element in the Array.
%%
-spec set(keys(), obj(), value()) -> jwalk_return().

set(Keys, Obj, Element) ->
    IsMap = is_map(Obj) orelse (is_list(Obj) andalso is_map(hd(Obj))),
    try 
        set_(to_binary_list(Keys), Obj, Element, [], false, IsMap)
    catch
        throw:R ->
            error(R)
    end.

%% -----------------------------------------------------------------------------
%% @doc Set a value in `Obj'. Same as set/3, but creats intermediary Elements 
%% if necessary.
%%
-spec set_p(keys(), obj(), value()) -> jwalk_return().

set_p(Keys, Obj, Element) ->
    IsMap = is_map(Obj) orelse (is_list(Obj) andalso length(Obj) > 0 andalso is_map(hd(Obj))),
    try 
        set_(to_binary_list(Keys), Obj, Element, [], true, IsMap)
    catch
        throw:R ->
            error(R)
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

% Target = ARRAY, Path element (Name) is used to get all Values from Members
% of Objects in the Array with name = Name.
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



                


% Last part of the Path found, if it existis in the OBJECT replace it; else create it.
set_([Name], Obj, Element, _Acc, _P, _IsMap) when ?IS_PL(Obj) andalso not ?IS_SELECTOR(Name)->
    lists:keystore(Name, 1, Obj, {Name, Element});

% Find OBJECT's Member with Path Name, that Member's Value will be the result 
% of the recursive call of set_ on its Value with the ballance of the Path.
set_([Name|Ks]=Path, [{N, V}|Ms], Element, Acc, P, IsMap) when not ?IS_SELECTOR(Name)->
    case Name of
        N ->
            NewV = set_(Ks, V, Element, [], P, IsMap),
            lists:append(lists:reverse(Acc),[{N, NewV}|Ms]);
        _Other ->
            set_(Path, Ms, Element, [{N,V}|Acc], P, IsMap)
    end;

% Intermediate Path element does not exist either create it or throw.
set_([Name|Ks], [], Element, Acc, P, IsMap) when not ?IS_SELECTOR(Name)->
    case P of
        true -> 
            lists:append(lists:reverse(Acc), [{Name, set_(Ks,[], Element, [], P, IsMap)}]);
        _ -> throw({no_path, Name})
    end;


% Last part of the Path found, if it exists in the OBJECT replace it; else 
% create it.
set_([Name], #{}=M, V, _Acc, _P, _IsMap) when not ?IS_SELECTOR(Name)->
    maps:put(Name, V, M);

% Find Object's Member with Path Name, that Members' Value will be thre result
% of the recursive call of set_ on the Value with the ballance of the Path. If 
% not there, either create it or throw.
set_([Name|Ks], #{}=M, Element, _Acc, P,IsMap) when not ?IS_SELECTOR(Name)->
    case maps:get(Name, M, not_found) of
        not_found -> 
            case P of
                true ->
                    maps:put(Name, set_(Ks, #{}, Element, [], P, IsMap), M);
                _ -> 
                    throw({no_path, Name})
            end;
        Value -> 
            maps:put(Name, set_(Ks, Value, Element, [], P, IsMap), M)
    end;

% ALL OBJECT CASES HANDLED ABOVE %

% The atom, new, applied to an ARRAY creates Element as the first element in Array.
set_([new], [_|_]=Array, Element, _Acc, _P, _IsMap) ->  
    [Element|Array];

% The final path element is a select_by_member applied to an ARRAY. Set
% the selected Objects with theTarget
set_([{select,{K,V}}=S], Array, Target, _Acc, _P, IsMap) when ?IS_OBJ(Target)->
    Found = selector_to_element(S, Array),
    Replace = case Found of
                  [] when IsMap ->
                       [maps:merge(#{K =>V}, Target)];
                  []  -> % build it
                      [[{K,V} | Target]];
                  Found ->
                      merge_members(Found, Target)
              end,

    case Found of 
        [] -> lists:reverse(Replace);
        _ ->
            NewArray = lists:append(remove(Array, Found), Replace),
            lists:reverse(NewArray)
    end;

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
    case Found of
        [] -> lists:reverse(Replaced);
        _ ->
             NewArray = lists:append(remove(Array, Found), Replaced),
            lists:reverse(NewArray)
    end;


% Path component is an index, either recurse on the selected Object, or replace
% it.
set_([S | R], Array, Element, _Acc, P, IsMap) when is_integer(S); S == last; S == first ->
    N = case S of 
            first -> 1;
            last -> length(Array);
            Int -> Int
        end,
    case R of
        [] ->
            lists:sublist(Array, 1, N-1) ++
                [Element] ++  
                lists:sublist(Array, N + 1, length(Array));
        _More when P ; N =<length(Array) ->
            lists:sublist(Array, 1, N-1) ++
                [set_(R, lists:nth(N, Array), Element, [], P, IsMap)] ++
                lists:sublist(Array, N + 1, length(Array));
        _More ->
            throw({no_path, S})
    end;

% Final Path component is a Name, target is an ARRAY, replace/add 
% Member to all selected Objects pulled form the Array.
set_([Name], [_|_]=Array, Element, _Acc, _P, IsMap) ->
    case elements_with(Name, Array) of
        [undefined] when IsMap -> % create and pretend it was always there
             merge_members(Array, #{Name => Element});
        [undefined] -> % create and pretend it was always there
            merge_members(Array, [{Name, Element}]);
        ObjectSet -> 
            case ?IS_OBJ(Element) of 
                true ->
                    merge_members(remove(Array, ObjectSet), Element);
                false ->
                    throw({replacing_object_with_value, Element})
            end
    end;

% Path component is a Name, target is an ARRAY,  Set will recursively 
% processes the selected objects containing a Member with name Name,
% with the ballance of the Path. If the Member isn't found, it will
% created.
set_([Name|Keys]=Ks, [_|_]=Array, Element, _Acc, P, IsMap) ->
    case elements_with(Name, Array) of
        [undefined] when P, IsMap -> 
            NewArray = merge_members(Array, #{Name => Element}),
            set_(Ks, [NewArray], Element, [], P, IsMap);
        [undefined] when P -> 
            [NewArray] = merge_members(Array, [{Name, Element}]),
            set_(Ks, [NewArray], Element, [], P, IsMap);
        [undefined] ->
            throw({no_path, Name});
        ObjectSet -> 
            NewObjSet = set_(Keys, ObjectSet, Element, [], P, IsMap),
            merge_members(remove(Array,ObjectSet), NewObjSet)

    end.

merge_members([#{}|_] = Maps, Target) ->
    [maps:merge(M, Target) || M <- Maps];

merge_members(Objects, M) ->
    [merge_pl(O, M) || O <- Objects].

merge_pl(P1, [{K,V}|Ts]) ->
    merge_pl(lists:keystore(K, 1, P1, {K,V}), Ts);
merge_pl(P1, []) ->
    P1.

remove(Objects, []) -> Objects;
remove(Objects, Remove) -> 
    lists:reverse(ordsets:to_list(
                    ordsets:subtract(ordsets:from_list(Objects),
                                     ordsets:from_list(Remove)))).


% Return all Values, from Object's Members (Name, Value) from an Array of 
% Objects.
elements_with(Key, Array) ->
    Elements = [walk([Key], Obj) || Obj <- Array, ?IS_OBJ(Obj)],
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


to_binary_list(Keys) ->
    L = tuple_to_list(Keys),
    lists:map(fun(K) -> make_binary(K) end, L).

make_binary(K) when is_binary(K); is_number(K) -> K;
make_binary(K) when is_list(K) -> list_to_binary(K);
make_binary(K) when is_atom(K) -> K;
make_binary({select, {K, V}}) -> 
    {select, {make_binary(K), make_binary(V)}}.


