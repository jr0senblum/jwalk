%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@prodigy.net>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%% The jwalk modue is intended to make it easier to work with Erlang encodings
%%% of JSON - either Maps or Proplists.
%%%
%%% This work is a derivitive of [https://github.com/seth/ej] but focuses on 
%%% map and property list representations of JSON.
%%%
%%% Functions always take at least two parameters: a first parameter which is a
%%% tuple of elements representing a Path into a JSON Object, and a second 
%%% parameter which is expected to be a proplist or map representation of a JSON
%%% structure.
%%%
%%% The Path components of the first parameter are a tuple representation of 
%%% a javascript-like path: i.e., 
%%%
%%% Obj.cars.make.model would be expressed as {"cars","make","model"}
%%%
%%% Path components may also contain:
%%%
%%% The atoms `` 'first' '' and `` 'last' '' or an integer index indicating an 
%%% element from a JSON Array; or,
%%%
%%% {select, {"name","value"}} which will return a subset of JSON objects in 
%%% an Array that have a {"name":"value"} Member. For example, given
%%%
%%% ```
%%% Cars = [{<<"cars">>, [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
%%%                        [{<<"color">>, <<"red">>},  {<<"age">>, <<"old">>}],
%%%                        [{<<"color">>, <<"blue">>}, {<<"age">>, <<"new">>}]
%%%                     ]}].
%%% '''
%%% Then 
%%%
%%% ```
%%% jwalk:get({"cars", {select, {"age", "old"}}}, Cars).
%%%
%%%  [ [{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}],
%%%    [{<<"color">>, <<"red">>},   {<<"age">>, <<"old">>}]
%%% ]
%%%
%%% jwalk:get({"cars", {select, {"age", "old"}}, 1}, Cars).
%%% [{<<"color">>,<<"white">>},{<<"age">>,<<"old">>}]
%%%
%%% jwalk:get({"cars", {select, {"age", "old"}},first,"color"}, Cars).
%%% <<"white">>
%%% '''
%%% @end
%%% Created : 20 Nov 2015 by Jim Rosenblum <jrosenblum@prodigy.net>
%%% ----------------------------------------------------------------------------
-module(jwalk).

-export([delete/2,
         get/2, get/3,
         set/3, 
         set_p/3]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-define(IS_OBJ(X), (is_map(X) orelse (is_list(X) andalso is_tuple(hd(X))))).
-define(IS_PL(X), (is_list(X) andalso is_tuple(hd(X)))).

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


-type name()   :: binary() | string().
-type value()  :: binary() | string() | number() | true | false | null| integer().
-type select() :: {select, {name(), value()}}.
-type p_elt()  :: name() | select() |'first' | 'last' | non_neg_integer() | new.
-type path()   :: {p_elt()}.
-type pl()     :: [{}] | [[{name(), value()|[value()]}],...].
-type obj()    :: map() | list(pl()).
-type jwalk_return() :: obj() | undefined | [obj() | value() | undefined,...].

-export_type ([jwalk_return/0]).



%% ----------------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% @doc Remove the value at the location specified by `Path' and return the
%% new representation. 
%%
%% Path elements that are strings can be binary or not - they will be converted 
%% to binary if not.
%%
-spec delete(path(), obj()) -> jwalk_return().

delete(Path, Obj) ->
    case rep_type(Obj) of
        map ->
            do_set(to_binary_list(Path), Obj, delete, [], false, true);
        proplist ->
            do_set(to_binary_list(Path), Obj, delete, [], false, false);
        _ ->
            error({illegal_object, Obj})
    end.

    
%% -----------------------------------------------------------------------------
%% @doc Return a value from 'Obj'. 
%%
%% Path elements that are strings can be binary or not - they will be converted 
%% to binary if not.
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
%% @doc Same as {@link get/2. get/2}, but returns `Default' if `Path' does not
%% exist in `Obj'. 
%%
-spec get(path(), obj(), Default::any()) -> jwalk_return().

get(Path, Obj, Default) ->
    case get(Path, Obj) of
        undefined ->
            Default;
        Found ->
            Found
    end.


%% -----------------------------------------------------------------------------
%% @doc Set a value in `Obj'.
%%
%% Replace the value at the specified `Path' with `Value' and return the new
%% structure. If the final element of the Path does not exist, it will be
%% created. 
%%
%% The atom, `new', applied to an ARRAY, will create the Element as the first
%% element in the Array.
%%
%% Path elements that are strings can be binary or not - they will be converted
%% to binary if not.
%%
-spec set(path(), obj(), value()) -> jwalk_return().

set(Path, Obj, Val) ->
    case rep_type(Obj) of
        map ->
            do_set(to_binary_list(Path), Obj, Val, [], false, true);
        proplist -> 
            do_set(to_binary_list(Path), Obj, Val, [], false, false);
        error ->
            error({illegal_object, Obj})
    end.


%% -----------------------------------------------------------------------------
%% @doc Sames as {@link set/3. set/3} but creates intermediary elements 
%% if necessary. Assumes `Obj' is (or will be) a Map.
%%
-spec set_p(path(), obj(), value()) -> jwalk_return().

set_p(Path, Obj, Val) ->
    case rep_type(Obj) of
        map->
            do_set(to_binary_list(Path), Obj, Val, [], true, true);
        proplist ->
            do_set(to_binary_list(Path), Obj, Val, [], true, false);
        error ->
            error({illegal_object, Obj})
    end.



do_set(Path, Obj, Val, Acc, P, IsMap) ->
    try 
        set_(Path, Obj, Val, Acc, P, IsMap)
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

% Applying a Path to an empty list means the Path doesn't exist.
walk(_, []) ->
    undefined;

% Any Path applied to null is undefined.
walk(_, null) -> undefined;

% OBJECT: if Member with Name exists in Obj, continue with that Member's Value.
walk([Name|Path], [{_, _}|_]=Obj) when not ?IS_SELECTOR(Name)->
    continue(lists:keyfind(Name, 1, Obj), Path);

% OBJECT: if Member with Name exists in Obj continue with that Member's Value.
walk([Name|Path], #{}=Obj) when not ?IS_SELECTOR(Name) ->
    continue(map_get(Name, Obj, undefined),Path);

% OBJECT: but we have a selector wanting an Array.
walk([S|_], Obj) when ?IS_SELECTOR(S), ?IS_OBJ(Obj)->
    throw({index_for_non_array, Obj});

% ARRAY: access it via selector and continue as appropriate.
walk([S|Path], [_|_]=Array) when ?IS_SELECTOR(S) ->
    continue(selector_to_element(S, Array), Path);

% ARRAY: Get all Values of Members with name = Name and continue as appropriate.
walk([Name|Path], [_|_]=Array) ->
    continue(values_at(Name, Array), Path);

% Element is something other than an ARRAY, but we have a selector.
walk([S|_], Element) when ?IS_SELECTOR(S) ->
    throw({index_for_non_array, Element}).


continue(false, _Path)                         -> undefined;
continue(undefined, _Path)                     -> undefined;
continue({_Name, Value}, Path) when Path == [] -> Value;
continue(Value, Path) when Path == []          ->  Value;
continue({_Name, Value}, Path)                 -> walk(Path, Value);
continue(Value, Path)                          -> walk(Path, Value).



% Final Path element: if it exists in the OBJECT delete it.
set_([Name], Obj, delete, _Acc, _IsP, _IsMap) when ?IS_PL(Obj) andalso
                                                   not ?IS_SELECTOR(Name) ->
    lists:keydelete(Name, 1, Obj);

set_([Name], Map, delete, _Acc, _IsP, _IsMap) when is_map(Map) andalso
                                                   not ?IS_SELECTOR(Name) ->
    maps:remove(Name, Map);

set_([{select,{_,_}}=S], Array, delete, _Acc, _IsP, _IsMap) ->
    Found = selector_to_element(S, Array),
    remove(Array, Found);

set_([Name], [_|_]=Array, delete, _Acc, _IsP, _IsMap) ->
    F = fun(#{}=Map)      -> maps:remove(Name, Map);
           ([{_,_}|_]=Pl) -> lists:keydelete(Name, 1, Pl)
        end,
    [F(O) || O <- Array];


% Final Path element: if it exist in the OBJECT replace or create it.
set_([Name], [{}], Val, _Acc, _IsP, _IsMap) when not ?IS_SELECTOR(Name) ->
    [{Name, Val}];

set_([Name], Obj, Val, _Acc, _IsP, false) when not ?IS_SELECTOR(Name) andalso 
                                               ?IS_PL(Obj) ->
    lists:keystore(Name, 1, Obj, {Name, Val});

set_([Name], Map, Val, _Acc, _P, true) when not ?IS_SELECTOR(Name) andalso 
                                            is_map(Map)->
    maps:put(Name, Val, Map);


% Iterate (proplist) Members for target. Replace value with recur call if found.
set_([Name|Ps]=Path, [{N, V}|Ms], Val, Acc, IsP, false) when not ?IS_SELECTOR(Name)->
    case Name of
        N ->
            NewVal = set_(Ps, V, Val, [], IsP, false),
            lists:append(lists:reverse(Acc),[{N, NewVal}|Ms]);
        _Other ->
            set_(Path, Ms, Val, [{N,V}|Acc], IsP, false)
    end;

% Intermediate Path element does not exist either create it or throw.
set_([Name|Ps], [{}], Val, _Acc, true, IsMap) when not ?IS_SELECTOR(Name) ->
    [{Name, set_(Ps, [{}], Val, [], true, IsMap)}];

set_([Name|Ps], [], Val, Acc, IsP, false) when not ?IS_SELECTOR(Name) ->
    case IsP of
        true -> 
            lists:append(lists:reverse(Acc), 
                         [{Name, set_(Ps,[{}], Val, [], IsP, false)}]);
        _ -> 
            throw({no_path, Name})
    end;

% map case
set_([Name|Ps], Map, Val, _Acc, P, true) when not ?IS_SELECTOR(Name) andalso is_map(Map)->
    case map_get(Name, Map, not_found) of
        not_found -> 
            case P of
                true ->
                    maps:put(Name, set_(Ps, #{}, Val, [], P, true), Map);
                _ -> 
                    throw({no_path, Name})
            end;
        Value -> 
            maps:put(Name, set_(Ps, Value, Val, [], P, true), Map)
    end;


% When final Path elemenet is NEW, just return the Value in an Array.
set_([new], #{}, Val, _Acc, _P, _IsMap) when ?IS_J_TERM(Val) ->
    [Val];

set_([new], [{}], Val, _Acc, _P, _IsMap)  when ?IS_J_TERM(Val) ->
    [Val];


set_([S|_], Obj, _V, _A, _P, _IsMap) when ?IS_SELECTOR(S) andalso ?IS_OBJ(Obj)->
    throw({selector_used_on_object, S, Obj});
    

% ALL OBJECT CASES HANDLED ABOVE %

% New applied to an ARRAY creates Value as the first element in  Array.
set_([new], Array, Val, _Acc, _P, _IsMap) when is_list(Array) ->
    [Val|Array];


% The final path element is a 'select by member' applied to an ARRAY. Set the 
% selected Objects with the Value
set_([{select,{K,V}}=S], Array, Val, _Acc, _P, IsMap) when ?IS_OBJ(Val), is_list(Array) ->
    Found = selector_to_element(S, Array),
    Replace = case Found of
                  [] when IsMap ->
                       [maps:merge(maps:put(K,V, #{}), Val)];
                  []  -> 
                      [[{K,V} | Val]];
                  Found ->
                      merge_members(Found, Val)
              end,
    lists:append(remove(Array, Found), Replace);
    
set_([{select,{_,_}}], _Array, Val, _Acc, _P, _IsMap) ->
    throw({replacing_object_with_value, Val});


% Select_by_member applied to an ARRAY. 
set_([{select,{K,V}}=S|Ks], Array, Val, _Acc, P, IsMap) ->
    Found = selector_to_element(S, Array),
    Objects = case Found of
                  [] when P andalso IsMap ->
                       [maps:put(K,V, #{})];
                  [] when P ->
                      [[{K,V}]];
                  [] -> 
                    throw({no_path, S});
                  _ ->
                      Found
              end,
    Replaced = set_(Ks, Objects, Val, [], P, IsMap),
    lists:append(remove(Array, Found), Replaced);


% Path component is index, either recurse on the selected Object, or replace it.
set_([S|_], Array, _Val, _Acc, _P, _IsMap) when not is_list(Array), ?IS_SELECTOR(S) ->
    throw({selector_used_on_non_array, S, Array});

set_([S|Path], Array, Val, _Acc, P, IsMap) when is_integer(S); S == last; S == first ->
    N = index_to_n(Array, S),
    case Path of
        [] ->
            lists:sublist(Array, 1, min(1, N-1)) ++
                [Val] ++  
                lists:sublist(Array, N + 1, length(Array));
        _More when P ; N =<length(Array) ->
            lists:sublist(Array, 1, min(1, N-1)) ++
                [set_(Path, lists:nth(N, Array), Val, [], P, IsMap)] ++
                lists:sublist(Array, N + 1, length(Array));
        _More ->
            throw({no_path, S})
    end;


% Final Path component is a Name, target is an ARRAY, replace/add Member to all
% selected Objects pulled from the Array.
set_([Name], [_|_]=Array, Val, _Acc, _P, IsMap) ->
    case found_elements(Name, Array) of
        undefined when IsMap -> 
             merge_members(Array, maps:put(Name, Val, #{}));
        undefined -> 
            merge_members(Array, [{Name, Val}]);
        ObjectSet -> 
            case ?IS_OBJ(Val) of 
                true ->
                    merge_members(remove(Array, ObjectSet), Val);
                false ->
                    throw({replacing_object_with_value, Val})
            end
    end;

% Path component is a Name, target is an ARRAY,  Set will recursively process
% the selected objects containing a Member with name Name, with the ballance
% ballance of the Path. 
set_([Name|Keys], Array, Val, _Acc, P, IsMap) ->
    case found_elements(Name, Array) of
        undefined ->
            throw({no_path, Name});
        ObjectSet -> 
            NewObjSet = set_(Keys, ObjectSet, Val, [], P, IsMap),
            merge_members(remove(Array,ObjectSet), NewObjSet)
    end.



%% ----------------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%% ----------------------------------------------------------------------------


found_elements(Name, Array) ->
    case values_at(Name, Array) of
        undefined -> undefined;
        EList -> 
            case lists:filter(fun(R) -> R /= undefined end, EList) of
                [] -> undefined;
                Elements ->
                    Elements
            end
    end.
             

% Return list of Values from Objects in Array who have Member with name = Name.
values_at(Name, Array) ->
    Elements = [walk([Name], Obj) || Obj <- Array, ?IS_OBJ(Obj)],
    case Elements of
        [] -> undefined;
        _ -> dont_nest(Elements)
    end.


% Make sure that we always return an Array - proplists can be over flattened.
dont_nest(H) -> 
    A = lists:flatten(H),
    case A of
        [{_,_}|_] = Obj ->
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
    case map_get(K, L, jwalk_false) of
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

selector_to_element(N, L)  when N =< length(L) ->
    lists:nth(N, L);

selector_to_element(N, L)  when N > length(L) ->
    throw({error, index_out_of_bounds, N, L}).


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


% looks for the first object and returns its representation type.
rep_type(#{}) -> map;
rep_type([{}]) -> proplist;
rep_type([{_,_}|_]) -> proplist;
rep_type([#{}|_]) -> map;
rep_type([[{_,_}|_]|_]) -> proplist;
rep_type([[{}]|_TL]) -> proplist;
rep_type([H|_TL]) when is_list(H) -> rep_type(H);
rep_type(_) -> error.


% to support erlang 17 need to roll my own get/3
map_get(Key, Map, Default) ->
     try  maps:get(Key, Map) of
          Value ->
             Value
     catch
         _:_ ->
             Default
     end.
        


-ifdef(TEST).

jwalk_alt_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("./test/widget.alt_terms"),
         {ok, [Glossary]} = file:consult("./test/glossary.alt_terms"),
         {ok, [Menu]} = file:consult("./test/menu.alt_terms"),
         ObjList = [{<<"objects">>,
                     [[{<<"id">>,1}],
                      [{<<"id">>,2}],
                      [{<<"id">>,3}],
                      [{<<"id">>,4}],
                      [{<<"id">>,5}]]}],
         {Widget, Glossary, Menu, ObjList}
 end,
 fun({Widget, Glossary, Menu, ObjList}) ->
         [{"jwalk:get",
           [
            ?_assertMatch([{_, _}|_], jwalk:get({"widget"}, Widget)),
            ?_assertEqual(<<"1">>, jwalk:get({"widget", "version"}, Widget)),
            ?_assertEqual(250, jwalk:get({"widget", "image", "hOffset"}, Widget)),
            ?_assertEqual([1,2,3,4,5], jwalk:get({"widget", "values"}, Widget)),
            ?_assertEqual(2, jwalk:get({"widget", "values", 2}, Widget)),
            ?_assertEqual(4, jwalk:get({"widget", "values", 4}, Widget)),
            ?_assertEqual(1, jwalk:get({"widget", "values", first}, Widget)),
            ?_assertEqual(5, jwalk:get({"widget", "values", last}, Widget)),
            ?_assertEqual(undefined, jwalk:get({"widget", "keys", first}, Widget)),
            ?_assertEqual(undefined, jwalk:get({"widget", "keys", last}, Widget)),
            ?_assertEqual(undefined, jwalk:get({"widget", "keys", 2}, Widget)),
            ?_assertEqual([{<<"id">>, 5}],
                          jwalk:get({<<"objects">>, last}, ObjList)),
            ?_assertEqual([{<<"id">>, 1}],
                          jwalk:get({<<"objects">>, first}, ObjList)),
            ?_assertEqual(undefined, jwalk:get({"fizzle"}, Widget)),
            ?_assertEqual(undefined, jwalk:get({"widget", "fizzle"}, Widget)),
            ?_assertEqual(undefined,
                          jwalk:get({"widget", "values", "fizzle"},Widget)),
            ?_assertEqual(<<"SGML">>,
                          jwalk:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "Acronym"}, Glossary)),
            ?_assertEqual(undefined,
                          jwalk:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "fizzle"}, Glossary)),
            ?_assertException(error, {index_for_non_array, _},
                              jwalk:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", 1}, Glossary)),
            ?_assertException(error, {index_for_non_array, _},
                              jwalk:get({"glossary", "title", 1}, Glossary))]},
          {"jwalk:get from array by matching key",
           fun() ->
              Path1 = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
              ?assertMatch([[{<<"value">>,<<"New">>}|_]], jwalk:get(Path1, Menu)),
              Path2 = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
              ?assertEqual([<<"CreateNewDoc()">>], jwalk:get(Path2, Menu)),
              PathNoneMatched = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}},
              ?assertEqual([], jwalk:get(PathNoneMatched, Menu)),
              PathDoesntExist = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}, "bar"},
              ?assertEqual(undefined, jwalk:get(PathDoesntExist, Menu)),
              Data =   [[{<<"match">>, <<"me">>}],
                        [{<<"match">>, <<"me">>}]
                       ],
              ComplexBeginning = {{select, {"match", "me"}}},
              ?assertMatch([[{_,_}], [{_,_}]], jwalk:get(ComplexBeginning, Data)),
              ComplexBeginningDeeper = {{select, {"match", "me"}}, "match"},
              ?assertMatch([<<"me">>, <<"me">>], jwalk:get(ComplexBeginningDeeper, Data)),
              PathAgainstEmptyList = {"menu", "popup", "titleitem", {select, {"value", "Title"}}},
              ?assertMatch([], jwalk:get(PathAgainstEmptyList, Menu))
            end},
          {"jwalk:get with multi-level array matching",
           fun() ->
                %% When doing multilevel deep array matching, we want the
                %% array returned to be a single top level list, and not
                %% a nested list of lists ...
                Data = [
                        {<<"users">>, [
                                       [{<<"id">>,<<"sebastian">>},
                                        {<<"books">>, [
                                                       [{<<"title">>, <<"faust">>},
                                                        {<<"rating">>, 5}]]}

                                       ]
                                      ]
                        }],

                Path = {"users", {select, {"id", "sebastian"}}, "books",
                        {select, {"title", "faust"}}, "rating"},
                Result = jwalk:get(Path, Data),
                ?assertEqual([5], Result)
            end},
          {"jwalk:set new value in an object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "alt"},
                   Val = <<"helptext">>,
                   Menu1 = jwalk:set(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path, Menu1))
           end},
          {"jwalk:set new value in a object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem",
                           {select, {"value", "New"}}},
                   Path2 = {"menu", "popup", "menuitem",
                            {select, {"value", "New"}}, "onclick"},
                   Val = [{<<"onclick">>, <<"CreateDifferentNewDoct()">>}],
                   Menu1 = jwalk:set(Path, Menu, Val),
                   ?assertEqual([<<"CreateDifferentNewDoct()">>], jwalk:get(Path2, Menu1)),
                   Path3 = {"menu", "popup", "menuitem",
                            {select, {"value", "New"}}, "speed"},
                   ValHigh = <<"high">>,
                   Menu2 = jwalk:set(Path3, Menu1, ValHigh),
                   ?assertEqual([ValHigh], jwalk:get(Path3, Menu2))
           end},
          {"jwalk:set replace multiple children of a complex path",
           fun() ->
                   %% We want the ability to affect multiple array elements
                   %% when a complex selector returns more than one match.
                   %% In this case all the selected array elements should be
                   %% replaced.
                   StartData =  [
                      [{<<"match">>, <<"me">>}, {<<"param">>, 1}],
                      [{<<"match">>, <<"me">>}, {<<"param">>, 2}]
                   ],
                   Path = {{select, {"match", "me"}}},
                   Path2 = {{select, {"match", "me"}}, "more"},
                   Val = [{<<"more">>, <<"content">>}],
                   Result = jwalk:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], jwalk:get(Path2, Result))
           end},
      {"jwalk:set replace multiple children deep in a complex path",
           fun() ->
                   %% We want the ability to affect multiple array elements
                   %% when a complex selector returns more than one match.
                   %% In this case we show that the array does not have to
                   %% be at the top level.
                   StartData =  [{<<"parent">>, [
                           [{<<"match">>, <<"me">>}, {<<"param">>, 1}],
                           [{<<"match">>, <<"me">>}, {<<"param">>, 2}]
                          ]}
                   ],
                   Path = {"parent", {select, {"match", "me"}}},
                   Path2 = {"parent", {select, {"match", "me"}}, "more"},
                   Val = [{<<"more">>, <<"content">>}],
                   EndData = jwalk:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], jwalk:get(Path2, EndData))
           end},
          {"jwalk:set should not allow replacing an array element at a complex path with a pure value",
           fun() ->
                   %% If the user has made a filtered selection on an array,
                   %% then all the elements in the array are objects.
                   %% Replacing the matched selection with a non-object value
                   %% will break this constraint.
                   Data = [ [{<<"match">>, <<"me">>}] ],
                   Path = {"match", "me"},
                   Val = <<"pure-value-and-not-a-struct">>,
                   ?_assertException(error, {replacing_object_with_value, _},
                                      jwalk:set(Path, Data, Val))
           end},
         {"jwalk:set, replacing existing value",
           fun() ->
                   Path = {"widget", "window", "name"},
                   CurrentValue = jwalk:get(Path, Widget),
                   NewValue = <<"bob">>,
                   ?assert(NewValue /= CurrentValue),
                   Widget1 = jwalk:set(Path, Widget, NewValue),
                   ?assertEqual(NewValue, jwalk:get(Path, Widget1)),
                   % make sure the structure hasn't been disturbed
                   Widget2 = jwalk:set(Path, Widget1, <<"main_window">>),
                   ?assertEqual(Widget, Widget2)
           end},
          {"jwalk:set, creating new value",
           fun() ->
                   Path = {"widget", "image", "newOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, jwalk:get(Path, Widget)),
                   Widget1 = jwalk:set(Path, Widget, Value),
                   ?assertEqual(Value, jwalk:get(Path, Widget1))
           end},
          {"jwalk:set, missing intermediate path",
           fun() ->
                   Path = {"widget", "middle", "nOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, jwalk:get(Path, Widget)),
                   ?assertException(error, {no_path, _},
                                    jwalk:set(Path, Widget, Value))
           end},
          {"jwalk:set top-level",
           fun() ->
                   OrigVal = jwalk:get({"widget", "version"}, Widget),
                   NewVal = <<"2">>,
                   NewWidget = jwalk:set({"widget", "version"}, Widget, NewVal),
                   ?assertEqual(NewVal,jwalk:get({"widget", "version"}, NewWidget)),
                   Reset = jwalk:set({"widget", "version"}, NewWidget, OrigVal),
                   ?assertEqual(Widget, Reset)
           end},
          {"jwalk:set nested",
           fun() ->
                   NewVal = <<"JSON">>,
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry",
                           "ID"},
                   Unchanged = jwalk:get({"glossary", "GlossDiv", "GlossList",
                                       "GlossEntry", "SortAs"}, Glossary),
                   Glossary1 = jwalk:set(Path, Glossary, NewVal),
                   ?assertEqual(NewVal, jwalk:get(Path, Glossary1)),
                   ?assertEqual(Unchanged, jwalk:get({"glossary", "GlossDiv",
                                                   "GlossList", "GlossEntry",
                                                   "SortAs"}, Glossary1)),
                   Reset = jwalk:set(Path, Glossary1, <<"SGML">>),
                   ?assertEqual(Glossary, Reset)
           end},
          {"jwalk:set list element",
           fun() ->
                   Orig = jwalk:get({"menu", "popup", "menuitem", 2}, Menu),
                   New = jwalk:set({"onclick"}, Orig, <<"OpenFile()">>),
                   Menu1 = jwalk:set({"menu", "popup", "menuitem", 2}, Menu, New),
                   ?assertEqual(New,
                                jwalk:get({"menu", "popup", "menuitem", 2}, Menu1)),
                   Reset = jwalk:set({"menu", "popup", "menuitem", 2}, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},
          {"jwalk:set list element path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", 2, "onclick"},
                   Orig = jwalk:get(Path, Menu),
                   New = <<"OpenFile()">>,
                   Menu1 = jwalk:set(Path, Menu, New),
                   ?assertEqual(New, jwalk:get(Path, Menu1)),
                   Reset = jwalk:set(Path, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},
          {"jwalk:set list element path first, last",
           fun() ->
                   FPath = {"menu", "popup", "menuitem", first, "value"},
                   LPath = {"menu", "popup", "menuitem", last, "value"},
                   FMenu = jwalk:set(FPath, Menu, <<"create">>),
                   LMenu = jwalk:set(LPath, FMenu, <<"kill">>),
                   ?assertEqual(<<"create">>, jwalk:get(FPath, FMenu)),
                   ?assertEqual(<<"create">>, jwalk:get(FPath, LMenu)),
                   ?assertEqual(<<"kill">>, jwalk:get(LPath, LMenu))
           end},
          {"jwalk:set new list element",
           fun() ->
                   Path = {"menu", "popup", "menuitem", new},
                   Path1 = {"menu", "popup", "menuitem", first},
                   Menu1 = jwalk:set(Path, Menu, <<"first-item">>),
                   ?assertEqual(<<"first-item">>, jwalk:get(Path1, Menu1)),
                   List = jwalk:get({"menu", "popup", "menuitem"}, Menu1),
                   ?assertEqual(4, length(List))
           end},
          {"jwalk:set_p creates intermediate missing nodes",
           fun() ->
                   StartData = [{}],
                   EndData = [{<<"a">>,
                      [{<<"b">>,
                           [{<<"c">>, <<"value">>}]
                      }]
                   }],
                   Path = {"a", "b", "c"},
                   Result = jwalk:set_p(Path, StartData, <<"value">>),
                   ?assertEqual(EndData, Result),
                   ?assertEqual(<<"value">>, jwalk:get(Path, Result)),
                   Path2 = {"1", "2"},
                   Result2 = jwalk:set_p(Path2, Result, <<"other-value">>),
                   ?assertEqual(<<"other-value">>, jwalk:get(Path2, Result2)),
                   %% Does not affect existing values
                   ?assertEqual(<<"value">>, jwalk:get(Path, Result2))
           end},
          {"jwalk:set_p value in a non-existent object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem",
                           {select, {"value", "Edit"}}},
                   Path2 = {"menu", "popup", "menuitem",
                            {select, {"value", "Edit"}}, "text"},
                   Path3 = {"menu", "popup", "menuitem",
                            {select, {"value", "Edit"}}, "value"},
                   Val =  [{<<"text">>, <<"helptext">>}],
                   Menu1 = jwalk:set_p(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path2, Menu1)),
                   ?assertEqual([<<"Edit">>], jwalk:get(Path3, Menu1))
           end},
          {"jwalk:set_p starting with an empty structure",
           fun() ->
                   Path = {"menu", "popup","menuitem",new},
                   Path2 = {"menu", "popup","menuitem",first},
                   Val =  [{<<"onclick">>, <<"CreateNewDoc()">>},{<<"value">>, <<"New">>}],
                   Menu1 = jwalk:set_p(Path, [{}], Val),
                   ?assertMatch(Val, jwalk:get(Path2, Menu1))
           end},
          {"jwalk:set using selector on non-array",
           fun() ->
                   ?assertException(error, {selector_used_on_non_array,first,_},
                                    jwalk:set({<<"menu">>,<<"id">>,first},Menu,true)),
                   ?assertException(error, {selector_used_on_object,first,__},
                                    jwalk:set({"menu","popup","menuitem",first,first},Menu, true))
           end},
          {"jwalk:set_p using selector on object",
           fun() ->
                   ?assertException(error, {selector_used_on_non_array,first,_},
                                    jwalk:set_p({<<"menu">>,<<"id">>,first},Menu,true)),
                   ?assertException(error, {selector_used_on_object,first,__},
                                    jwalk:set_p({"menu","popup","menuitem",first,first},Menu, true))
           end},
          {"jwalk:remove",
           fun() ->
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry", "Abbrev"},
                   Orig = jwalk:get(Path, Glossary),
                   ?assert(undefined /= Orig),
                   Glossary1 = jwalk:delete(Path, Glossary),
                   ?assertEqual(undefined, jwalk:get(Path, Glossary1)),
                   % verify some structure
                   ?assertEqual(<<"SGML">>, jwalk:get({"glossary", "GlossDiv",
                                                    "GlossList", "GlossEntry",
                                                    "Acronym"}, Glossary1)),
                   ?assertEqual(<<"S">>, jwalk:get({"glossary", "GlossDiv",
                                                    "title"}, Glossary1))
           end},
          {"jwalk:remove parameter at complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
                   Orig = jwalk:get(Path, Menu),
                   ?assert(undefined /= Orig),
                   Menu1 = jwalk:delete(Path, Menu),
                   ?assertEqual([undefined], jwalk:get(Path, Menu1)),
                   % verify some structure
                   VerifyPath = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "value"},
                   ?assertEqual([<<"New">>], jwalk:get(VerifyPath, Menu1)),
                   % verify that we didn't delete siblings
                   VerifyOpen = {"menu", "popup", "menuitem", {select, {"value", "Open"}}, "onclick"},
                   ?assertEqual([<<"OpenDoc()">>], jwalk:get(VerifyOpen, Menu1)),
                   VerifyClose = {"menu", "popup", "menuitem", {select, {"value", "Close"}}, "onclick"},
                   ?assertEqual([<<"CloseDoc()">>], jwalk:get(VerifyClose, Menu1))
           end},
          {"jwalk:remove object at complex path, keys is tuple",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
                   Orig = jwalk:get(Path, Menu),
                   ?assert([] /= Orig),
                   Menu1 = jwalk:delete(Path, Menu),
                   ?assertEqual([], jwalk:get(Path, Menu1)),
                   % verify some structure
                   VerifyPath = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "value"},
                   ?assertEqual(undefined, jwalk:get(VerifyPath, Menu1)),
                   % % verify that we didn't delete siblings
                   VerifyOpen = {"menu", "popup", "menuitem", {select, {"value", "Open"}}, "onclick"},
                   ?assertEqual([<<"OpenDoc()">>], jwalk:get(VerifyOpen, Menu1)),
                   VerifyClose = {"menu", "popup", "menuitem", {select, {"value", "Close"}}, "onclick"},
                   ?assertEqual([<<"CloseDoc()">>], jwalk:get(VerifyClose, Menu1))
           end},
           {"jwalk:remove object at complex path, keys is list",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
                   Orig = jwalk:get(Path, Menu),
                   ?assert([] /= Orig),
                   Menu1 = jwalk:delete(Path, Menu),
                   ?assertEqual([], jwalk:get(Path, Menu1)),
                   % verify some structure
                   VerifyPath = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "value"},
                   ?assertEqual(undefined, jwalk:get(VerifyPath, Menu1)),
                   % % verify that we didn't delete siblings
                   VerifyOpen = {"menu", "popup", "menuitem", {select, {"value", "Open"}}, "onclick"},
                   ?assertEqual([<<"OpenDoc()">>], jwalk:get(VerifyOpen, Menu1)),
                   VerifyClose = {"menu", "popup", "menuitem", {select, {"value", "Close"}}, "onclick"},
                   ?assertEqual([<<"CloseDoc()">>], jwalk:get(VerifyClose, Menu1))
           end}

         ]
 end
}.

jwalk_map_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("./test/widget.map_terms"),
         {ok, [Glossary]} = file:consult("./test/glossary.map_terms"),
         {ok, [Menu]} = file:consult("./test/menu.map_terms"),
         ObjList = #{<<"objects">> =>
                         [#{<<"id">> => 1},
                          #{<<"id">> => 2},
                          #{<<"id">> => 3},
                          #{<<"id">> => 4},
                          #{<<"id">> => 5}]},
         {Widget, Glossary, Menu, ObjList}
 end,
 fun({Widget, Glossary, Menu, ObjList}) ->
         [{"jwalk:get",
           [
            ?_assertMatch(#{<<"debug">> := <<"on">>}, jwalk:get({"widget"}, Widget)),
            ?_assertEqual(<<"1">>, jwalk:get({"widget", "version"}, Widget)),
            ?_assertEqual(250, jwalk:get({"widget", "image", "hOffset"}, Widget)),
            ?_assertEqual([1,2,3,4,5], jwalk:get({"widget", "values"}, Widget)),
            ?_assertEqual(2, jwalk:get({"widget", "values", 2}, Widget)),
            ?_assertEqual(4, jwalk:get({"widget", "values", 4}, Widget)),
            ?_assertEqual(1, jwalk:get({"widget", "values", first}, Widget)),
            ?_assertEqual(5, jwalk:get({"widget", "values", last}, Widget)),
            ?_assertEqual(undefined, jwalk:get({"widget", "keys", first}, Widget)),
            ?_assertEqual(undefined, jwalk:get({"widget", "keys", last}, Widget)),
            ?_assertEqual(undefined, jwalk:get({"widget", "keys", 2}, Widget)),
            ?_assertEqual(#{<<"id">> => 5},
                          jwalk:get({<<"objects">>, last}, ObjList)),
            ?_assertEqual(#{<<"id">> => 1},
                          jwalk:get({<<"objects">>, first}, ObjList)),
            ?_assertEqual(undefined, jwalk:get({"fizzle"}, Widget)),
            ?_assertEqual(undefined, jwalk:get({"widget", "fizzle"}, Widget)),
            ?_assertEqual(undefined,
                          jwalk:get({"widget", "values", "fizzle"},Widget)),
            ?_assertEqual(<<"SGML">>,
                          jwalk:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "Acronym"}, Glossary)),
            ?_assertEqual(undefined,
                          jwalk:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "fizzle"}, Glossary)),
            ?_assertException(error, {index_for_non_array, _},
                              jwalk:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", 1}, Glossary)),
            ?_assertException(error, {index_for_non_array, _},
                              jwalk:get({"glossary", "title", 1}, Glossary))]},
          {"jwalk:get from array by matching key",
           fun() ->
              Path1 = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
              ?assertMatch([#{<<"value">> := <<"New">>}], jwalk:get(Path1, Menu)),
              Path2 = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
              ?assertEqual([<<"CreateNewDoc()">>], jwalk:get(Path2, Menu)),
              PathNoneMatched = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}},
              ?assertEqual([], jwalk:get(PathNoneMatched, Menu)),
              PathDoesntExist = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}, "bar"},
              ?assertEqual(undefined, jwalk:get(PathDoesntExist, Menu)),
              Data =   [#{<<"match">> => <<"me">>},#{<<"match">> => <<"me">>}],

              ComplexBeginning = {{select, {"match", "me"}}},
              ?assertMatch([#{<<"match">> := <<"me">>},#{<<"match">> := <<"me">>}], jwalk:get(ComplexBeginning, Data)),
              ComplexBeginningDeeper = {{select, {"match", "me"}}, "match"},
              ?assertMatch([<<"me">>, <<"me">>], jwalk:get(ComplexBeginningDeeper, Data)),
              PathAgainstEmptyList = {"menu", "popup", "titleitem", {select, {"value", "Title"}}},
              ?assertMatch([], jwalk:get(PathAgainstEmptyList, Menu))
            end},
          {"jwalk:get with multi-level array matching",
           fun() ->
                %% When doing multilevel deep array matching, we want the
                %% array returned to be a single top level list, and not
                %% a nested list of lists ...
                Data = #{<<"users">> => 
                             [#{<<"books">> => 
                                    [#{<<"rating">> => 5,<<"title">> => <<"faust">>}],
                                <<"id">> => <<"sebastian">>}]},


                Path = {"users", {select, {"id", "sebastian"}}, "books",
                        {select, {"title", "faust"}}, "rating"},
                Result = jwalk:get(Path, Data),
                ?assertEqual([5], Result)
            end},
          {"jwalk:set new value in an object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "alt"},
                   Val = <<"helptext">>,
                   Menu1 = jwalk:set(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path, Menu1))
           end},
          {"jwalk:set new value in a object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem",
                           {select, {"value", "New"}}},
                   Path2 = {"menu", "popup", "menuitem",
                            {select, {"value", "New"}}, "onclick"},
                   Val = #{<<"onclick">> => <<"CreateDifferentNewDoct()">>},
                   Menu1 = jwalk:set(Path, Menu, Val),
                   ?assertEqual([<<"CreateDifferentNewDoct()">>], jwalk:get(Path2, Menu1)),
                   Path3 = {"menu", "popup", "menuitem",
                            {select, {"value", "New"}}, "speed"},
                   ValHigh = <<"high">>,
                   Menu2 = jwalk:set(Path3, Menu1, ValHigh),
                   ?assertEqual([ValHigh], jwalk:get(Path3, Menu2))
           end},
          {"jwalk:set replace multiple children of a complex path",
           fun() ->
                   %% We want the ability to affect multiple array elements
                   %% when a complex selector returns more than one match.
                   %% In this case all the selected array elements should be
                   %% replaced.
                   StartData =  [
                                 #{<<"match">> => <<"me">>, <<"param">> => 1},
                                 #{<<"match">> => <<"me">>, <<"param">> => 2}
                                ],
                   Path = {{select, {"match", "me"}}},
                   Path2 = {{select, {"match", "me"}}, "more"},
                   Val = #{<<"more">> => <<"content">>},
                   Result = jwalk:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], jwalk:get(Path2, Result))
           end},
      {"jwalk:set replace multiple children deep in a complex path",
           fun() ->
                   %% We want the ability to affect multiple array elements
                   %% when a complex selector returns more than one match.
                   %% In this case we show that the array does not have to
                   %% be at the top level.
                   StartData =  #{<<"parent">> => 
                                      [#{<<"match">> => <<"me">>, <<"param">> => 1},
                                       #{<<"match">> => <<"me">>, <<"param">> => 2}]
                                 },
                   Path = {"parent", {select, {"match", "me"}}},
                   Path2 = {"parent", {select, {"match", "me"}}, "more"},
                   Val = #{<<"more">> => <<"content">>},
                   EndData = jwalk:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], jwalk:get(Path2, EndData))
           end},
          {"jwalk:set should not allow replacing an array element at a complex path with a pure value",
           fun() ->
                   %% If the user has made a filtered selection on an array,
                   %% then all the elements in the array are objects.
                   %% Replacing the matched selection with a non-object value
                   %% will break this constraint.
                   Data = [ [{<<"match">>, <<"me">>}] ],
                   Path = {"match", "me"},
                   Val = <<"pure-value-and-not-a-struct">>,
                   ?_assertException(error, {replacing_object_with_value, _},
                                      jwalk:set(Path, Data, Val))
           end},
         {"jwalk:set, replacing existing value",
           fun() ->
                   Path = {"widget", "window", "name"},
                   CurrentValue = jwalk:get(Path, Widget),
                   NewValue = <<"bob">>,
                   ?assert(NewValue /= CurrentValue),
                   Widget1 = jwalk:set(Path, Widget, NewValue),
                   ?assertEqual(NewValue, jwalk:get(Path, Widget1)),
                   % make sure the structure hasn't been disturbed
                   Widget2 = jwalk:set(Path, Widget1, <<"main_window">>),
                   ?assertEqual(Widget, Widget2)
           end},
          {"jwalk:set, creating new value",
           fun() ->
                   Path = {"widget", "image", "newOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, jwalk:get(Path, Widget)),
                   Widget1 = jwalk:set(Path, Widget, Value),
                   ?assertEqual(Value, jwalk:get(Path, Widget1))
           end},
         {"jwalk:set, missing intermediate path",
           fun() ->
                   Path = {"widget", "middle", "nOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, jwalk:get(Path, Widget)),
                   ?assertException(error, {no_path, _},
                                    jwalk:set(Path, Widget, Value))
           end},
         {"jwalk:set top-level",
           fun() ->
                   OrigVal = jwalk:get({"widget", "version"}, Widget),
                   NewVal = <<"2">>,
                   NewWidget = jwalk:set({"widget", "version"}, Widget, NewVal),
                   ?assertEqual(NewVal,jwalk:get({"widget", "version"}, NewWidget)),
                   Reset = jwalk:set({"widget", "version"}, NewWidget, OrigVal),
                   ?assertEqual(Widget, Reset)
           end},
          {"jwalk:set nested",
           fun() ->
                   NewVal = <<"JSON">>,
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry",
                           "ID"},
                   Unchanged = jwalk:get({"glossary", "GlossDiv", "GlossList",
                                       "GlossEntry", "SortAs"}, Glossary),
                   Glossary1 = jwalk:set(Path, Glossary, NewVal),
                   ?assertEqual(NewVal, jwalk:get(Path, Glossary1)),
                   ?assertEqual(Unchanged, jwalk:get({"glossary", "GlossDiv",
                                                   "GlossList", "GlossEntry",
                                                   "SortAs"}, Glossary1)),
                   Reset = jwalk:set(Path, Glossary1, <<"SGML">>),
                   ?assertEqual(Glossary, Reset)
           end},
          {"jwalk:set list element",
           fun() ->
                   Orig = jwalk:get({"menu", "popup", "menuitem", 2}, Menu),
                   New = jwalk:set({"onclick"}, Orig, <<"OpenFile()">>),
                   Menu1 = jwalk:set({"menu", "popup", "menuitem", 2}, Menu, New),
                   ?assertEqual(New,
                                jwalk:get({"menu", "popup", "menuitem", 2}, Menu1)),
                   Reset = jwalk:set({"menu", "popup", "menuitem", 2}, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},
          {"jwalk:set list element path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", 2, "onclick"},
                   Orig = jwalk:get(Path, Menu),
                   New = <<"OpenFile()">>,
                   Menu1 = jwalk:set(Path, Menu, New),
                   ?assertEqual(New, jwalk:get(Path, Menu1)),
                   Reset = jwalk:set(Path, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},
          {"jwalk:set list element path first, last",
           fun() ->
                   FPath = {"menu", "popup", "menuitem", first, "value"},
                   LPath = {"menu", "popup", "menuitem", last, "value"},
                   FMenu = jwalk:set(FPath, Menu, <<"create">>),
                   LMenu = jwalk:set(LPath, FMenu, <<"kill">>),
                   ?assertEqual(<<"create">>, jwalk:get(FPath, FMenu)),
                   ?assertEqual(<<"create">>, jwalk:get(FPath, LMenu)),
                   ?assertEqual(<<"kill">>, jwalk:get(LPath, LMenu))
           end},
          {"jwalk:set new list element",
           fun() ->
                   Path = {"menu", "popup", "menuitem", new},
                   Path1 = {"menu", "popup", "menuitem", first},
                   Menu1 = jwalk:set(Path, Menu, <<"first-item">>),
                   ?assertEqual(<<"first-item">>, jwalk:get(Path1, Menu1)),
                   List = jwalk:get({"menu", "popup", "menuitem"}, Menu1),
                   ?assertEqual(4, length(List))
           end},
          {"jwalk:set_p creates intermediate missing nodes",
           fun() ->
                   StartData = #{},
                   EndData = #{<<"a">> =>
                      #{<<"b">> =>
                           #{<<"c">> => <<"value">>}
                      }
                   },
                   Path = {"a", "b", "c"},
                   Result = jwalk:set_p(Path, StartData, <<"value">>),
                   ?assertEqual(EndData, Result),
                   ?assertEqual(<<"value">>, jwalk:get(Path, Result)),
                   Path2 = {"1", "2"},
                   Result2 = jwalk:set_p(Path2, Result, <<"other-value">>),
                   ?assertEqual(<<"other-value">>, jwalk:get(Path2, Result2)),
                   %% Does not affect existing values
                   ?assertEqual(<<"value">>, jwalk:get(Path, Result2))
           end},
          {"jwalk:set_p value in a non-existent object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem",
                           {select, {"value", "Edit"}}},
                   Path2 = {"menu", "popup", "menuitem",
                            {select, {"value", "Edit"}}, "text"},
                   Path3 = {"menu", "popup", "menuitem",
                            {select, {"value", "Edit"}}, "value"},
                   Val =  #{<<"text">> => <<"helptext">>},
                   Menu1 = jwalk:set_p(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path2, Menu1)),
                   ?assertEqual([<<"Edit">>], jwalk:get(Path3, Menu1))
           end},
          {"jwalk:set_p starting with an empty structure",
           fun() ->
                   Path = {"menu", "popup","menuitem",new},
                   Path2 = {"menu", "popup","menuitem",first},
                   Val =  #{<<"onclick">> => <<"CreateNewDoc()">>,<<"value">> => <<"New">>},
                   Menu1 = jwalk:set_p(Path, #{}, Val),
                   ?assertMatch(Val, jwalk:get(Path2, Menu1))
           end},
          {"jwalk:set using selector on non-array",
           fun() ->
                   ?assertException(error, {selector_used_on_non_array,first,_},
                                    jwalk:set({<<"menu">>,<<"id">>,first},Menu,true)),
                   ?assertException(error, {selector_used_on_object,first,__},
                                    jwalk:set({"menu","popup","menuitem",first,first},Menu, true))
           end},
          {"jwalk:set_p using selector on object",
           fun() ->
                   ?assertException(error, {selector_used_on_object,first,__},
                                    jwalk:set_p({"menu","popup","menuitem",first,first},Menu, true)),
                   ?assertException(error, {selector_used_on_non_array,first,_},
                                    jwalk:set_p({<<"menu">>,<<"id">>,first},Menu,true))
           end},
          {"jwalk:remove",
           fun() ->
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry", "Abbrev"},
                   Orig = jwalk:get(Path, Glossary),
                   ?assert(undefined /= Orig),
                   Glossary1 = jwalk:delete(Path, Glossary),
                   ?assertEqual(undefined, jwalk:get(Path, Glossary1)),
                   % verify some structure
                   ?assertEqual(<<"SGML">>, jwalk:get({"glossary", "GlossDiv",
                                                    "GlossList", "GlossEntry",
                                                    "Acronym"}, Glossary1)),
                   ?assertEqual(<<"S">>, jwalk:get({"glossary", "GlossDiv",
                                                    "title"}, Glossary1))
           end},
          {"jwalk:remove parameter at complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
                   Orig = jwalk:get(Path, Menu),
                   ?assert(undefined /= Orig),
                   Menu1 = jwalk:delete(Path, Menu),
                   ?assertEqual([undefined], jwalk:get(Path, Menu1)),
                   % verify some structure
                   VerifyPath = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "value"},
                   ?assertEqual([<<"New">>], jwalk:get(VerifyPath, Menu1)),
                   % verify that we didn't delete siblings
                   VerifyOpen = {"menu", "popup", "menuitem", {select, {"value", "Open"}}, "onclick"},
                   ?assertEqual([<<"OpenDoc()">>], jwalk:get(VerifyOpen, Menu1)),
                   VerifyClose = {"menu", "popup", "menuitem", {select, {"value", "Close"}}, "onclick"},
                   ?assertEqual([<<"CloseDoc()">>], jwalk:get(VerifyClose, Menu1))
           end},
          {"jwalk:remove object at complex path, keys is tuple",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
                   Orig = jwalk:get(Path, Menu),
                   ?assert([] /= Orig),
                   Menu1 = jwalk:delete(Path, Menu),
                   ?assertEqual([], jwalk:get(Path, Menu1)),
                   % verify some structure
                   VerifyPath = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "value"},
                   ?assertEqual(undefined, jwalk:get(VerifyPath, Menu1)),
                   % % verify that we didn't delete siblings
                   VerifyOpen = {"menu", "popup", "menuitem", {select, {"value", "Open"}}, "onclick"},
                   ?assertEqual([<<"OpenDoc()">>], jwalk:get(VerifyOpen, Menu1)),
                   VerifyClose = {"menu", "popup", "menuitem", {select, {"value", "Close"}}, "onclick"},
                   ?assertEqual([<<"CloseDoc()">>], jwalk:get(VerifyClose, Menu1))
           end},
           {"jwalk:remove object at complex path, keys is list",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
                   Orig = jwalk:get(Path, Menu),
                   ?assert([] /= Orig),
                   Menu1 = jwalk:delete(Path, Menu),
                   ?assertEqual([], jwalk:get(Path, Menu1)),
                   % verify some structure
                   VerifyPath = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "value"},
                   ?assertEqual(undefined, jwalk:get(VerifyPath, Menu1)),
                   % % verify that we didn't delete siblings
                   VerifyOpen = {"menu", "popup", "menuitem", {select, {"value", "Open"}}, "onclick"},
                   ?assertEqual([<<"OpenDoc()">>], jwalk:get(VerifyOpen, Menu1)),
                   VerifyClose = {"menu", "popup", "menuitem", {select, {"value", "Close"}}, "onclick"},
                   ?assertEqual([<<"CloseDoc()">>], jwalk:get(VerifyClose, Menu1))
           end}
 
         ]
 end
}.
-endif.
