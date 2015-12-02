%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@prodigy.net>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%% The jwalk modue is intended to make it easier to work with Erlang encodings
%%% of JSON - either MAPS or PROPERTY LISTS. 
%%%
%%% This work is a derivitive  of https://github.com/seth/ejm but focuses on 
%%% map and property list representation of JSON.
%%%
%%% Functions take a tuple, where the elements of the tuple represent a Path 
%%% into a JSON Object, and an encoded JSON structure (Proplist or Map) and 
%%% return or delete the values indicated by the Path.
%%%
%%% The Path components are a tuple representation of a javascript-like, path: 
%%% i.e., 
%%%
%%% Obj.cars.make.model would be expressed as {"cars","make","model"}
%%%
%%% Path components may also contain:
%%%
%%% The atoms `` 'first' '' and `` 'last' '' or an integer index indicating an 
%%% element from a JSON array; or,
%%%
%%% {select, {"name","value"}} which will return a subset of JSON objects in 
%%% an Array. For example, given
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


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


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
%% new Map representation. Path elements that are strings can be binary or not
%% - they will be converted to binary if not.
%%
-spec delete(path(), obj()) -> jwalk_return().

delete(Path, Obj) ->
    do_set(to_binary_list(Path), Obj, delete, [], false, true).



%% -----------------------------------------------------------------------------
%% @doc Same as {@link delete/2. delete/2} except it expects and returns a 
%% Property list representation of `obj'.
%%
-spec delete(path(), obj(), proplist) -> jwalk_return().

delete(Path, Obj, proplist) ->
    do_set(to_binary_list(Path), Obj, delete, [], false, false).



%% -----------------------------------------------------------------------------
%% @doc Return a value from 'Obj'.
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
%% @doc Set a value in `Obj', assumes Obj is (or will be) a Map.
%%
%% Replace the value at the specified `Path' with `Value' and return the new
%% structure. If the final element of the Path, does not exist, it will be
%% created. 
%%
%% The atom, `new', applied to an ARRAY, will create the Element as the first
%% element in the Array.
%%
%% Path elements that are strings can be binary or not - they will be converted
%% to binary if not.
%%
-spec set(path(), obj(), value()) -> jwalk_return().

set(Path, Obj, Element) ->
    do_set(to_binary_list(Path), Obj, Element, [], false, true).



%% -----------------------------------------------------------------------------
%% @doc Same as {@link set/3. set/3} but `Obj' is, or will be, a Proplist 
%% representation.
%%
-spec set(path(), obj(), value(), proplist) -> jwalk_return().

set(Path, Obj, Element, proplist) ->
    do_set(to_binary_list(Path), Obj, Element, [], false, false).



%% -----------------------------------------------------------------------------
%% @doc Sames ase {@link set/3. set/3} but creates intermediary elements 
%% if necessary. Assumes `Obj' is (or will be) a Map.
%%
-spec set_p(path(), obj(), value()) -> jwalk_return().

set_p(Path, Obj, Element) ->
    do_set(to_binary_list(Path), Obj, Element, [], true, true).


%% -----------------------------------------------------------------------------
%% @doc Sames ase {@link set/3. set/3} but creates intermediary elements 
%% if necessary. Assumes `Obj' is (or will be) a Proplist.
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
                       [maps:merge(maps:put(K,V, #{}), Target)];
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
                       [maps:put(K,V, #{})];
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
             merge_members(Array, maps:put(Name, Element, #{}));
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

selector_to_element(N, L)  when N =< length(L) ->
    lists:nth(N, L);
selector_to_element(N, L)  when N > length(L) ->
    throw({error, index_out_of_bounds, N, L}).




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
            ?_assertException(error, {index_for_non_list, _},
                              jwalk:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", 1}, Glossary)),
            ?_assertException(error, {index_for_non_list, _},
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
                   Menu1 = jwalk:set(Path, Menu, Val, proplist),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path, Menu1))
           end},
          {"jwalk:set new value in a object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem",
                           {select, {"value", "New"}}},
                   Path2 = {"menu", "popup", "menuitem",
                            {select, {"value", "New"}}, "onclick"},
                   Val = [{<<"onclick">>, <<"CreateDifferentNewDoct()">>}],
                   Menu1 = jwalk:set(Path, Menu, Val, proplist),
                   ?assertEqual([<<"CreateDifferentNewDoct()">>], jwalk:get(Path2, Menu1)),
                   Path3 = {"menu", "popup", "menuitem",
                            {select, {"value", "New"}}, "speed"},
                   ValHigh = <<"high">>,
                   Menu2 = jwalk:set(Path3, Menu1, ValHigh,proplist),
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
                   Result = jwalk:set(Path, StartData, Val,proplist),
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
                   EndData = jwalk:set(Path, StartData, Val,proplist),
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
                                      jwalk:set(Path, Data, Val,proplist))
           end},
         {"jwalk:set, replacing existing value",
           fun() ->
                   Path = {"widget", "window", "name"},
                   CurrentValue = jwalk:get(Path, Widget),
                   NewValue = <<"bob">>,
                   ?assert(NewValue /= CurrentValue),
                   Widget1 = jwalk:set(Path, Widget, NewValue,proplist),
                   ?assertEqual(NewValue, jwalk:get(Path, Widget1)),
                   % make sure the structure hasn't been disturbed
                   Widget2 = jwalk:set(Path, Widget1, <<"main_window">>,proplist),
                   ?assertEqual(Widget, Widget2)
           end},
          {"jwalk:set, creating new value",
           fun() ->
                   Path = {"widget", "image", "newOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, jwalk:get(Path, Widget)),
                   Widget1 = jwalk:set(Path, Widget, Value,proplist),
                   ?assertEqual(Value, jwalk:get(Path, Widget1))
           end},
          {"jwalk:set, missing intermediate path",
           fun() ->
                   Path = {"widget", "middle", "nOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, jwalk:get(Path, Widget)),
                   ?assertException(error, {no_path, _},
                                    jwalk:set(Path, Widget, Value,proplist))
           end},
          {"jwalk:set top-level",
           fun() ->
                   OrigVal = jwalk:get({"widget", "version"}, Widget),
                   NewVal = <<"2">>,
                   NewWidget = jwalk:set({"widget", "version"}, Widget, NewVal,proplist),
                   ?assertEqual(NewVal,jwalk:get({"widget", "version"}, NewWidget)),
                   Reset = jwalk:set({"widget", "version"}, NewWidget, OrigVal,proplist),
                   ?assertEqual(Widget, Reset)
           end},
          {"jwalk:set nested",
           fun() ->
                   NewVal = <<"JSON">>,
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry",
                           "ID"},
                   Unchanged = jwalk:get({"glossary", "GlossDiv", "GlossList",
                                       "GlossEntry", "SortAs"}, Glossary),
                   Glossary1 = jwalk:set(Path, Glossary, NewVal,proplist),
                   ?assertEqual(NewVal, jwalk:get(Path, Glossary1)),
                   ?assertEqual(Unchanged, jwalk:get({"glossary", "GlossDiv",
                                                   "GlossList", "GlossEntry",
                                                   "SortAs"}, Glossary1)),
                   Reset = jwalk:set(Path, Glossary1, <<"SGML">>,proplist),
                   ?assertEqual(Glossary, Reset)
           end},
          {"jwalk:set list element",
           fun() ->
                   Orig = jwalk:get({"menu", "popup", "menuitem", 2}, Menu),
                   New = jwalk:set({"onclick"}, Orig, <<"OpenFile()">>,proplist),
                   Menu1 = jwalk:set({"menu", "popup", "menuitem", 2}, Menu, New,proplist),
                   ?assertEqual(New,
                                jwalk:get({"menu", "popup", "menuitem", 2}, Menu1)),
                   Reset = jwalk:set({"menu", "popup", "menuitem", 2}, Menu1, Orig,proplist),
                   ?assertEqual(Menu, Reset)
           end},
          {"jwalk:set list element path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", 2, "onclick"},
                   Orig = jwalk:get(Path, Menu),
                   New = <<"OpenFile()">>,
                   Menu1 = jwalk:set(Path, Menu, New,proplist),
                   ?assertEqual(New, jwalk:get(Path, Menu1)),
                   Reset = jwalk:set(Path, Menu1, Orig,proplist),
                   ?assertEqual(Menu, Reset)
           end},
          {"jwalk:set list element path first, last",
           fun() ->
                   FPath = {"menu", "popup", "menuitem", first, "value"},
                   LPath = {"menu", "popup", "menuitem", last, "value"},
                   FMenu = jwalk:set(FPath, Menu, <<"create">>,proplist),
                   LMenu = jwalk:set(LPath, FMenu, <<"kill">>,proplist),
                   ?assertEqual(<<"create">>, jwalk:get(FPath, FMenu)),
                   ?assertEqual(<<"create">>, jwalk:get(FPath, LMenu)),
                   ?assertEqual(<<"kill">>, jwalk:get(LPath, LMenu))
           end},
          {"jwalk:set new list element",
           fun() ->
                   Path = {"menu", "popup", "menuitem", new},
                   Path1 = {"menu", "popup", "menuitem", first},
                   Menu1 = jwalk:set(Path, Menu, <<"first-item">>,proplist),
                   ?assertEqual(<<"first-item">>, jwalk:get(Path1, Menu1)),
                   List = jwalk:get({"menu", "popup", "menuitem"}, Menu1),
                   ?assertEqual(4, length(List))
           end},
          {"jwalk:set_p creates intermediate missing nodes",
           fun() ->
                   StartData = [],
                   EndData = [{<<"a">>,
                      [{<<"b">>,
                           [{<<"c">>, <<"value">>}]
                      }]
                   }],
                   Path = {"a", "b", "c"},
                   Result = jwalk:set_p(Path, StartData, <<"value">>,proplist),
                   ?assertEqual(EndData, Result),
                   ?assertEqual(<<"value">>, jwalk:get(Path, Result)),
                   Path2 = {"1", "2"},
                   Result2 = jwalk:set_p(Path2, Result, <<"other-value">>,proplist),
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
                   Menu1 = jwalk:set_p(Path, Menu, Val,proplist),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path2, Menu1)),
                   ?assertEqual([<<"Edit">>], jwalk:get(Path3, Menu1))
           end},
          {"jwalk:set_p starting with an empty structure",
           fun() ->
                   Path = {"menu", "popup","menuitem",new},
                   Path2 = {"menu", "popup","menuitem",first},
                   Val =  [{<<"onclick">>, <<"CreateNewDoc()">>},{<<"value">>, <<"New">>}],
                   Menu1 = jwalk:set_p(Path, [], Val, proplist),
                   ?assertMatch(Val, jwalk:get(Path2, Menu1))
           end},
          {"jwalk:set using selector on non-array",
           fun() ->
                   ?assertException(error, {selector_used_on_non_array,first,_},
                                    jwalk:set({<<"menu">>,<<"id">>,first},Menu,true,proplist)),
                   ?assertException(error, {selector_used_on_object,first,__},
                                    jwalk:set({"menu","popup","menuitem",first,first},Menu, true,proplist))
           end},
          {"jwalk:set_p using selector on object",
           fun() ->
                   ?assertException(error, {selector_used_on_non_array,first,_},
                                    jwalk:set_p({<<"menu">>,<<"id">>,first},Menu,true,proplist)),
                   ?assertException(error, {selector_used_on_object,first,__},
                                    jwalk:set_p({"menu","popup","menuitem",first,first},Menu, true,proplist))
           end},
          {"jwalk:remove",
           fun() ->
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry", "Abbrev"},
                   Orig = jwalk:get(Path, Glossary),
                   ?assert(undefined /= Orig),
                   Glossary1 = jwalk:delete(Path, Glossary, proplist),
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
                   Menu1 = jwalk:delete(Path, Menu, proplist),
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
                   Menu1 = jwalk:delete(Path, Menu, proplist),
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
                   Menu1 = jwalk:delete(Path, Menu, proplist),
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
            ?_assertException(error, {index_for_non_list, _},
                              jwalk:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", 1}, Glossary)),
            ?_assertException(error, {index_for_non_list, _},
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
