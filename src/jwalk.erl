%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@prodigy.net>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%% The jwalk module is intended to make it easier to work with Erlang encodings
%%% of JSON - either Maps, Proplists or eep18-style representations.
%%%
%%% This work is inspired (stolen) from [https://github.com/seth/ej] but handles 
%%% maps, proplists and eep18 representations of JSON but not mochijson's 
%%% struct/tuple encodings.
%%%
%%% Functions always take at least two parameters: a first parameter which is a
%%% tuple of elements representing a Path into a JSON Object, and a second 
%%% parameter which is expected to be a proplist, map or eep18 representation 
%%% of a JSON structure.
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
%%% an Array that have a {"name":"value"} Member. 
%%%
%%% new: for set/2 and set_p/2, when the final element of a path is the atom 
%%% `new', the supplied value is added to the stucture as the first element of
%%% an array, the array is created if necessary.
%%%
%%% Path, string elements can be binary or not
%%%
%%% ```
%%% Cars = {[{<<"cars">>, [ {[{<<"color">>, <<"white">>}, {<<"age">>, <<"old">>}]},
%%%                         {[{<<"color">>, <<"red">>},  {<<"age">>, <<"old">>}]},
%%%                         {[{<<"color">>, <<"blue">>}, {<<"age">>, <<"new">>}]}
%%%                       ] }]}.
%%% '''
%%% Then 
%%%
%%% ```
%%% jwalk:get({"cars", {select, {"age", "old"}}}, Cars).
%%%
%%% [ {[{<<"color">>,<<"white">>},{<<"age">>,<<"old">>}]},
%%%   {[{<<"color">>,<<"red">>},{<<"age">>,<<"old">>}]} ]
%%%
%%% jwalk:get({"cars", {select, {"age", "old"}}, 1}, Cars).
%%% {[{<<"color">>,<<"white">>},{<<"age">>,<<"old">>}]}
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


-define(IS_EEP(X),  (X == [{}] orelse 
                     is_tuple(X) andalso 
                     is_list(element(1, X)) andalso 
                     (hd(element(1,X)) == {} orelse 
                     tuple_size(hd(element(1,X))) == 2))).

-define(IS_PL(X), (X == {[]} orelse 
                   (is_list(X) andalso is_tuple(hd(X)) andalso
                   (tuple_size(hd(X)) == 0 orelse tuple_size(hd(X)) == 2)))).

-define(IS_OBJ(X), (is_map(X) orelse ?IS_PL(X) orelse ?IS_EEP(X))).
-define(EMPTY_STRUCT(X), (X == [{}] orelse X == {[]})).
-define(EMPTY_OBJ(X), (X == [{}] orelse X == {[]} orelse X == #{})).
-define(NOT_MAP_OBJ(X), (not is_map(X)) andalso ?IS_OBJ(X)).

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
         (K == {select, all}) orelse
         (is_tuple(K) andalso (element(1, K) == select)))).

-define(IS_INDEX(K), 
        (K == first) orelse
        (K == last) orelse 
        is_integer(K)).

-define(NOT_SELECTOR(K), (not ?IS_SELECTOR(K))).

-type obj_type() :: map | proplist | eep.
-type name()     :: binary() | string().
-type value()    :: binary() | string() | number() | delete | true | false | null| integer().
-type select()   :: {select, {name(), value()}}.
-type p_index()  :: 'first' | 'last' | non_neg_integer().
-type p_elt()    :: name() | select() | p_index() | 'new'.
-type path()     :: {p_elt()} | [p_elt(),...].
-type pl()       :: [{}] | [{name(), value()  | pl() | [pl()]},...].
-type eep()      :: {[]} | {[{name(), value() | eep() | [eep(),...]}]}.
-type obj()      :: map() | pl() | eep() | [pl(),...] | [eep(),...].
-type jwalk_return() :: obj() | undefined | [obj() | value() | undefined,...].

-export_type ([jwalk_return/0]).



%% ----------------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% @doc Remove the value at the location specified by `Path' and return the
%% new representation. 
%%
%% Path elements that are strings can be binary or not - they are converted to 
%% binary if not. 
%%
-spec delete(path(), obj()) -> jwalk_return().

delete(Path, Obj) ->
    case rep_type(Obj) of
        error ->
            error({illegal_object, Obj});
        Type ->
            do_set(to_binary_list(Path), Obj, delete, [], false, Type)
    end.

    
%% -----------------------------------------------------------------------------
%% @doc Return a value from an `Obj'. 
%%
%% Path elements that are strings can be binary or not - they are converted to
%% binary if not.
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
%% @doc Return a value from an `Obj' or Default if value not found. 
%%
%% Path elements that are strings can be binary or not - they are converted to
%% binary if not.
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
%% The atom, `new', applied to an ARRAY, will make the Value the first Element 
%% in an Array, creating that Array if necessary.
%%
%% Path elements that are strings can be binary or not - they are converted to
%% binary if not.
%%
-spec set(path(), obj(), value()) -> jwalk_return().

set(Path, Obj, Value) ->
    case rep_type(Obj) of
        error ->
            error({illegal_object, Obj});
        Type ->
            do_set(to_binary_list(Path), Obj, Value, [], false, Type)
    end.


%% -----------------------------------------------------------------------------
%% @doc Same as {@link set/3. set/3} but creates intermediary elements as 
%% necessary. 
%%
-spec set_p(path(), obj(), value()) -> jwalk_return().

set_p(Path, Obj, Value) ->
    case rep_type(Obj) of
        error ->
            error({illegal_object, Obj});
        Type ->
            do_set(to_binary_list(Path), Obj, Value, [], true, Type)
    end.


-spec do_set(path(), obj(), value(), [tuple()], boolean(), obj_type()) -> jwalk_return().
do_set(Path, Obj, Value, Acc, P, RepType) ->
    try 
        set_(Path, Obj, Value, Acc, P, RepType)
    catch
        throw:Error ->
            error(Error)
    end.



%% -----------------------------------------------------------------------------
%%                WALK and SET_ INTERNAL FUNCTIONS
%% -----------------------------------------------------------------------------

-spec walk(path(), obj()|[]) -> jwalk_return().
% Some base cases.
walk([{select, {_,_}}|_], []) ->  [];

walk([], _) -> undefined;

walk(_, []) -> undefined;

walk(_, null) -> undefined;

walk([{select, {_, _}}|_], Obj) when ?IS_OBJ(Obj) ->
    throw({selector_used_on_object, Obj});

walk([S|_], Obj) when ?IS_OBJ(Obj), ?IS_SELECTOR(S) -> 
    throw({index_for_non_array, Obj});

walk([Name|Path], Obj) when ?IS_OBJ(Obj) -> 
    continue(get_member(Name, Obj), Path);

walk([S|Path], {[_|_]=Array}) ->
    walk([S|Path], Array);

% ARRAY with a Selector/Index: continue with selected subset.
walk([{select, {_,_}}=S|Path], [_|_]=Array) ->
    continue(subset_from_selector(S, Array), Path);

walk([S|Path], [_|_]=Array) when ?IS_INDEX(S) ->
    continue(nth(S, Array), Path);

% ARRAY with a Member Name: continue with the values from the Objects in the
% Array that have Member = {Name, Value}.
walk([Name|Path], [_|_]=Array) ->
    continue(values_from_member(Name, Array), Path);

% Element is something other than an ARRAY, but we have a selector.
walk([S|_], Element) when ?IS_SELECTOR(S) ->
    case S of
        {select, {_,_}} ->
            throw({selector_for_non_array, Element});
        _ ->
            throw({index_for_non_array, Element})
    end.


continue(false, _Path)                         -> undefined;
continue(undefined, _Path)                     -> undefined;
continue({_Name, Value}, Path) when Path == [] -> Value;
continue(Value, Path) when Path == []          ->  Value;
continue({_Name, Value}, Path)                 -> walk(Path, Value);
continue(Value, Path)                          -> walk(Path, Value).



-spec set_(path(), obj()|[], term(), [tuple()], boolean(), obj_type()) -> 
                                                                 jwalk_return().

% Final Path element: DELETE.
set_([Name], Obj, delete, _Acc, _IsP, _RType) when ?IS_OBJ(Obj) andalso
                                                          ?NOT_SELECTOR(Name) ->
    delete_member(Name, Obj);

set_([S], [_|_]=Array, delete, _Acc, _IsP, _RType) when ?IS_INDEX(S)->
    remove(Array, nth(S, Array));

set_([{select, {_,_}}=S], [_|_]=Array, delete, _Acc, _IsP, _RType) ->
    remove(Array, subset_from_selector(S, Array));

% Final Path element: remove Objects from Array with Member whose name = Name.
set_([Name], [_|_]=Array, delete, _Acc, _IsP, _RType) ->
    [delete_member(Name, O) || O <- Array, ?IS_OBJ(O)];


% Final Path element: if it exists in the OBJECT replace or create it.
set_([Name], Obj, Val, _Acc, _IsP, _RType) when ?IS_OBJ(Obj) andalso
                                                          ?NOT_SELECTOR(Name) ->
    add_member(Name, Val, Obj);


% Members applied to an empty object, if set_p, create it and move on
set_([Name|Ps], Obj, Val, _Acc, true, RType) when ?EMPTY_STRUCT(Obj),
                                                          ?NOT_SELECTOR(Name) ->
    eep_or_pl(RType, 
              [{Name, set_(Ps, empty(RType), Val, [], true, RType)}]);


% Iterate Members for one w/ name=Name. Replace value with recur call if found.
set_([Name|Ps]=Path, Obj, Val, Acc, IsP, RType) when ?NOT_MAP_OBJ(Obj), ?NOT_SELECTOR(Name) ->
    {N,V,Ms} = normalize_members(Obj), % first member Name, Value and rest of Members
    case Name of
        N ->
            NewVal = set_(Ps, V, Val, [], IsP, RType),
            eep_or_pl(RType,
                      lists:append(lists:reverse(Acc),[{N, NewVal}|Ms]));
        _Other when Ms /= [] ->
            set_(Path, Ms, Val, [{N,V}|Acc], IsP, RType);
        _Other when Ms == [], IsP == true->
            lists:append(lists:reverse(Acc), [{N,V},  {Name, set_(Ps, empty(RType), Val, [], IsP, RType)}]);
        _Other when Ms == []  -> 
            throw({no_path, Name})
    end;

% map case, 
set_([Name|Ps], Map, Val, _Acc, P, map) when is_map(Map), ?NOT_SELECTOR(Name) ->
    case map_get(Name, Map, not_found) of
        not_found -> 
            case P of
                true ->
                    maps:put(Name, set_(Ps, #{}, Val, [], P, map), Map);
                _ -> 
                    throw({no_path, Name})
            end;
        Value -> 
            maps:put(Name, set_(Ps, Value, Val, [], P, map), Map)
    end;

% When final Path elemenet is NEW applied to empty object, return Value in Array
set_([new], Obj, Val, _Acc, _P, _RType) when ?IS_J_TERM(Val), ?EMPTY_OBJ(Obj) ->
    [Val];
    
% Select_by_member applied to an empty Object. 
set_([{select,{K,V}}=S|Ks], Obj, Val, _Acc, P, RType) when ?EMPTY_OBJ(Obj) ->
    Object = case P of
                 true ->
                     [add_member(K, V, empty(RType))];
                  false -> 
                    throw({no_path, S})
              end,
    set_(Ks, Object, Val, [], P, RType);

set_([S|_], Obj, _V, _A, _P, _IsMap) when ?IS_SELECTOR(S) andalso ?IS_OBJ(Obj)-> 
    throw({selector_used_on_object, S, Obj});
    
% ALL OBJECT CASES HANDLED ABOVE %

% New applied to an ARRAY creates Value as the first element in  Array.
set_([new], [_|_]=Array, Val, _Acc, _P, _RType) ->
    [Val|Array];


% Final Path element is 'select by member' applied to ARRAY. Set / replace the 
% selected Objects with the Value
set_([{select,{_,_}}=S], {[_|_]=Array}, Val, Acc, P, RType) when ?IS_OBJ(Val) ->
    set_([S], Array, Val, Acc, P, RType);

set_([{select,{K,V}}=S], [_|_]=Array, Val, _Acc, _P, RType) when ?IS_OBJ(Val) ->
    Found = subset_from_selector(S, Array),
    Replace = case Found of
                  [] -> 
                      merge_members([add_member(K, V, empty(RType))], Val);
                  Found -> 
                      merge_members(Found, Val)
              end,
    replace_object(Array, Found, Replace);


set_([{select,{_,_}}], _Array, Val, _Acc, _P, _RType) -> 
    throw({replacing_object_with_value, Val});


% Intermediate Path element is a Select_by_member applied to an ARRAY. 
set_([{select,{K,V}}=S|Ks], Array, Val, _Acc, P, RType) ->
    Found = subset_from_selector(S, Array),
    Objects = case Found of
                  [] when P -> 
                      [add_member(K, V, empty(RType))];
                  [] -> 
                    throw({no_path, S});
                  _ ->
                      Found
              end,
    Replaced = set_(Ks, Objects, Val, [], P, RType),
    replace_object(Array, Found, Replaced);



% Path component is index, make Val the index-ed element of the Array.
set_([S|Path], [_|_]=Array, Val, _Acc, P, RType) when ?IS_SELECTOR(S) ->
    N = index_to_n(Array, S),
    case Path of
        [] ->
            lists:sublist(Array, 1, min(1, N-1)) ++
                [Val] ++  
                lists:sublist(Array, N + 1, length(Array));
        _More when P ; N =<length(Array) ->
            lists:sublist(Array, 1, min(1, N-1)) ++
                [set_(Path, lists:nth(N, Array), Val, [], P, RType)] ++
                lists:sublist(Array, N + 1, length(Array));
        _More ->
            throw({no_path, S})
    end;

set_([S|_], NotArray, _Val, _Acc, _P, _RType) when ?IS_SELECTOR(S) -> 
    throw({selector_used_on_non_array, S, NotArray});


% Final Path component is a Name, target is an ARRAY, replace/add Member to all
% selected Objects pulled from the Array.
set_([Name], [_|_]=Array, Val, _Acc, _P, RType) ->
    case found_elements(Name, Array) of
        undefined ->
           merge_members(Array, add_member(Name, Val, empty(RType)));
        Found -> 
            case ?IS_OBJ(Val) of 
                true ->
                    merge_members(remove(Array, Found), Val);
                false -> 
                    replace_member(Name, Array, Val)
            end
    end;


% Path component is a Name, target is an ARRAY,  Set will recursively process
% the selected objects containing a Member with name Name, with the ballance
% ballance of the Path. 
set_([Name|Keys], [_|_]=Array, Val, _Acc, P, RType) ->
    Found = found_elements(Name, Array),
    Objects = case Found of
                  undefined when P ->
                      add_member(Name, 
                                 set_(Keys, empty(RType), Val, [], P, RType), 
                                 empty(RType));
                  undefined ->
                      throw({no_path, Name});
                  _ -> 
                      set_(Keys, Found, Val, [], P, RType)
              end,
    case Found of
        undefined ->
            merge_members(Array, Objects);
        _ ->
            merge_members(Array, [{Name, Objects}])
    end.




%% ----------------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%% ----------------------------------------------------------------------------
replace_member(Name, Array, Val) ->
    F = fun(Obj, Acc) when ?IS_OBJ(Obj) ->                
                case get_member(Name, Obj) of
                    undefined -> [Obj|Acc];
                    _Value ->
                        [add_member(Name, Val, Obj) | Acc]
                end;
           (Element, Acc) ->
                [Element|Acc]
        end,
    lists:reverse(lists:foldl(F, [], Array)).


replace_object(Same, Same, New) -> 
    New;
replace_object(Array, [], New) ->
     lists:append(Array,New);

replace_object(Array, [Old], [New]) ->
     F = fun(Obj) when Obj == Old -> New;
             (Other) -> Other
          end,
      lists:map(F, Array);
replace_object(Array, [Old], New) ->
     F = fun(Obj) when Obj == Old -> New;
             (Other) -> Other
          end,
      lists:map(F, Array).



found_elements(Name, Array) ->
    case values_from_member(Name, Array) of
        undefined -> 
            undefined;
        EList -> 
            case lists:filter(fun(R) -> R /= undefined end, EList) of
                [] -> undefined;
                Elements ->
                    Elements
            end
    end.
             

% Return list of Results from trying to get Name from each Obj an Array.
-spec values_from_member(name(), [obj(),...]) -> [obj(),...] | undefined.

values_from_member(Name, Array) ->
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


% Select out subset of Object/s that contain Member {K:V}
-spec subset_from_selector(select(), [obj()]) -> [obj()].

subset_from_selector({select, {K,V}}, Array) -> 
    F = fun(Obj) when ?IS_OBJ(Obj) -> 
                get_member(K, Obj) == V;
           (_) -> false
        end,
    lists:filter(F, Array).



% Select out nth Object from Array.
-spec nth(p_index(), [obj()]) -> obj().

nth(first, L) ->
    hd(L);
nth(last, L) ->
    lists:last(L);
nth(N, L)  when N =< length(L) ->
    lists:nth(N, L);
nth(N, L)  when N > length(L) ->
    throw({error, index_out_of_bounds, N, L}).


-spec remove([obj()], [obj()]) -> [obj()].
remove(Objects, []) -> Objects;
remove(Objects, Remove) -> 
    lists:reverse(ordsets:to_list(
                    ordsets:subtract(ordsets:from_list(Objects),
                                     ordsets:from_list(Remove)))).


%% Representation-specifc object manipulation: adding, deleteing members, etc.

-spec get_member(name(), obj()) -> term() | 'undefined'.
get_member(Name, #{}=Obj) ->
    map_get(Name, Obj, undefined);

get_member(Name, {PrpLst}) ->
    proplists:get_value(Name, PrpLst, undefined);

get_member(Name, Obj) ->
    proplists:get_value(Name, Obj, undefined).


-spec delete_member(name(), obj()) -> obj().
delete_member(Name, #{}=Obj) ->
    maps:remove(Name, Obj);

delete_member(Name, {PrpLst}) ->
    proplists:delete(Name, PrpLst);

delete_member(Name, Obj) ->
    proplists:delete(Name, Obj).


-spec add_member(name(), value(), obj()) -> obj().
add_member(Name, Val, #{}=Obj) ->
    maps:put(Name, Val, Obj);

add_member(Name, Val, [{}]) ->
    [{Name, Val}];

add_member(Name, Val, {[]}) ->
    {[{Name, Val}]};

add_member(Name, Val, [{_,_}|_]=Obj) ->
    lists:keystore(Name, 1, Obj, {Name, Val});

add_member(Name, Val, {[{_,_}|_]=PrpLst}) ->
    {lists:keystore(Name, 1, PrpLst, {Name, Val})}.


merge_members([#{}|_] = Maps, Target) ->
    [maps:merge(M, Target) || M <- Maps];
merge_members(Objects, M) ->
    [merge_pl(O, M) || O <- Objects].

merge_pl(P1, [{K,V}|Ts]) when ?IS_PL(P1) ->
    merge_pl(lists:keystore(K, 1, P1, {K,V}), Ts);

merge_pl({P1}, [{K,V}|Ts]) ->
    merge_pl({lists:keystore(K, 1, P1, {K,V})}, Ts);

merge_pl({P1}, {[{K,V}|Ts]}) ->
    merge_pl({lists:keystore(K, 1, P1, {K,V})}, {Ts});

merge_pl(P1, {[]}) ->
    P1;
merge_pl(P1, []) ->
    P1.




index_to_n(_Array, first) -> 1;
index_to_n(Array, last) -> length(Array);
index_to_n(_Array, Integer) -> Integer.


to_binary_list(Keys) ->
    L = case is_tuple(Keys) of
            true -> tuple_to_list(Keys);
            false -> Keys
        end,
    lists:map(fun(K) -> make_binary(K) end, L).



make_binary(K) when is_binary(K); is_number(K) -> K;
make_binary(K) when is_list(K) -> list_to_binary(K);
make_binary(K) when is_atom(K) -> K;
make_binary({select, {K, V}}) -> 
    {select, {make_binary(K), make_binary(V)}}.


% looks for the first object and returns its representation type.
rep_type(#{}) -> map;
rep_type([{}]) -> proplist;
rep_type({[]}) -> eep;
rep_type([#{}|_]) -> map;
rep_type([{_,_}|_]) -> proplist;
rep_type({[{_,_}|_]}) -> eep;
rep_type({[H|_]})  -> rep_type(H);
rep_type([H|_TL]) when is_list(H) -> rep_type(H);
rep_type(_) -> error.



% In support erlang 17 need to roll my own get/3
-spec map_get(name(), #{}, term()) -> term().

map_get(Key, Map, Default) ->
     try  maps:get(Key, Map) of
          Value ->
             Value
     catch
         _:_ ->
             Default
     end.
        

eep_or_pl(proplist, Item) ->
    Item;
eep_or_pl(eep, Item) ->
    {Item}.

empty(proplist) ->
    [{}];
empty(eep) ->
    {[]};
empty(map) ->
    #{}.


normalize_members([{N,V}|Ms]) ->
    {N, V, Ms};
normalize_members({[{N,V}|Ms]}) ->
    {N, V, Ms}.



-ifdef(TEST).

ej_eep_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("./test/widget.eep_terms"),
         {ok, [Glossary]} = file:consult("./test/glossary.eep_terms"),
         {ok, [Menu]} = file:consult("./test/menu.eep_terms"),
         ObjList = {[{<<"objects">>,
                      [ {[{<<"id">>, I}]} ||
                          I <- lists:seq(1, 5) ]}]},
         {Widget, Glossary, Menu, ObjList}
 end, 
 fun({Widget, Glossary, Menu, ObjList}) ->
         [{"jwalk:get",
           [
            ?_assertMatch({[{_, _}|_]}, jwalk:get({"widget"}, Widget)),
            ?_assertEqual(<<"1">>, jwalk:get({"widget", "version"}, Widget)),
            ?_assertEqual(250, jwalk:get({"widget", "image", "hOffset"}, Widget)),
            ?_assertEqual([1,2,3,4,5], jwalk:get({"widget", "values"}, Widget)),
            ?_assertEqual(2, jwalk:get({"widget", "values", 2}, Widget)),
            ?_assertEqual(4, jwalk:get({"widget", "values", 4}, Widget)),
            ?_assertEqual(1, jwalk:get({"widget", "values", first}, Widget)),
            ?_assertEqual(5, jwalk:get({"widget", "values", last}, Widget)),
            ?_assertEqual({[{<<"id">>, 5}]},
                          jwalk:get({<<"objects">>, last}, ObjList)),
            ?_assertEqual({[{<<"id">>, 1}]},
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
              ?assertMatch([{[{<<"value">>,<<"New">>}|_]}], jwalk:get(Path1, Menu)),
              Path2 = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
              ?assertEqual([<<"CreateNewDoc()">>], jwalk:get(Path2, Menu)),
              PathNoneMatched = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}},
              ?assertEqual([], jwalk:get(PathNoneMatched, Menu)),
              PathDoesntExist = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}, "bar"},
              ?assertEqual(undefined, jwalk:get(PathDoesntExist, Menu)),
              Data = {[
                       {[{<<"match">>, <<"me">>}]},
                       {[{<<"match">>, <<"me">>}]}
                      ]},
              ComplexBeginning = {{select, {"match", "me"}}},
              ?assertMatch([{_}, {_}], jwalk:get(ComplexBeginning, Data)),
              ComplexBeginningDeeper = {{select, {"match", "me"}}, "match"},
              ?assertMatch([<<"me">>, <<"me">>], jwalk:get(ComplexBeginningDeeper, Data))
            end},
          {"jwalk:get with multi-level array matching",
           fun() ->
                %% When doing multilevel deep array matching, we want the
                %% array returned to be a single top level list, and not
                %% a nested list of lists ...
                Data = {[
                   {<<"users">>, [
                         {[{<<"id">>,<<"sebastian">>},
                                  {<<"books">>, [
                                     {[{<<"title">>, <<"faust">>},
                                       {<<"rating">>, 5}]}
                                  ]}
                         ]}
                   ]}
                ]},
                Path = {"users", {select, {"id", "sebastian"}}, "books",
                        {select, {"title", "faust"}}, "rating"},
                Result = jwalk:get(Path, Data),
                ?assertEqual([5], Result)
            end},

          {"jwalk:set_p creates intermediate missing nodes",
           fun() ->
                   StartData = {[]},
                   EndData = {[{<<"a">>,
                      {[{<<"b">>,
                          { [{<<"c">>, <<"value">>}]}
                      }]}
                   }]},
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
          {"jwalk:set new value in an object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "alt"},
                   Val = <<"helptext">>,
                   Menu1 = jwalk:set(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path, Menu1))
           end},
          {"jwalk:set_p value in a non-existent object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem",
                           {select, {"value", "Edit"}}},
                   Path2 = {"menu", "popup", "menuitem",
                            {select, {"value", "Edit"}}, "text"},
                   Path3 = {"menu", "popup", "menuitem",
                            {select, {"value", "Edit"}}, "value"},
                   Val = { [{<<"text">>, <<"helptext">>}]},
                   Menu1 = jwalk:set_p(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path2, Menu1)),
                   ?assertEqual([<<"Edit">>], jwalk:get(Path3, Menu1))
           end},

          {"jwalk:set new value in a object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem",
                           {select, {"value", "New"}}},
                   Path2 = {"menu", "popup", "menuitem",
                            {select, {"value", "New"}}, "onclick"},
                   Val = { [{<<"onclick">>, <<"CreateDifferentNewDoct()">>}]},
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
                   StartData = { [
                      { [{<<"match">>, <<"me">>}, {<<"param">>, 1}]},
                      { [{<<"match">>, <<"me">>}, {<<"param">>, 2}]}
                   ]},
                   Path = {{select, {"match", "me"}}},
                   Path2 = {{select, {"match", "me"}}, "more"},
                   Val = { [{<<"more">>, <<"content">>}]},
                   Result = jwalk:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], jwalk:get(Path2, Result))
           end},

          {"jwalk:set replace multiple children deep in a complex path",
           fun() ->
                   %% We want the ability to affect multiple array elements
                   %% when a complex selector returns more than one match.
                   %% In this case we show that the array does not have to
                   %% be at the top level.
                   StartData = { [{<<"parent">>, [
                          { [{<<"match">>, <<"me">>}, {<<"param">>, 1}]},
                          { [{<<"match">>, <<"me">>}, {<<"param">>, 2}]}
                          ]}
                   ]},
                   Path = {"parent", {select, {"match", "me"}}},
                   Path2 = {"parent", {select, {"match", "me"}}, "more"},
                   Val = { [{<<"more">>, <<"content">>}]},
                   EndData = jwalk:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], jwalk:get(Path2, EndData))
           end},

          {"jwalk:set should not allow replacing an array element at a complex path with a pure value",
           fun() ->
                   %% If the user has made a filtered selection on an array,
                   %% then all the elements in the array are objects.
                   %% Replacing the matched selection with a non-object value
                   %% will break this constraint.
                   Data = { [{ [{<<"match">>, <<"me">>}] }] },
                   Path = {{select, {"match", "me"}}},
                   Val = <<"pure-value-and-not-a-struct">>,
                   ?assertException(error, {replacing_object_with_value, _},
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
                   ?assertEqual(NewVal, jwalk:get({"widget", "version"}, NewWidget)),
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
           end}
         ]
 end
}.






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
            ?_assertEqual(not_found, jwalk:get({"widget", "keys", 2}, Widget, not_found)),
            ?_assertEqual(5, jwalk:get({"widget", "values", last}, Widget, not_found)),
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
          {"jwalk:set fails on non-object",
           fun() ->
                   ?assertException(error, {illegal_object, _},
                                     jwalk:set({"does"}, [1,2], 1)),
                   ?assertException(error, {illegal_object, _},
                                     jwalk:set_p({"does"}, [1,2], 1))
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
                   ?assertMatch(Val, jwalk:get(Path2, Menu1)),
                   Path3 = {"users", {select, {"name", "sebastian"}}, "location"},
                   Val3 = <<"Germany">>,
                   Result3 = [{<<"users">>,[[{<<"name">>,<<"sebastian">>},{<<"location">>,<<"Germany">>}]]}],
                   ?assertMatch(Result3, jwalk:set_p(Path3, [{}], Val3))
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
                   ?assertException(error, {illegal_object,_},
                                    jwalk:delete({<<"menu">>,<<"id">>,first},true)),
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
            ?_assertEqual(not_found, jwalk:get({"widget", "keys", 2}, Widget, not_found)),
            ?_assertEqual(5, jwalk:get({"widget", "values", last}, Widget, not_found)),
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
                   ?assertMatch(Val, jwalk:get(Path2, Menu1)),
                   Path3 = {"users", {select, {"name", "sebastian"}}, "location"},
                   Val3 = <<"Germany">>,
                   Result3 = #{<<"users">> => [#{<<"location">> => <<"Germany">>,<<"name">> => <<"sebastian">>}]},
                   ?assertMatch(Result3, jwalk:set_p(Path3, #{}, Val3))
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

ej_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("./test/widget.termss"),
         {ok, [Glossary]} = file:consult("./test/glossary.termss"),
         {ok, [Menu]} = file:consult("./test/menu.termss"),
         ObjList = { [{<<"objects">>,
                              [ { [{<<"id">>, I}]} ||
                                  I <- lists:seq(1, 5) ]}]},
         {Widget, Glossary, Menu, ObjList}
 end,
 fun({Widget, Glossary, Menu, ObjList}) ->
         [{"jwalk:get",
           [
            ?_assertMatch({ [{_, _}|_]}, jwalk:get({"widget"}, Widget)),
            ?_assertMatch({ [{_, _}|_]}, jwalk:get(["widget"], Widget)),

            ?_assertEqual(<<"1">>, jwalk:get({"widget", "version"}, Widget)),
            ?_assertEqual(<<"1">>, jwalk:get(["widget", "version"], Widget)),

            ?_assertEqual(250, jwalk:get({"widget", "image", "hOffset"}, Widget)),
            ?_assertEqual(250, jwalk:get(["widget", "image", "hOffset"], Widget)),

            ?_assertEqual([1,2,3,4,5], jwalk:get({"widget", "values"}, Widget)),
            ?_assertEqual([1,2,3,4,5], jwalk:get(["widget", "values"], Widget)),

            ?_assertEqual(2, jwalk:get({"widget", "values", 2}, Widget)),
            ?_assertEqual(4, jwalk:get({"widget", "values", 4}, Widget)),
            ?_assertEqual(1, jwalk:get({"widget", "values", first}, Widget)),
            ?_assertEqual(5, jwalk:get({"widget", "values", last}, Widget)),

            ?_assertEqual(2, jwalk:get(["widget", "values", 2], Widget)),
            ?_assertEqual(4, jwalk:get(["widget", "values", 4], Widget)),
            ?_assertEqual(1, jwalk:get(["widget", "values", first], Widget)),
            ?_assertEqual(5, jwalk:get(["widget", "values", last], Widget)),

            ?_assertEqual({ [{<<"id">>, 5}]},
                          jwalk:get({<<"objects">>, last}, ObjList)),
            ?_assertEqual({ [{<<"id">>, 1}]},
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

            ?_assertEqual(undefined,
                          jwalk:get({"not_present"}, {[]})),

            ?_assertEqual(undefined,
                          jwalk:get({"not_present"}, { []})),

            ?_assertEqual(undefined,
                          jwalk:get({"root", "not_present"}, { [{<<"root">>, null}]})),

            ?_assertEqual(undefined, jwalk:get({[]}, Widget)),

            ?_assertException(error, {index_for_non_array, _},
                              jwalk:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", 1}, Glossary)),

            ?_assertException(error, {index_for_non_array, _},
                              jwalk:get({"glossary", "title", 1}, Glossary))]},

          {"jwalk:get with default",
           [
            ?_assertEqual(<<"1">>, jwalk:get({"widget", "version"}, Widget, "you'll never see this default")),
            ?_assertEqual(<<"defaults rock">>, jwalk:get({"widget", "NOT_PRESENT"}, Widget, <<"defaults rock">>)),
            ?_assertEqual(<<"a default">>, jwalk:get({}, Widget, <<"a default">>)),
            ?_assertEqual(<<"a default">>, jwalk:get({[]}, Widget, <<"a default">>))
           ]},

          {"jwalk:get with json_plist",
           [
            ?_assertEqual(<<"1">>, jwalk:get({"a"}, [{<<"a">>, <<"1">>}])),
            ?_assertEqual(undefined, jwalk:get({"x"}, [{<<"a">>, <<"1">>}])),
            ?_assertEqual(undefined, jwalk:get({"x"}, []))
           ]},

          {"jwalk:get from array by matching key",
           fun() ->
              Path1 = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
              ?assertMatch([{ [{<<"value">>,<<"New">>}|_]}], jwalk:get(Path1, Menu)),
              Path2 = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
              ?assertEqual([<<"CreateNewDoc()">>], jwalk:get(Path2, Menu)),
              PathNoneMatched = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}},
              ?assertEqual([], jwalk:get(PathNoneMatched, Menu)),
              PathDoesntExist = {"menu", "popup", "menuitem", {select, {"value", "NotThere"}}, "bar"},
              ?assertEqual(undefined, jwalk:get(PathDoesntExist, Menu)),
              Data = [
                  { [{<<"match">>, <<"me">>}]},
                  { [{<<"match">>, <<"me">>}]}
              ],
              ComplexBeginning = {{select, {"match", "me"}}},
              ?assertMatch([{ _}, { _}], jwalk:get(ComplexBeginning, Data)),
              ComplexBeginningDeeper = {{select, {"match", "me"}}, "match"},
              ?assertMatch([<<"me">>, <<"me">>], jwalk:get(ComplexBeginningDeeper, Data))
            end},

          {"jwalk:get with multi-level array matching",
           fun() ->
                %% When doing multilevel deep array matching, we want the
                %% array returned to be a single top level list, and not
                %% a nested list of lists ...
                Data = {[
                   {<<"users">>, [
                         {[{<<"id">>,<<"sebastian">>},
                                  {<<"books">>, [
                                     { [{<<"title">>, <<"faust">>},
                                               {<<"rating">>, 5}]}
                                  ]}
                         ]}
                   ]}
                ]},
                Path = {"users", {select, {"id", "sebastian"}}, "books",
                        {select, {"title", "faust"}}, "rating"},
                Result = jwalk:get(Path, Data),
                ?assertEqual([5], Result)
            end},

          {"jwalk:get filter at top-level",
           fun() ->
                   Data = {[{<<"users">>,
                                    [{[{<<"company">>,<<"opscode">>},
                                              {<<"name">>,<<"seth">>}]},
                                     {[{<<"location">>,<<"Germany">>},
                                              {<<"name">>,<<"sebastian">>},
                                              {<<"company">>,<<"aircloak">>}]}]}]},
                   ?assertEqual([<<"opscode">>, <<"aircloak">>],
                                jwalk:get({"users", "company"}, Data))
           end},


          {"jwalk:set, replacing existing value, keys is tuple",
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

          {"jwalk:set replacing existing value, keys is lists",
           fun() ->
                   Path = ["widget", "window", "name"],
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
                   ?assertEqual(NewVal, jwalk:get({"widget", "version"}, NewWidget)),
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

          {"jwalk:set_p creates intermediate missing nodes, keys is tuple",
           fun() ->
                   StartData = {[]},
                   EndData = {[{<<"a">>,
                      {[{<<"b">>,
                          { [{<<"c">>, <<"value">>}]}
                      }]}
                   }]},
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

           {"jwalk:set_p creates intermediate missing nodes, keys is lists",
           fun() ->
                   StartData = {[]},
                   EndData = {[{<<"a">>,
                      {[{<<"b">>,
                          { [{<<"c">>, <<"value">>}]}
                      }]}
                   }]},
                   Path = ["a", "b", "c"],
                   Result = jwalk:set_p(Path, StartData, <<"value">>),
                   ?assertEqual(EndData, Result),
                   ?assertEqual(<<"value">>, jwalk:get(Path, Result)),
                   Path2 = {"1", "2"},
                   Result2 = jwalk:set_p(Path2, Result, <<"other-value">>),
                   ?assertEqual(<<"other-value">>, jwalk:get(Path2, Result2)),
                   %% Does not affect existing values
                   ?assertEqual(<<"value">>, jwalk:get(Path, Result2))
           end},


          {"jwalk:set new value in an object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "alt"},
                   Val = <<"helptext">>,
                   Menu1 = jwalk:set(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path, Menu1))
           end},

          {"jwalk:set_p value in a non-existent object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "Edit"}}},
                   Path2 = {"menu", "popup", "menuitem", {select, {"value", "Edit"}}, "text"},
                   Path3 = {"menu", "popup", "menuitem", {select, {"value", "Edit"}}, "value"},
                   Val = { [{<<"text">>, <<"helptext">>}]},
                   Menu1 = jwalk:set_p(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path2, Menu1)),
                   ?assertEqual([<<"Edit">>], jwalk:get(Path3, Menu1))
           end},

          {"jwalk:set new value in a object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", {select, {"value", "New"}}},
                   Path2 = {"menu", "popup", "menuitem", {select, {"value", "New"}}, "onclick"},
                   Val = { [{<<"onclick">>, <<"CreateDifferentNewDoct()">>}]},
                   Menu1 = jwalk:set(Path, Menu, Val),
                   ?assertEqual([<<"CreateDifferentNewDoct()">>], jwalk:get(Path2, Menu1)),
                   Path3 = {"menu", "popup", "menuitem", {select,{"value", "New"}}, "speed"},
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
                   StartData = { [
                      { [{<<"match">>, <<"me">>}, {<<"param">>, 1}]},
                      { [{<<"match">>, <<"me">>}, {<<"param">>, 2}]}
                   ]},
                   Path = {{select, {"match", "me"}}},
                   Path2 = {{select, {"match", "me"}}, "more"},
                   Val = { [{<<"more">>, <<"content">>}]},
                   Result = jwalk:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], jwalk:get(Path2, Result))
           end},

          {"jwalk:set replace multiple children deep in a complex path",
           fun() ->
                   %% We want the ability to affect multiple array elements
                   %% when a complex selector returns more than one match.
                   %% In this case we show that the array does not have to
                   %% be at the top level.
                   StartData = { [{<<"parent">>, [
                          { [{<<"match">>, <<"me">>}, {<<"param">>, 1}]},
                          { [{<<"match">>, <<"me">>}, {<<"param">>, 2}]}
                          ]}
                   ]},
                   Path = {"parent", {select, {"match", "me"}}},
                   Path2 = {"parent", {select, {"match", "me"}}, "more"},
                   Val = { [{<<"more">>, <<"content">>}]},
                   EndData = jwalk:set(Path, StartData, Val),
                   ?assertMatch([<<"content">>, <<"content">>], jwalk:get(Path2, EndData))
           end},

          {"jwalk:set doesn't alter order when setting a complex path",
           fun() ->
                   StartData = { [{<<"parent">>, [
                          { [{<<"name">>, <<"alice">>}, {<<"param">>, 1}]},
                          { [{<<"name">>, <<"bob">>}, {<<"param">>, 2}]},
                          { [{<<"name">>, <<"clara">>}, {<<"param">>, 3}]}
                          ]}
                   ]},
                   Path = {"parent", {select, {"name", "bob"}}, "param"},
                   EndData = jwalk:set(Path, StartData, 4),
                   Names = [ jwalk:get({"name"}, Elt) || Elt <- jwalk:get({"parent"}, EndData) ],
                   ExpectNames = [<<"alice">>, <<"bob">>, <<"clara">>],
                   ?assertEqual(ExpectNames, Names)
           end},

          {"jwalk:set should not allow replacing an array element at a complex path with a pure value",
           fun() ->
                   %% If the user has made a filtered selection on an array,
                   %% then all the elements in the array are objects.
                   %% Replacing the matched selection with a non-object value
                   %% will break this constraint.
                   Data = { [{ [{<<"match">>, <<"me">>}]}]},
                   Path = {{select, {"match", "me"}}},
                   Val = <<"pure-value-and-not-a-struct">>,
                   ?assertException(error, {replacing_object_with_value, _},
                                      jwalk:set(Path, Data, Val))
           end},

          {"jwalk:set a value within array",
           fun() ->
                   %% We should be able to set values on elements we
                   %% have filtered out of an array, rather than just
                   %% replacing them.
                   StartData = {[
                      {<<"users">>, [
                            {[{<<"id">>,<<"sebastian">>}]}
                      ]}
                   ]},
                   EndData = {[
                      {<<"users">>, [
                            {[{<<"id">>,<<"sebastian">>},
                                     {<<"books">>, []}
                            ]}
                      ]}
                   ]},
                   Path = {"users", {select, {"id", "sebastian"}}, "books"},
                   Val = [],
                   Result = jwalk:set(Path, StartData, Val),
                   ?assertEqual(EndData, Result)
           end},

          {"jwalk:set should throw error for trying to missing intermediate nodes",
           fun() ->
                   %% If we request a composite path that doesn't exist,
                   %% and we are using set, rather than set_p, then we
                   %% should get an error thrown at us.
                   Path = {{select, {"id", "seb"}}},
                   Val = { [{<<"continent">>, <<"europe">>}]},
                   ?assertException(error, {no_path, _},
                                    jwalk:set(Path, { []}, Val))
           end},
          {"jwalk:set_p should construct intermediate nodes if missing",
           fun() ->
                   %% If we request a composite path that doesn't exist,
                   %% the missing nodes should be created for us dynamically
                   %% to match the filtering criteria we are searching for.
                   StartData = {[]},
                   Path = {"users", {select, {"id", "seb"}}, "room",
                           {select, {"room_id", "living-room"}},
                           "books", {select, {"title", "faust"}}, "rating"},
                   Val = 5,
                   Result = jwalk:set_p(Path, StartData, Val),
                   ?assertEqual([5], jwalk:get(Path, Result))
           end},

          {"jwalk:set_p should create intermediate nodes if missing in existing structures",
           fun() ->
                   %% If we request a composite path that doesn't exist,
                   %% the missing nodes should be created for us dynamically
                   %% to match the filtering criteria we are searching for.
                   %% Furthermore, this should not affect old values already existing in the
                   %% same structure.
                   StartData = {[{<<"users">>,[
                        {[{<<"rooms">>,[
                                        {[{<<"books">>, [ {[{<<"rating">>,5},{<<"title">>,<<"faust">>}]} ]},
                                          {<<"room_id">>,<<"livingroom">>}
                                         ]}]},
                          {<<"id">>,<<"seb">>}]}
                                              ]}]},
                   ValidPath = {"users", {select, {"id", "seb"}},
                                "rooms", {select, {"room_id", "livingroom"}},
                                "books", {select, {"title", "faust"}}, "rating"},
                   ?assertEqual([5], jwalk:get(ValidPath, StartData)),
                   NewPath = {"users", {select, {"id", "seb"}},
                              "rooms", {select, {"room_id", "bathroom"}},
                              "sink"},
                   NewValue = true,
                   Result = jwalk:set_p(NewPath, StartData, NewValue),
                   ?assertEqual([true], jwalk:get(NewPath, Result)),
                   OtherPath = {"users", {select, {"id", "seb"}},
                                "computers", {select, {"laptop", true}}, "name"},
                   OtherValue = <<"paris">>,
                   Result1 = jwalk:set_p(OtherPath, Result, OtherValue),
                   io:format("~p", [Result1]),
                   ?assertEqual([<<"paris">>], jwalk:get(OtherPath, Result1)),
                   %% Old values still valid
                   ?assertEqual([5], jwalk:get(ValidPath, Result1)),
                   ?assertEqual([true], jwalk:get(NewPath, Result1))
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
                   Path = ["menu", "popup", "menuitem", {select, {"value", "New"}}],
                   Orig = jwalk:get(Path, Menu),
                   ?assert([] /= Orig),
                   Menu1 = jwalk:delete(Path, Menu),
                   ?assertEqual([], jwalk:get(Path, Menu1)),
                   % verify some structure
                   VerifyPath = ["menu", "popup", "menuitem", {select, {"value", "New"}}, "value"],
                   ?assertEqual(undefined, jwalk:get(VerifyPath, Menu1)),
                   % % verify that we didn't delete siblings
                   VerifyOpen = ["menu", "popup", "menuitem", {select, {"value", "Open"}}, "onclick"],
                   ?assertEqual([<<"OpenDoc()">>], jwalk:get(VerifyOpen, Menu1)),
                   VerifyClose = ["menu", "popup", "menuitem", {select, {"value", "Close"}}, "onclick"],
                   ?assertEqual([<<"CloseDoc()">>], jwalk:get(VerifyClose, Menu1))
           end}
         ]
 end
}.

-endif.
