%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@prodigy.net>
%%% @copyright (C) 2015, Jim Rosenblum
%%% @doc
%%% The jwalk modue is intended to make it easy to work with the Erlang
%%% encodings of JSON which return either MAPS or PROPERTY lists. 
%%%
%%% This work is really a rip-off of https://github.com/seth/ej
%%% Currently get/2 and get/3 are supported which allows one to walk a
%%% structure and return a particular value.
%%% @end
%%% Created : 20 Nov 2015 by Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% ----------------------------------------------------------------------------
-module(jwalk).

-export([get/2, get/3,
        set/3]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(IS_OBJ(X), is_map(X) orelse (is_list(X) andalso is_tuple(hd(X)))).

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
%% @doc Get takes a Path list and a strcture representing the encoded JSON.
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


-spec set(keys(), obj(), value()) -> jwalk_return().

set(Keys, Obj, Element) ->
    try 
        set_(to_binary_list(Keys), Obj, Element, [])
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
% the Key. If we can find the Element in an object, then the object gets 
% added to the return list.
having_element(Key, Array) ->
    Elements = [walk([Key], Obj) || Obj <- Array, ?IS_OBJ(Obj)],
    case Elements of
        [] -> undefined;
        _ -> dont_nest(Elements)
    end.



% Make sure that serial {select, {_,_}} doesn't give us nested lists of returns.
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



% Found the last part of the Path in an OBJECT, replace the value with Element.
set_([Name], [{Name, _V}|Tl], Element, Acc) when not ?IS_SELECTER(Name)->
    lists:flatten(lists:reverse(Acc),[{Name, Element}|Tl]);

% Member with Name doesn't exist in OBJECT, create Member.
set_([Name], [], Element, Acc) when not ?IS_SELECTER(Name)->
    lists:flatten(lists:reverse(Acc),[{Name, Element}]);

% Intermediate Path element does not exist.
set_([Name|_]=Path, [], _Element, _Acc) when not ?IS_SELECTER(Name)->
    throw({no_path, Path});

% Found an OBJECT's Member on the Path, that Member's Value will be the result 
% of the recursive call of set_ on its Value with the ballance of the Path.
set_([Name|Ks], [{Name, V}|Tl], Element, Acc) when not ?IS_SELECTER(Name)->
    New = set_(Ks, V, Element, []),
    lists:flatten(lists:reverse(Acc),[{Name, New}|Tl]);

% This OBJECT's head Member isn't relevant, try with the next Member
set_([Name|_]=Ks, [{N, V}|Tl], Element, Acc) when not ?IS_SELECTER(Name)->
    set_(Ks, Tl, Element, [{N,V}|Acc]);



% The path element is a select_by_member applied to an ARRAY. 
% If this is the last Path element, Add the Member to selected objects; 
% otherwise, replace the selected Objects with the recursive set_
set_([{select,{_, _}}=S|R], [O|_Os]=Array, Obj, Acc) when ?IS_OBJ(O) ->
    Objects = selector_to_element(S, Array),
    Replaced = case R of
                   [] ->
                       case Obj of
                           {K,V} -> 
                               O2 = remove_member(Objects, Obj),
                               add_member(O2, {K,V});
                           NotObj ->
                               throw({replacing_object_with_value,NotObj})
                       end;
                   _more -> 
                       set_(R, Objects, Obj, [])
               end,
    NewArray = remove(Array, Objects) ++ Replaced,
    [Result] = [lists:reverse(NewArray)|Acc], 
    Result;
            

% The final part of the path is an index, replace the target.
set_([S | R], Array, Element, _Acc) when is_integer(S); S == last; S == first ->
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
        _More ->
            lists:sublist(Array, 1, N-1) ++
                [set_(R, lists:nth(N, Array), Element, [])] ++
                lists:sublist(Array, N + 1, length(Array))
    end;

% Path component is a Name, target is an ARRAY,  Set will recursively 
% processes the selected objects containing a Member with name Name,
% with the ballance of the Path. If the Member isn't found, it will
% created.
set_([Name|Keys]=Ks, [_|_]=Array, Element, Acc) ->
    case having_element(Name, Array) of
        [undefined] -> % create and pretend it was always there
            [NewArray] = [Object ++ [{Name, Element}] || Object<-Array],
            case Keys of
                [] ->[NewArray|Acc];
                _ -> set_(Ks, [NewArray], Element, Acc)
            end;
        ObjectSet -> 
            case Keys of 
                [] when ?IS_OBJ(Element) ->
                    NewArray = remove(Array, ObjectSet) ++ [Element],
                    [Result] = [NewArray|Acc],
                    Result;
                [] ->
                    throw({replacing_object_with_value, Element});
                _More ->
                    NewObjSet = set_(Keys, ObjectSet, Element, []),
                    NewArray = remove(Array,ObjectSet) ++ NewObjSet,
                    [Result] = [NewArray|Acc],
                    Result
            end
    end.



to_binary_list(Keys) ->
    L = tuple_to_list(Keys),
    lists:map(fun(K) -> make_binary(K) end, L).


make_binary(K) when is_binary(K); is_number(K) -> K;
make_binary(K) when is_list(K) -> list_to_binary(K);
make_binary(K) when is_atom(K) -> K;
make_binary({select, {K, V}}) -> 
    {select, {make_binary(K), make_binary(V)}}.


add_member(Objects, M) ->
    [lists:flatten([O,M]) || 
        O <- lists:reverse(Objects)].

remove_member(Objects, {K, _V}) ->
    [proplists:delete(K, L) || L <- Objects].

replace_member(Objects, {Name, NewValue}) ->
    F = fun({N, _}) when N == Name -> {Name, NewValue};
           (Other) -> Other
        end,
    [lists:map(F, O) || O <- Objects].

                 
remove(Objects, Remove) ->
    ordsets:to_list(
      ordsets:subtract(ordsets:from_list(Objects),
                       ordsets:from_list(Remove))).

          
    
    

