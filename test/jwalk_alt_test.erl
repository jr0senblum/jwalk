-module(jwalk_alt_test).

-include_lib("eunit/include/eunit.hrl").

jwalk_alt_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("../test/widget.alt_terms"),
         {ok, [Glossary]} = file:consult("../test/glossary.alt_terms"),
         {ok, [Menu]} = file:consult("../test/menu.alt_terms"),
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
                   Menu1 = jwalk:set(Path, Menu, Val),
                   ?assertMatch([<<"helptext">>], jwalk:get(Path, Menu1))
           end},


          {"jwalk:set new value in a object at a complex path",
           fun() ->
                   Path = {"menu", "popup", "menuitem",
                           {select, {"value", "New"}}},
                   Path2 = {"menu", "popup", "menuitem",
                            {select, {"value", "New"}}, "onclick"},
                   Val = {<<"onclick">>, <<"CreateDifferentNewDoct()">>},
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
                   Val = {<<"more">>, <<"content">>},
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
                   Val = {<<"more">>, <<"content">>},
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
           end}
         ]
 end
}.

