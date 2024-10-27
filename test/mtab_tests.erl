-module(mtab_tests).
-include_lib("eunit/include/eunit.hrl").

-export([mk_doc/0]).

%% Use this to generate sections for the README
mk_doc() ->
    TupleData = tuple_tab(),
    io:put_chars("```\n"),
    io:format("Data = ~p\n", [TupleData]),
    io:put_chars("```\n\n"),
    lists:foreach(
      fun(Opts) ->
              io:format("### ~s\n\n", [maps:get(style, Opts)]),
              io:put_chars("```\n"),
              io:format("mtab:format(Data, ~p)\n\n", [Opts]),
              io:put_chars(mtab:format(TupleData, Opts)),
              io:put_chars("```\n"),
              io:put_chars("\n\n")
      end, tuple_optsl()),

    MapData = map_tab(),
    io:put_chars("```\n"),
    io:format("Data = ~p\n", [MapData]),
    io:put_chars("```\n"),
    lists:foreach(
      fun(Opts) ->
              io:put_chars("```\n"),
              io:format("mtab:format(Data, ~p)\n\n", [Opts]),
              io:put_chars(mtab:format(MapData, Opts)),
              io:put_chars("```\n"),
              io:put_chars("\n\n")
      end, map_optsl()),

    ok.


tuple_tab() ->
    [{"Year", "Album", "Songs"},
     {1981, "Kollaps", 13},
     {1983, "Zeichnungen Des Patienten O.T.", 12},
     {1993, "Tabula rasa", 7},
     {2004, "Perpetuum Mobile", 12}].

map_tab() ->
    [#{year => 1981, album => "Kollaps", songs => 13},
     #{year => 1983, album => "Zeichnungen Des Patienten O.T.", songs => 12},
     #{year => 1993, album => "Tabula rasa", songs => 7},
     #{year => 2004, album => "Perpetuum Mobile", songs => 12}].


tuple_optsl() ->
    [#{style => plain},
     #{style => simple},
     #{style => pretty},
     #{style => simple_pretty},
     #{style => presto},
     #{style => ascii},
     #{style => grid},
     #{style => simple_grid}].

map_optsl() ->
    [#{header_fmt => titlecase,
       style => presto},
     #{header => [year, album, songs],
       header_fmt => titlecase,
       style => presto,
       cols => [#{align => left, width => 6},
                #{align => center},
                #{align => right}]}].


tab1() ->
    [["Item\nname", "Qty"],
     ["spam", "42"],
     ["white\neggs", "451"],
     ["fatty bacon", "0"]].

tab2() ->
    [#{foo => 42, bar => "hej", <<"baz">> => 99},
     #{bar => "hopp"},
     #{foo => 44, <<"baz">> => 100}].

tab3() ->
    [["hej", "hopp", "foo"],
     [10, 20, 30],
     [10, 20],
     [30]].

tab4() ->
    [[{"foo", 42}, {bar, "hej"}, {baz, 99}],
     [{bar, "hopp"}],
     [{"foo", 44}, {baz, 100}]].

tab5() ->
    [{"Item\nname", "Qty"},
     {"spam", "42"},
     {"white\neggs", "451"},
     {"fatty bacon", "0"}].


fmt(Data, Opts) ->
    lists:flatten(io_lib:format("~s", [mtab:format(Data, Opts)])).

simple_test() ->
    ?assertEqual(
"+-------------+-----+
|    Item     | Qty |
|    Name     |     |
+-------------+-----+
|    spam     | 42  |
+-------------+-----+
|    white    | 451 |
|    eggs     |     |
+-------------+-----+
| fatty bacon |  0  |
+-------------+-----+
",
        fmt(tab1(),
            #{header => first_row,
              header_fmt => titlecase,
              style => ascii,
              cols => #{align => center}})).

simple_tuple_test() ->
    ?assertEqual(
"+-------------+-------+
| Item        |   Qty |
| Name        |       |
+-------------+-------+
| spam        |    42 |
+-------------+-------+
| white       |   451 |
| eggs        |       |
+-------------+-------+
| fatty bacon |     0 |
+-------------+-------+
",
        fmt(tab5(),
            #{header => first_row,
              header_fmt => titlecase,
              style => ascii,
              cols => [#{align => left}, #{width => 5, align => right}]})).

sparse_list_test() ->
    ?assertEqual(
"Hej Hopp Foo
--- ---- ---
10  20   30
10  20
30
",
        fmt(tab3(),
            #{header => first_row,
              header_fmt => titlecase})).

sparse_list_no_trim_test() ->
    ?assertEqual(
"Hej Hopp Foo
--- ---- ---
10  20   30\s
10  20\s\s\s\s\s\s
30\s\s\s\s\s\s\s\s\s\s
",
        fmt(tab3(),
            #{header => first_row,
              header_fmt => titlecase,
              no_trim => true})).

sparse_map_test() ->
    ?assertEqual(
" Bar  | Baz | Foo
------+-----+-----
 hej  | 99  | 42
 hopp |     |
      | 100 | 44
",
        fmt(tab2(),
            #{header_fmt => titlecase,
              header => [bar, <<"baz">>, foo],
              style => presto})).

sparse_proplist_test() ->
    ?assertEqual(
" Bar  | Baz | Foo
------+-----+-----
 hej  | 99  | 42
 hopp |     |
      | 100 | 44
",
        fmt(tab4(),
            #{header_fmt => titlecase,
              header => [bar, baz, "foo"],
              style => presto})).


no_header_map_test() ->
    ?assertEqual(
" hej  | 42 | 99
 hopp |    |
      | 44 | 100
",
        fmt(tab2(),
            #{header_fmt => titlecase,
              header => none,
              style => presto})).
