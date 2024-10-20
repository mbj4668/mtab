-module(mtab_tests).
-include_lib("eunit/include/eunit.hrl").

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
        fmt(tab5(),
            #{header => first_row,
              header_fmt => titlecase,
              style => ascii,
              cols => #{align => center}})).

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


%% plain

%% item      qty
%% spam       42
%% eggs      451
%% bacon       0

%% simple

%% item      qty
%% ------  -----
%% spam       42
%% eggs      451
%% bacon       0


%% pretty

%% +--------+-------+
%% | item   | qty   |
%% +--------+-------+
%% | spam   | 42    |
%% | eggs   | 451   |
%% | bacon  | 0     |
%% +--------+-------+

%% simple_pretty

%% | item   | qty   |
%% +--------+-------+
%% | spam   | 42    |
%% | eggs   | 451   |
%% | bacon  | 0     |

%% presto

%%  item   | qty
%%  -------+-----
%%  spam   | 42
%%  eggs   | 45
%%  bacon  | 0


%% grid

%% +--------+-------+
%% | item   |   qty |
%% +========+=======+
%% | spam   |    42 |
%% +--------+-------+
%% | eggs   |   451 |
%% +--------+-------+
%% | bacon  |     0 |
%% +--------+-------+


%% simple_grid

%% | item   |   qty |
%% +========+=======+
%% | spam   |    42 |
%% +--------+-------+
%% | eggs   |   451 |
%% +--------+-------+
%% | bacon  |     0 |


%% ascii

%% +--------+-------+
%% | item   |   qty |
%% +--------+-------+
%% | spam   |    42 |
%% +--------+-------+
%% | eggs   |   451 |
%% +--------+-------+
%% | bacon  |     0 |
%% +--------+-------+
