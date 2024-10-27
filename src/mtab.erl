-module(mtab).
-export([format/1, format/2]).

%%% Pretty-prints a list of rows as a table.
%%%
%%% An row can be a list of values, a tuple of values,
%%% a map of values, or a proplist.
%%%
%%% By default, the header is taken from the first row.  If the rows
%%% are lists or tuples, the first row is the header. If the the rows
%%% are maps or proplists, the header is the keys of the row.

-type col() :: #{
                  align => left | right | center % left is default
                 %% NOTE: width of text, the style may add additional spaces
                , width => pos_integer() % default is dynamically calculated
                }.

-type style() :: plain | simple | pretty | simple_pretty | presto | ascii |
                 grid | simple_grid.

-type key() :: atom() | unicode:chardata().

-type custom_style() ::
        #{
          %% padding before and after the text in a cell
          spacer => iodata()
          %% format of the first line above the header
         , first_line => custom_sep()
          %% format of the header
         , header := custom_row()
          %% format of line directly below the
         , header_sep => custom_sep()
          %% format of each row
         , row := custom_row()
          %% format of line between rows
         , row_sep => custom_sep()
          %% format of the last line after the lat row
         , last_line => custom_sep()
         }.

-type custom_sep() ::
        #{
          %% leftmost character
          left => unicode:chardata()
          %% rightmost character
         , right => unicode:chardata()
          %% separator character between columns
         , col_sep => unicode:chardata()
          %% fill character, multiplied according to cell width
         , fill => unicode:chardata()
         }.

-type custom_row() ::
        #{
          %% leftmost character of a row
          left => unicode:chardata()
          %% rightmost character of a row
         , right => unicode:chardata()
          %% separator character between each cell
         , col_sep => unicode:chardata()
         }.

-spec format(Data :: [ [unicode:chardata()]
                     | {unicode:chardata()}
                     | #{key() => unicode:chardata()}
                     | [{key(), unicode:chardata()}]],
             Opts :: #{
                       header => first_row | [key()] | none
                      , header_fmt => lowercase | uppercase | titlecase
                      , cols => col() | [col()]
                      , style => style() | custom_style()
                      }
            ) -> iodata().
format(Data) ->
    format(Data, #{}).
format(Data, Opts) ->
    Style = style(maps:get(style, Opts, simple)),
    {Header, Items} = mk_header(Data, Opts),
    Rows = [mk_row(Item) || Item <- Items],
    AllRows = if Header /= undefined -> [Header | Rows];
                 true -> Rows
              end,
    Cols = mk_cols(AllRows, maps:get(cols, Opts, undefined)),
    fmt_table(Header, Rows, Cols, Style, Opts).

%%% internal

-record(sep, {
              left = ""
             , col_sep = ""
             , right = ""
             , fill = " "
             }).

-record(row, {
              left = ""
             , col_sep = " "
             , right = ""
             }).

-record(style, {
                spacer = ""
               , first_line :: #sep{} | undefined
               , header = #row{} :: #row{}
               , header_sep :: #sep{} | undefined
               , row = #row{} :: #row{}
               , row_sep :: #sep{} | undefined
               , last_line :: #sep{} | undefined
               }).

-record(cell, {
               text :: iodata()
              , width :: non_neg_integer()  % calculated width of text
              }).

mk_header([H | T] = Data, Opts) ->
    Type = classify(H),
    {DoHeader, Keys} =
        case maps:get(header, Opts, first_row) of
            first_row ->
                {true, keys(Type, H)};
            Keys0 when is_list(Keys0) ->
                {true, Keys0};
            none ->
                {false, keys(Type, H)}
        end,
    Items =
        if Type == list; Type == tuple ->
                T;
           true ->
                to_items(Type, Data, Keys)
        end,
    {mk_header_row(DoHeader, [to_chardata(Key) || Key <- Keys], Opts), Items}.

classify(H) when is_map(H) ->
    map;
classify([{_, _} | _]) ->
    proplist;
classify(H) when is_tuple(H) ->
    tuple;
classify(H) when is_list(H) ->
    list.

keys(map, H) ->
    maps:keys(H);
keys(proplist, H) ->
    [Key || {Key, _Value} <- H];
keys(tuple, H) ->
    tuple_to_list(H);
keys(list, H) ->
    H.

to_items(map, Data, Keys) ->
    [lists:map(fun(Key) -> maps:get(Key, Map, <<"">>) end, Keys) ||
        Map <- Data];
to_items(proplist, Data, Keys) ->
    to_items(map, [maps:from_list(PropList) || PropList <- Data], Keys).


to_chardata(X) when is_atom(X) -> atom_to_binary(X);
to_chardata(X) -> X.

mk_header_row(true, Line, Opts) ->
    Row = mk_row(Line),
    [fmt_cells(Cells, maps:get(header_fmt, Opts, undefined)) || Cells <- Row];
mk_header_row(false, _, _) ->
    undefined.

fmt_cells(Cells, Fmt) ->
    F = case Fmt of
            uppercase -> fun string:uppercase/1;
            lowercase -> fun string:lowercase/1;
            titlecase -> fun titlecase/1;
            undefined -> fun(X) -> X end
        end,
    [fmt_cell(Cell, F) || Cell <- Cells].

fmt_cell(Cell, F) ->
    Cell#cell{text = F(remove_underscore(Cell#cell.text))}.

titlecase(Str) ->
    string:titlecase(string:lowercase(Str)).

remove_underscore(Str) ->
    string:replace(Str, <<"_">>, <<" ">>, all).

mk_row(Items) when is_tuple(Items) ->
    mk_row(tuple_to_list(Items));
mk_row(Items) when is_list(Items) ->
    Ls = [string:split(fmt(Item), <<"\n">>) || Item <- Items],
    NLines = lists:max([length(L) || L <- Ls]),
    lists:map(
      fun(N) -> [mk_cells(L, N) || L <- Ls] end,
      lists:seq(1, NLines)).

mk_cells(L, N) ->
    case length(L) of
        Len when N =< Len ->
            Text = lists:nth(N, L),
            #cell{text = Text, width = string:length(Text)};
        _ ->
            #cell{text = "", width = 0}
    end.

fmt(Bin) when is_binary(Bin) ->
    Bin;
fmt(List) when is_list(List) ->
    List;
fmt(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
fmt(Int) when is_integer(Int) ->
    integer_to_list(Int);
fmt(X) ->
    io_lib:format("~p", [X]).


mk_cols(Rows, ColOpts) ->
    NumCols =
        if is_list(ColOpts) ->
                length(ColOpts);
           true ->
                lists:max([length(hd(Row)) || Row <- Rows])
        end,
    mk_cols(1, NumCols, Rows, ColOpts).

mk_cols(ColN, NumCols, Rows, ColOpts) when ColN =< NumCols ->
    ColOpt0 = maps:merge(default_col(), get_col_opt(ColN, ColOpts)),
    ColOpt1 =
        case maps:is_key(width, ColOpt0) of
            true ->
                ColOpt0;
            false ->
                Width = lists:max([get_col_width(ColN, Row) || Row <- Rows]),
                ColOpt0#{width => Width}
        end,
    [ColOpt1 | mk_cols(ColN+1, NumCols, Rows, ColOpts)];
mk_cols(_, _, _, _) ->
    [].

default_col() ->
    #{align => left}.

get_col_opt(_, undefined) ->
    #{};
get_col_opt(_, ColOpt) when is_map(ColOpt) ->
    ColOpt;
get_col_opt(N, ColOpts) when is_list(ColOpts) ->
    lists:nth(N, ColOpts).

get_col_width(ColN, Row) ->
    get_col_width(ColN, Row, 0).
get_col_width(ColN, [Cells | T], MaxWidth) ->
    case length(Cells) of
        Len when ColN =< Len ->
            #cell{width = Width} = lists:nth(ColN, Cells),
            get_col_width(ColN, T, max(Width, MaxWidth));
        _ ->
            get_col_width(ColN, T, MaxWidth)
    end;
get_col_width(_, [], MaxWidth) ->
    MaxWidth.

fmt_table(Header, Rows, Cols, Style, Opts) ->
    #style{spacer = Spacer} = Style,
    SpWidth = string:length(Spacer) * 2,
    [fmt_sep(Style#style.first_line, SpWidth, Cols),
     if Header /= undefined ->
             [fmt_row(Style#style.header, Spacer, Cols, Header, Opts),
              fmt_sep(Style#style.header_sep, SpWidth, Cols)];
        true ->
             []
     end,
     lists:join(fmt_sep(Style#style.row_sep, SpWidth, Cols),
                [fmt_row(Style#style.row, Spacer, Cols, Row, Opts)
                 || Row <- Rows]),
     fmt_sep(Style#style.last_line, SpWidth, Cols)].

fmt_sep(undefined, _, _) ->
    [];
fmt_sep(#sep{left = L, col_sep = CS, right = R, fill = F}, SpWidth, Cols) ->
    [L,
     lists:join(CS, [dup(F, SpWidth + maps:get(width, Col)) || Col <- Cols]),
     R,
     $\n].

fmt_row(_, _, _, undefined, _) ->
    [];
fmt_row(#row{left = L, col_sep = CS, right = R}, Spacer, Cols, Row, Opts) ->
    lists:map(
      fun(Cells) ->
              [trim(
                 [L,
                  lists:join(
                    CS, [fmt_cell(Spacer, Cell, Col) ||
                            {Cell, Col}
                                <- lists:zip(pad_cells(Cells, Cols), Cols)]
                   ),
                  R], Opts),
               $\n]
      end, Row).

trim(Str, #{no_trim := true}) ->
    Str;
trim(Str, _) ->
    string:trim(Str, trailing).

pad_cells([CellH | CellT], [_ | ColT]) ->
    [CellH | pad_cells(CellT, ColT)];
pad_cells([], [_ | ColT]) ->
    [#cell{text = "", width = 0} | pad_cells([], ColT)];
pad_cells(_, []) ->
    [].

fmt_cell(Spacer, #cell{text = Text, width = W}, #{align := A, width := ColW}) ->
    [Spacer, align(A, Text, ColW - W), Spacer].

align(left, Text, Pad) ->
    [Text, pad(Pad)];
align(right, Text, Pad) ->
    [pad(Pad), Text];
align(center, Text, Pad) ->
    Left = Pad div 2,
    [pad(Left), Text, pad(Pad - Left)].

dup(Ch, N) when N > 0 ->
    lists:duplicate(N, Ch);
dup(_, _) ->
    [].

pad(N) when N > 0 ->
    lists:duplicate(N, $\s);
pad(_) ->
    [].


style(plain) -> plain();
style(simple) -> simple();
style(pretty) -> pretty();
style(simple_pretty) -> simple_pretty();
style(presto) -> presto();
style(ascii) -> ascii();
style(grid) -> grid();
style(simple_grid) -> simple_grid();
style(M) when is_map(M) -> mk_style(M).

plain() ->
    #style{}.

simple() ->
    #style{
       header_sep = #sep{col_sep = " ", fill = "-"}
      }.

pretty() ->
    #style{
       spacer = " "
      , first_line = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      , header = #row{left = "|", col_sep = "|", right = "|"}
      , header_sep = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      , row = #row{left = "|", col_sep = "|", right = "|"}
      , last_line = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      }.

simple_pretty() ->
    #style{
       spacer = " "
      , header = #row{left = "|", col_sep = "|", right = "|"}
      , header_sep = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      , row = #row{left = "|", col_sep = "|", right = "|"}
      }.

presto() ->
    #style{
       spacer = " "
      , header = #row{col_sep = "|"}
      , header_sep = #sep{col_sep = "+", fill = "-"}
      , row = #row{col_sep = "|"}
      }.

grid() ->
    #style{
       spacer = " "
      , first_line = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      , header = #row{left = "|", col_sep = "|", right = "|"}
      , header_sep = #sep{left = "+", right = "+", col_sep = "+", fill = "="}
      , row = #row{left = "|", col_sep = "|", right = "|"}
      , row_sep = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      , last_line = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      }.

simple_grid() ->
    #style{
       spacer = " "
      , header = #row{left = "|", col_sep = "|", right = "|"}
      , header_sep = #sep{left = "+", right = "+", col_sep = "+", fill = "="}
      , row = #row{left = "|", col_sep = "|", right = "|"}
      , row_sep = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      }.

ascii() ->
    #style{
       spacer = " "
      , first_line = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      , header = #row{left = "|", col_sep = "|", right = "|"}
      , header_sep = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      , row = #row{left = "|", col_sep = "|", right = "|"}
      , row_sep = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      , last_line = #sep{left = "+", right = "+", col_sep = "+", fill = "-"}
      }.

mk_style(M) ->
    D = #style{},
    #style{spacer = maps:get(spacer, M, D#style.spacer),
           first_line = mk_sep0(maps:get(first_line, M, D#style.first_line)),
           header = mk_row0(maps:get(header, M)),
           header_sep = mk_sep0(maps:get(header_sep, M, D#style.header_sep)),
           row = mk_row0(maps:get(row, M)),
           row_sep = mk_sep0(maps:get(row_sep, M, D#style.row_sep)),
           last_line = mk_sep0(maps:get(last_line, M, D#style.last_line))
      }.

mk_sep0(undefined) ->
    undefined;
mk_sep0(M) ->
    D = #sep{},
    #sep{left = maps:get(left, M, D#sep.left),
         right = maps:get(right, M, D#sep.right),
         col_sep = maps:get(col_sep, M, D#sep.col_sep),
         fill = maps:get(fill, M, D#sep.fill)}.

mk_row0(M) ->
    D = #row{},
    #row{left = maps:get(left, M, D#row.left),
         right = maps:get(right, M, D#row.right),
         col_sep = maps:get(col_sep, M, D#row.col_sep)}.
