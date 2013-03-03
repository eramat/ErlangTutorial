-module(state).
-author('eric.ramat@gmail.com').

-export([create/0, finish/1, is_reverse/1, push_tile/5]).

% initial state
% [[red, yellow, ...],...[yellow, red, ..]]

create() ->
    init2(lists:seq(1, 6), lists:seq(1, 6), red, [], []).

init2([_], [], _, L, L2) ->
    L2 ++ [L];
init2([Row|T1], [_|T2], red, L, L2) ->
    init2([Row|T1], T2, yellow, L ++ [red], L2);
init2([Row|T1], [_|T2], yellow, L, L2) ->
    init2([Row|T1], T2, red, L ++ [yellow], L2);
init2([_|T1], [], red, L, L2) ->
    init2(T1, lists:seq(1, 6), yellow, [], L2 ++ [L]);
init2([_|T1], [], yellow, L, L2) ->
    init2(T1, lists:seq(1, 6), red, [], L2 ++ [L]).

transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | _] <- Xss]]
     | transpose([Xs | [T || [_ | T] <- Xss]])];
transpose([[] | Xss]) -> transpose(Xss);
transpose([]) -> [].

reverse(yellow) ->
    yellow_reverse;
reverse(red) ->
    red_reverse.

plain(yellow) ->
    yellow;
plain(red) ->
    red;
plain(yellow_reverse) ->
    yellow;
plain(red_reverse) ->
    red.

is_reverse(yellow) ->
    false;
is_reverse(red) ->
    false;
is_reverse(yellow_reverse) ->
    true;
is_reverse(red_reverse) ->
    true.

select_row([Row|_], 1) ->
    Row;
select_row([_|T], Row) ->
    select_row(T, Row - 1).

remove_first([H|T]) ->
    {H, T}.

remove_last2([_]) ->
    [];
remove_last2([H|T]) ->
    [H] ++ remove_last2(T).

remove_last(L) ->
    {lists:last(L), remove_last2(L)}.

replace_row([_|T], 1, NewRow) ->
    [NewRow] ++ T;
replace_row([H|T], Row, NewRow) ->
    [H] ++ replace_row(T, Row - 1, NewRow).

push_tile(top, State, Column, Color) ->
    StateTr = transpose(State),
    {NewColor, SubRow} = remove_last(select_row(StateTr, Column)),
    NewRow = [reverse(Color) | SubRow],
    {transpose(replace_row(StateTr, Column, NewRow)), plain(NewColor)};
push_tile(bottom, State, Column, Color) ->
    StateTr = transpose(State),
    {NewColor, SubRow} = remove_first(select_row(StateTr, Column)),
    NewRow = SubRow ++ [reverse(Color)],
    {transpose(replace_row(StateTr, Column, NewRow)), plain(NewColor)};
push_tile(left, State, Row, Color) ->
    {NewColor, SubRow} = remove_last(select_row(State, Row)),
    NewRow = [reverse(Color) | SubRow],
    NewState = replace_row(State, Row, NewRow),
    {NewState, plain(NewColor)};
push_tile(right, State, Row, Color) ->
    {NewColor, SubRow} = remove_first(select_row(State, Row)),
    NewRow = SubRow ++ [reverse(Color)],
    NewState = replace_row(State, Row, NewRow),
    {NewState, plain(NewColor)}.

%check_tile(TileColor, PlayerColor)
check_tile(red_reverse, red) ->
    true;
check_tile(yellow_reverse, yellow) ->
    true;
check_tile(red, _) ->
    true;
check_tile(yellow, _) ->
    true;
check_tile(_, _) ->
    false.

first([H|_]) ->
    H.

valid_push(top, State, Column, PlayerColor) ->
    check_tile(lists:last(select_row(transpose(State), Column)), PlayerColor);
valid_push(bottom, State, Column, PlayerColor) ->
    check_tile(first(select_row(transpose(State), Column)), PlayerColor);
valid_push(left, State, Row, PlayerColor) ->
    check_tile(lists:last(select_row(State, Row)), PlayerColor);
valid_push(right, State, Row, PlayerColor) ->
    check_tile(first(select_row(State, Row)), PlayerColor).

push_tile(Orientation, State, Index, Color, PlayerColor) ->
    case valid_push(Orientation, State, Index, PlayerColor) of
	true -> push_tile(Orientation, State, Index, Color);
	_ -> {[], PlayerColor}
    end.

count2([], [RedTiles, YellowTiles]) ->
    [RedTiles, YellowTiles];
count2([red|T], [RedTiles, YellowTiles]) ->
    [NewRedTiles, NewYellowTiles] = count2(T, [RedTiles, YellowTiles]),
    [NewRedTiles + 1, NewYellowTiles];
count2([yellow|T], [RedTiles, YellowTiles]) ->
    [NewRedTiles, NewYellowTiles] = count2(T, [RedTiles, YellowTiles]),
    [NewRedTiles, NewYellowTiles + 1];
count2([_|T], [RedTiles, YellowTiles]) ->
    count2(T, [RedTiles, YellowTiles]).

count([], [RedTiles, YellowTiles]) ->
    [RedTiles, YellowTiles];
count([H|T], [RedTiles, YellowTiles]) ->
    [NewRedTiles, NewYellowTiles] = count2(H, [RedTiles, YellowTiles]),
    count(T, [NewRedTiles, NewYellowTiles]).

is_finish([0, _]) ->
    red;
is_finish([_, 0]) ->
    yellow;
is_finish([_, _]) ->
    false.

finish(State) ->
    [RedTiles, YellowTiles] = count(State, [0, 0]),
    is_finish([RedTiles, YellowTiles]).
