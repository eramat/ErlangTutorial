-module(gui).
-author('eric.ramat@gmail.com').

-export([start/0, init/0]).

-import(state, [create/0, is_reverse/1, push_tile/5]).
-import(draw, [draw/4, draw_board/3]).

-include("size.hrl").

start() ->
    register(invers_gui, spawn(gui, init, [])).

init() ->
    State = state:create(),
    Gs = gs:start(),
    Win = gs:create(window, Gs, [{width, ?BoardWidth},
				 {height, ?BoardHeight},
				 {keypress, false},
				 {keyrelease, true}]),
    Canvas = gs:create(canvas, Win, [{width, ?BoardWidth},
				     {height, ?BoardHeight},
				     {bg, green}]),
    Buttons = draw:draw(Win, Canvas, State, [red, yellow]),
    draw:show_player(Canvas, red),
    gs:config(Win,{title,"Invers"}),
    gs:config(Win, {map, true}),
    loop(Canvas, Buttons, {State, [red, yellow], red}).

next_player(red) ->
    yellow;
next_player(yellow) ->
    red.

%free_tiles(CurrentPlayer, NewColor, [RedFreeTileColor, YellowFreeTileColor])
free_tiles(red, NewColor, [_, C]) ->
    [NewColor, C];
free_tiles(yellow, NewColor, [C, _]) ->
    [C, NewColor].

play(red, Orientation, State, Index, [RedFreeTileColor, _]) ->
    state:push_tile(Orientation, State, Index, RedFreeTileColor, red);
play(yellow, Orientation, State, Index, [_, YellowFreeTileColor]) ->
    state:push_tile(Orientation, State, Index, YellowFreeTileColor, yellow).

loop3(false, Canvas, Buttons, {NewState,
			       [RedFreeTileColor, YellowFreeTileColor],
			       NextPlayerColor}) ->
    loop(Canvas, Buttons, {NewState, [RedFreeTileColor, YellowFreeTileColor],
			   NextPlayerColor});
loop3(Color, _, _, _) ->
    io:format("~w player wins~n",[Color]),
    exit(normal).

loop2(_, _, [], _, _) ->
    exit(wrong_move);
loop2(Canvas, Buttons, NewState, [RedFreeTileColor, YellowFreeTileColor],
      CurrentPlayerColor) ->
    draw:draw_board(Canvas, NewState, [RedFreeTileColor, YellowFreeTileColor]),
    draw:show_player(Canvas, next_player(CurrentPlayerColor)),
    loop3(state:finish(NewState), Canvas, Buttons,
	  {NewState, [RedFreeTileColor, YellowFreeTileColor],
	   next_player(CurrentPlayerColor)}).

loop(Canvas, Buttons, {State, [RedFreeTileColor, YellowFreeTileColor],
		       CurrentPlayerColor}) ->
    receive
        {gs, _, click, {Orientation, Index}, _} ->
	    {NewState, NewColor} = play(CurrentPlayerColor, Orientation, State,
					Index, [RedFreeTileColor,
						YellowFreeTileColor]),
            loop2(Canvas, Buttons, NewState,
		  free_tiles(CurrentPlayerColor, NewColor,
			     [RedFreeTileColor, YellowFreeTileColor]),
		  CurrentPlayerColor);
	{gs, _, keyrelease, _, [q|_]} ->
            exit(normal)
    end.
