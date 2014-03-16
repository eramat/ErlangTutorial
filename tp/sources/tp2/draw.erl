-module(draw).
-author('eric.ramat@gmail.com').

-export([draw/4, draw_board/3, invalid_buttons/1, show_player/2]).

-import(state, [is_reverse/1]).

-include("size.hrl").

state_to_color(yellow) ->
    yellow;
state_to_color(red) ->
    red;
state_to_color(yellow_reverse) ->
    yellow;
state_to_color(red_reverse) ->
    red.

invalid_buttons([Button|T]) ->
    gs:config(Button, [{enable, false}]),
    invalid_buttons(T);
invalid_buttons([]) ->
    true.

valid_buttons([Button|T]) ->
    gs:config(Button, [{enable, true}]),
    invalid_buttons(T);
valid_buttons([]) ->
    true.

draw_button(Win, X, Y, Text, Data) ->
    gs:create(button, Win, [{label, {text, Text}},
			    {x, X},
			    {y, Y},
			    {width, 30},
			    {data, Data}]).

draw_top_buttons(Win, [Column|T], Buttons) ->
    draw_top_buttons(Win, T,
		     [draw_button(Win, Column * ?TileWidth - 15, 5,
				  "V", {top, Column}) | Buttons]);
draw_top_buttons(_, [], Buttons) ->
    Buttons.

draw_bottom_buttons(Win, [Column|T], Buttons) ->
    draw_bottom_buttons(Win, T,
			[draw_button(Win, Column * ?TileWidth - 15,
				     6 * ?TileHeight + ?TileHeight / 2 + 5,
				  "/\\", {bottom, Column}) | Buttons]);
draw_bottom_buttons(_, [], Buttons) ->
    Buttons.

draw_left_buttons(Win, [Row|T], Buttons) ->
    draw_left_buttons(Win, T,
		      [draw_button(Win, 5, Row * ?TileHeight - 10,
				  ">", {left, Row}) | Buttons]);
draw_left_buttons(_, [], Buttons) ->
    Buttons.

draw_right_buttons(Win, [Row|T], Buttons) ->
    draw_right_buttons(Win, T,
		       [draw_button(Win,
				    6 * ?TileWidth + ?TileWidth / 2 + 5,
				    Row * ?TileHeight - 10,
				    "<", {right, Row}) | Buttons]);
draw_right_buttons(_, [], Buttons) ->
    Buttons.

draw_buttons(Win) ->
    draw_top_buttons(Win, lists:seq(1, 6), []) ++
    draw_bottom_buttons(Win, lists:seq(1, 6), []) ++
    draw_left_buttons(Win, lists:seq(1, 6), []) ++
    draw_right_buttons(Win, lists:seq(1, 6), []).

draw_free_tiles(Canvas, [RedFreeTileColor, YellowFreeTileColor]) ->
    gs:create(rectangle, Canvas, [{coords, [{5, 5},
					    {?TileWidth / 2 - 5,
					     ?TileHeight / 2 - 5}]},
				  {fill, RedFreeTileColor}]),
    gs:create(rectangle, Canvas, [{coords, [{?BoardWidth - ?TileWidth / 2 + 5,
					     5},
					    {?BoardWidth - 5,
					     ?TileHeight / 2 - 5}]},
				  {fill, YellowFreeTileColor}]).

draw_tile(Canvas, Row, Column, State) ->
    draw_tile2(Canvas, Row, Column, state_to_color(State),
	       state:is_reverse(State)).

draw_tile2(Canvas, Row, Column, Color, Reverse) ->
    gs:create(rectangle, Canvas, [{coords, [{(Column - 1) * ?TileWidth +
						 ?TileWidth / 2,
					     (Row - 1) * ?TileHeight +
						 ?TileHeight / 2},
					    {(Column - 1) * ?TileWidth +
						 3 * ?TileWidth / 2,
					     (Row - 1) * ?TileHeight +
						 3 * ?TileHeight / 2}]},
				  {fill, Color}]),
    draw_reverse(Canvas, Row, Column, Reverse).

draw_reverse(_, _, _, false) ->
    true;
draw_reverse(Canvas, Row, Column, true) ->
    gs:create(rectangle, Canvas, [{coords, [{(Column - 1) * ?TileWidth +
						 ?TileWidth - 10,
					     (Row - 1) * ?TileHeight +
						 ?TileHeight - 10},
					    {(Column - 1) * ?TileWidth +
						 ?TileWidth + 10,
					     (Row - 1) * ?TileHeight +
						 ?TileHeight + 10}]},
				 {fill, black}]).

draw_row(_, [], _, _) ->
    true;
draw_row(Canvas, [Case|T], Row, Column) ->
    draw_tile(Canvas, Row, Column, Case),
    draw_row(Canvas, T, Row, Column + 1).

draw_grid(_, [], _) ->
    true;
draw_grid(Canvas, [CaseList|T], Row) ->
    draw_row(Canvas, CaseList, Row, 1),
    draw_grid(Canvas, T, Row + 1).

draw_board(Canvas, State, [RedFreeTileColor, YellowFreeTileColor]) ->
    draw_free_tiles(Canvas, [RedFreeTileColor, YellowFreeTileColor]),
    draw_grid(Canvas, State, 1).

draw(Win, Canvas, State,  [RedFreeTileColor, YellowFreeTileColor]) ->
    Buttons = draw_buttons(Win),
    draw_board(Canvas, State, [RedFreeTileColor, YellowFreeTileColor]),
    Buttons.

show_player(Canvas, red) ->
    gs:create(rectangle, Canvas, [{coords, [{?TileWidth / 4 - 10,
					     ?TileHeight / 4 - 10},
					    {?TileWidth / 4 + 10,
					     ?TileHeight / 4 + 10}]},
				  {fill, black}]);
show_player(Canvas, yellow) ->
    gs:create(rectangle, Canvas, [{coords, [{?BoardWidth - ?TileWidth / 4 - 10,
					     ?TileHeight / 4 - 10},
					    {?BoardWidth - ?TileWidth / 4 + 10,
					     ?TileHeight / 4 + 10}]},
				  {fill, black}]).

