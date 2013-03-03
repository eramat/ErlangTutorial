-module(test_insert).
-export([]).
-import(insert, [insert_object_in_list/3]).

-include_lib("eunit/include/eunit.hrl").

-include("rectangle.hrl").

inc_1_test() ->
    Policy = fun(X, Y) -> if X > Y -> true; true -> false end end,
    R = #rectangle{width=10, height=20, color=black},
    ?assertEqual(insert:insert_object_in_list(R, [], Policy),
		 [#rectangle{width=10, height=20, color=black}]).

inc_2_test() ->
    Policy = fun(X, Y) -> if X > Y -> true; true -> false end end,
    R = 1,
    ?assertEqual(insert:insert_object_in_list(R, [], Policy),
		 {error, no_record}).

inc_3_test() ->
    Policy = 1,
    R = #rectangle{width=10, height=20, color=black},
    ?assertEqual(insert:insert_object_in_list(R, [], Policy),
		 {error, no_record}).

inc_4_test() ->
    Policy = fun(X, Y) -> if X > Y -> true; true -> false end end,
    R = #rectangle{width=10, height=20, color=black},
    ?assertEqual(insert:insert_object_in_list(R, 0, Policy),
		 {error, no_record}).

inc_5_test() ->
    Policy = fun(X, Y) -> if X > Y -> true; true -> false end end,
    R = #rectangle{width=10, height=20, color=black},
    ?assertEqual(insert:insert_object_in_list(
		   R,
		   [
		    #rectangle{width=5, height=10, color=black},
		    #rectangle{width=10, height=10, color=black},
		    #rectangle{width=20, height=30, color=black}
		   ],
		   Policy),
		 [
		  #rectangle{width=5, height=10, color=black},
		  #rectangle{width=10, height=10, color=black},
		  #rectangle{width=10, height=20, color=black},
		  #rectangle{width=20, height=30, color=black}
		 ]).

