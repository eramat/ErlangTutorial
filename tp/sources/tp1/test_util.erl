-module(test_util).
-export([]).
-import(util, [inc_element_at_integer_list/1, concat/1, flatten/1]).

-include_lib("eunit/include/eunit.hrl").

inc_1_test() ->
    ?assertEqual(util:inc_element_at_integer_list([]), []).

inc_2_test() ->
    ?assertEqual(util:inc_element_at_integer_list([1,2]), [2,3]).

inc_3_test() ->
    ?assertEqual(util:inc_element_at_integer_list([1,a,[x,y,"z"]]), [2]).

inc_4_test() ->
    ?assertEqual(util:inc_element_at_integer_list({1,2}), {error, no_list}).

concat_1_test() ->
    ?assertEqual(util:concat([]), []).

concat_2_test() ->
    ?assertEqual(util:concat([[1,2]]), [1,2]).

concat_3_test() ->
    ?assertEqual(util:concat([[1,2,3],[],[4,five]]), [1,2,3,4,five]).

flatten_1_test() ->
    ?assertEqual(util:flatten([]), []).

flatten_2_test() ->
    ?assertEqual(util:flatten([1,2]), [1,2]).

flatten_3_test() ->
    ?assertEqual(util:flatten([[1,[2,[3],[]]],[[[4]]],[5,6]]), [1,2,3,4,5,6]).
