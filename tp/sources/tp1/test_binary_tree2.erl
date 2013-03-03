-module(test_binary_tree2).
-export([]).
-import(binary_tree2, [count_node/1,count_leaf/1,print_leaf/1]).

-include_lib("eunit/include/eunit.hrl").

binary_tree_1_test() ->
    ?assertEqual(binary_tree2:count_node({x, {y, {a} ,{b}},{z, {c}, {d}}}), 7).

binary_tree_2_test() ->
    ?assertEqual(binary_tree2:count_node({}), 1).

binary_tree_3_test() ->
    ?assertEqual(binary_tree2:count_leaf({x, {y, {a} ,{b}},{z, {c}, {d}}}), 4).

binary_tree_4_test() ->
    ?assertEqual(binary_tree2:count_leaf({}), 0).

