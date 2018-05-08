
-module(hw2_test).

-include_lib("eunit/include/eunit.hrl").
-import(hw2, [fibm/2,seg_longest/2, seg_split/2,fibm_segs/2,
 fibm_leaf/2,seg_shift/2,fibm_combine_help/3,
 longest_fibm_seq_par/3,longest_fibm_seq_par/3,longest_fibm_seq/2,
 longest_fibm_seq/2
 ]).

-export([floor_log10/1,run_all_tests/0]).


floor_log10(N) when is_integer(N), N >= 10 ->
  1 + floor_log10(N div 10);
floor_log10(_) -> 0.

fib_test() ->
  ?assertEqual(0, fibm(0,100)),
  ?assertEqual(55, fibm(10,100)),
  ?assertEqual(89, fibm(11,100)),
  ?assertEqual(44, fibm(12,100)),
  ?assertEqual(96, fibm(42,100)).

seg_longest_test()->
  ?assertEqual({[9,48], {13,5}, [5,62]},seg_longest({[0,1], {1,2}, [0,1]},{[9,48], {13,5}, [5,62]}) ),
  ?assertEqual({[9,48], {13,5}, [5,62]},seg_longest({[9,48], {13,5}, [5,62]},{[0,1], {1,2}, [0,1]}) ),
  ?assertEqual({[0,1], {1,2}, [0,1]},seg_longest({[0,1], {1,2}, [0,1]}, {[62,0], {17,2}, [62,0]} ) ),
  ?assertEqual({[0,1], {1,2}, [0,1]},seg_longest( {[62,0], {17,2}, [62,0]},{[0,1], {1,2}, [0,1]} ) ),
  ?assertEqual({[10,10], {6,7}, [80,30]},seg_longest( {[2,4],{3,4},[6,10]}, {[10,10], {6,7}, [80,30]}) ) ,
  ?assertEqual({[10,10], {6,7}, [80,30]},seg_longest( {[10,10], {6,7}, [80,30]},{[2,4],{3,4},[6,10]} ) ) 
  .


longest_fibm_seq_test()->
  ?assertEqual({1,3},longest_fibm_seq([0,1,1],100)),
  ?assertEqual({5,6},longest_fibm_seq([0,1,2,3,5,10,15,25,40,65,100,200],100)),
  ?assertEqual({3,4},longest_fibm_seq([0,1,2,4,6,10,10,20,30,40,70,110],100)),
  ?assertEqual({1,2},longest_fibm_seq([5,4,3,2,1,0],100)),
  ?assertEqual({1,1},longest_fibm_seq([5],100)),
   ?assertEqual({1,0},longest_fibm_seq([],100)),
   ?assertError(_, longest_fibm_seq( [ 4, banana],100 )).


seq_split_test()->
   ?assertEqual({{[], {1,0},[]},[]}, seg_split([], 100) ),
   ?assertEqual({{[1,1], {1,4}, [2,3]}, [3,4,5]}, seg_split([1,1,2,3,4,5], 100) ),
    ?assertEqual({{[3,4], {1,6}, [18,29]}, []}, seg_split([3,4,7,11,18,29], 100) ),
    ?assertEqual({{[0,1],{1,5},[2,3]} , [3,6,9,15,21,36,57,100]}, seg_split([0,1,1,2,3,6,9,15,21,36,57,100], 100) )    

   .

 fibm_seqs_test()->
   ?assertEqual([ {[0,1],{1,2},[0,1]}, {[1,2],{2,2},[1,2]}, {[2,4],{3,4},[6,10]},
{[10,10],{6,7},[80,30]}, {[30,9],{12,2},[30,9]}, {[9,48],{13,5},[5,62]},
{[62,0],{17,2},[62,0]}
], fibm_segs([0,1,2,4,6,10,10,20,30,50,80,30,9,48,57,5,62,0], 100) ).


fibm_leaf_test()->
    ?assertEqual( [ {[0,1], {1,2}, [0,1]}, %% first
                  {[10,10], {6,7}, [80,30]} ,%% longest
                  {[62,0], {17,2}, [62,0]} ],%% last
     fibm_leaf([0,1,2,4,6,10,10,20,30,50,80,30,9,48,57,5,62,0], 100) ).

seg_shift_test()->
      ?assertEqual({[3,8], {1734,82}, [19,11]}, seg_shift(1234, {[3,8], {500,82}, [19,11]})),
      ?assertEqual([{[0,1], {1001,2}, [0,1]}, {[10,10], {1006,7}, [80,30]}, {[62,0], {1017,2}, [62,0]}]
        , seg_shift(1000, [{[0,1], { 1,2}, [0,1]}, {[10,10], {6,7}, [80,30]}, {[62,0], {17,2}, [62,0]}])),
      ?assertEqual([{[0,1], {600,2}, [0,1]}, {[10,10], { 605,7}, [80,30]}]
        , seg_shift({[3,8], {500,100}, [19,11]}, [{[0,1], {1,2}, [0,1]}, {[10,10], {6,7}, [80,30]}])).


fibm_combine_help_test()-> 
     ?assertEqual({[1,3],{1,21},[64,29]}, 
         fibm_combine_help({[1,3],{1,10},[65,1]},{[66,67],{11,11},[64,29]}, 100)) ,
     ?assertEqual({[1,3],{1,11},[1,66]}, 
      fibm_combine_help({[1,3],{1,10},[65,1]},{[66,99],{11,10},[37,52]}, 100)),
      ?assertEqual({[1,66],{10,11},[64,29]}, 
      fibm_combine_help({[45,84],{1,10},[49,1]},{[66,67],{11,10},[64,29]}, 100)),
      ?assertEqual( {[1,66],{10,2},[1,66]}, 
      fibm_combine_help({[45,84],{1,10},[49,1]},{[66,99],{11,10},[37,52]}, 100)).


fibm_test(N_Procs,M,L) ->

  code:add_path("/home/c/cs418/public_html/resources/erl"), 
  W = wtree:create(N_Procs),
  wtree:update(W, fibm_par_data, misc:cut(L, W)),
  
  
  ParValue = longest_fibm_seq_par(W, fibm_par_data, M),
  wtree:reap(W),
  
  
 ParValue.


fibm_par_test()->
  ?assertEqual({1,3},fibm_test(4,100, [0,1,1])),
  ?assertEqual({5,6},fibm_test(4,100,[0,1,2,3,5,10,15,25,40,65,100,200])),
  ?assertEqual({3,4},fibm_test(4,100,[0,1,2,4,6,10,10,20,30,40,70,110])),
  ?assertEqual({1,2},fibm_test(4,100, [5,4,3,2,1,0])),
  ?assertEqual({1,1},fibm_test(4,100, [5])),
   ?assertEqual({1,0},fibm_test(4,100, [])),
   ?assertThrow(_, fibm_test(4,100, [ 4, banana]) )  . 




run_all_tests()->
fib_test() ,
seg_longest_test(),
seq_split_test(),
fibm_leaf_test(),
seg_shift_test(),
fibm_combine_help_test(),
fibm_par_test(),

longest_fibm_seq_test()
.
