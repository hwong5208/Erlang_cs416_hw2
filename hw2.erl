-module(hw2).

-export([fibm/2, is_integer_list/1, is_fibm_seq/2, is_fibm_seq_help/2]).
-export([longest_fibm_seq/2, longest_fibm_seq_examples/0, longest_fibm_seq_print_time/1,   longest_fibm_seq_time/0]).
-export([seg_startPos/1, seg_length/1, seg_lastPos/1, seg_prefix/1,
         seg_suffix/1, seg_longest/2]).
-export([longest_fibm_seq_par/3, fibm_leaf/2, fibm_combine/3]).
-export([seg_split/2, seg_shift/2, fibm_segs/2, fibm_combine_help/3]).
-export([fibm_segs_example/0]).
-export([rand_fibbish_list/3, fibm_par_test/4, fibm_par_test/0,fibm_par_test_a/0,fibm_par_test_b/0]).
-export([you_need_to_write_this/2, you_need_to_write_this/3]).

% fibm(N, M) -> fib(N) rem N.
%   fib(N) can be huge, and it takes a long time to compute it explicitly.
%   instead, we move the rem N operation inside the fibm computation and
%   avoid having any large numbers.
%     Recall from HW1 that a brute-force implemenation of fib(N) takes
%   exponential time.  The same is true for fibm.  We can use the same
%   idea as from HW1 to get an implementation for fibm(N, M) whose
%   runtime is linear in N (assuming that M is small, i.e. 0 < M < 2^32).
%fibm(0, _M) -> 0;
%fibm(N,  M) when is_integer(N) , N >= 1, is_integer(M), M > 0 ->
%  you_need_to_write_this(fibm, "recursive case", [N,M]).

fibm2(1,_M)->{0,1};
fibm2(N,M)-> 
  {X,Y} = fibm2(N-1,M),{Y rem M , (X+Y) rem M}.

fibm(0, M) when is_integer(M),M>0 -> 0;
fibm(N,  M) when is_integer(N) , N >= 1, is_integer(M), M > 0 ->
{_,Z} = fibm2(N,M),
Z.



% Several of our functions below are simpler if we can check to make
%   sure that a list is a list of integers.
is_integer_list(L) when is_list(L) ->
  lists:all(fun(X) -> is_integer(X) end, L);
is_integer_list(_) -> false.

% is_fibm_seq(List, M) -> true if List is a M-Fibonacci sequence
%                      -> false otherwise.
is_fibm_seq(List, M) ->
  case is_integer_list(List) of
    true -> is_fibm_seq_help(List, M);
    false -> false
  end.

%is_fibm_seq_help([], _) ->
%  you_need_to_write_this(is_fibm_seq_help, "empty list");
%is_fibm_seq_help([_A], _) ->
%  you_need_to_write_this(is_fibm_seq_help, "singleton list");
%is_fibm_seq_help([_A, _B], _) ->
%  you_need_to_write_this(is_fibm_seq_help, "list of two elements");
%is_fibm_seq_help([A, B, C | Tl], M) ->[A, B, C, Tl, M]).


is_fibm_seq_help([], _) ->
  true;
is_fibm_seq_help([_A], _) ->
  true;
is_fibm_seq_help([_A, _B], _) ->
  true;
is_fibm_seq_help([A, B, C | Tl], M) ->
  if (A +B) rem M =/= C -> false;
    true-> is_fibm_seq_help([B,C|Tl],M) end.

%longest_fibm_seq(List, M) ->
%  you_need_to_write_this(longest_fibm_seq, "body of function", [List, M]).



longest_fibm_seq(List,M) when M> 0->
   case is_integer_list(List) of
    true->longest_fibm_seq_helper(List,M);
    false-> error(" List has element is not an integer.") end.


longest_fibm_seq_helper([],_) -> {1,0};
longest_fibm_seq_helper([_X],_) -> {1,1};
longest_fibm_seq_helper([_X,_Y],_) -> {1,2};
longest_fibm_seq_helper([X,Y,Z|Tl],M) ->longest_fibm_seq_helper2([X,Y,Z|Tl],M,{1,2},{1,2}).


longest_fibm_seq_helper2([_X,_Y],_M,{_CurStart,_CurLen},{LongestStart,LongestLen} )->{LongestStart,LongestLen};

longest_fibm_seq_helper2([X,Y,Z|Tl],M,{CurStart,CurLen},{LongestStart,LongestLen})->

if (X+Y )rem M =:=Z ->
  if CurLen+1 >LongestLen->
    longest_fibm_seq_helper2([Y,Z|Tl],M,{CurStart,CurLen+1},{CurStart,CurLen+1});
     true->longest_fibm_seq_helper2([Y,Z|Tl],M,{CurStart,CurLen+1},{LongestStart,LongestLen})end;
  true-> longest_fibm_seq_helper2([Y,Z|Tl],M,{CurStart + CurLen-1,2},{LongestStart,LongestLen})end.



longest_fibm_seq_examples() ->
  Examples = % the examples given in the question statement.
    [ [0,1,1], % -> {1,3}.
      [0,1,2,3,4,5,10,15,25,40,100,200], % -> {6,5}.
      [0,1,2,4,6,10,10,20,30,0], % -> {3,4}.
      [3,-1,2,1,3,4,7,11,22,33,55,88,17,105,0], % -> {1,8}.
      [5,4,3,2,1,0], % -> {1,2}.
      [5],  % -> {1,1}.
      [],   % -> {1,0}.
      [42,bananas] % should raise an error
    ],
  Try = fun(List) ->
    try
      longest_fibm_seq(List, 1000)
    catch _:_ -> error
    end
  end,
  [     io:format("longest_fibm_seq(~w) -> ~w~n", [L, Try(L)])
    ||  L <- Examples
  ].


  longest_fibm_seq_print_time(N)-> 
  code:add_path("/home/c/cs418/public_html/resources/erl"), M = 1000000000,
    List = [rand:uniform(10) || _ <- lists:seq(1, N)],
    [io:format("N = ~w T= ~w ~n",[ length(List),element(2, hd( time_it:t(fun()->longest_fibm_seq(List,M) end)))
      ])].

  longest_fibm_seq_time()-> [longest_fibm_seq_print_time(N) || N <-[10,100,1000,10000,100000,1000000,10000000]].



% For the parallel version, we'll divide the sequen,e into segments
%   where each segment is fibonacci list.  We describe the segment
%   with the a tuple of the form:
%     {[A,B], {StartPos, Length}, [C,D]}
%   where
%     [A,B] are the first two elements of the segment.
%     {StartPos, Length} are the starting position of the segment
%       within the original list, and the Length of the segment.
%     [C,D] are the last two elements of the segment.
%   For example, the fibonacci list [3, 4, 7, 11, 18, 29]
%   would get the description {[3,4], {1,6}, [18,29]}
%   If the original list is of empty or of length 1, then the
%   left and right tuples will be the same.  For example, the
%   tuple for [] is {[], {1,0}, []}, and the tuple for
%   [42] is {[42], {1,1}, [42]}.

% field extractors for segments
seg_startPos({_,{StartPos,_Length},_}) -> StartPos.
seg_length({_,{_StartPos,Length},_}) -> Length.
seg_lastPos({_,{StartPos,Length},_}) -> StartPos + Length - 1.
seg_prefix({Prefix,_,_}) -> Prefix.
seg_suffix({_,_,Suffix}) -> Suffix.


% seg_longest(X, Y) -> TheLongerSegment
%   If X and Y have different lengths, return the longer one.
%   If X and Y have the same length, return the one with the smaller
%     starting position.
%seg_longest(X, Y) ->
%  you_need_to_write_this(seg_longest, "function body", [X,Y]).

seg_longest(X, Y) ->
  case seg_length(X) == seg_length(Y) of 
     true -> case seg_startPos(X) < seg_startPos(Y) of
             true-> X;
             false->Y end;
     false -> case seg_length(X) > seg_length(Y)of
            true-> X;
            false->Y end
      end.


% seg_split(List, M) -> {FirstSeg, Rest}
%   List is assumed to satisfy is_integer_list.  The check should be
%   part of fibm_segs so we only check the elements of List once (in
%   fibm_segs) rather that with each recursive call of this function.
%
%   FirstSeg is a tuple describing the longest prefix of List that is
%     a M-Fibonacci sequence.  Because any pair of integers is a M-Fibonacci
%     sequence, FirstSeg has a length of at least two as long as List has
%     a length of at least two.
%   If FirstSeg == List, then Rest == [].
%   Otherwise, Rest is the rest of the list starting with the last element
%   of FirstSeg.  This is to handle cases such as:
%     [0,1,1,2,3,6,9,15,21,36,57,100]
%   where FirstSeg is {[0,1],{1,5},[2,3]} because [0,1,1,2,3] is a
%   M-Fibonacci sequence but [0,1,1,2,3,6] is not, and Rest is
%   [3,6,9,16,21,36,57,100] which will allow a subseqent call to find
%   the segment {[3,6],{5,7},[36,57]} which is longest fibonacci sequence
%   in the original list.
%
%   If length(List) =< 2, then List is a M-Fibonacci sequence and Rest is [].
%     See the notes describing segments for how to construct segment tuples
%     for lists of the form [], [A], or [A, B].
%
% Examples:
%   seg_split([], 100) -> {[], {1,0}, []}
%   seg_split([1,1,2,3,4,5], 100) -> {{[1,1], {1,4}, {2,3}}, [3,4,5]}
%   seg_split([1,1,2,3,4,5], 100) -> {{[1,1], {1,4}, {2,3}}, [3,4,5]}
%   seg_split([3,4,7,11,18,29], 100) -> {{3,4},{1,6},[18,29]}, []}
%seg_split([], _) -> you_need_to_write_this(seg_split, "List is empty");
%seg_split([A], _) -> you_need_to_write_this(seg_split, "List is a singleton", [A]);
%seg_split([A, B], _) -> you_need_to_write_this(seg_split, "List has two elements", [A,B]);
%seg_split([A, B, C | Tl], M) ->
%  you_need_to_write_this(seg_split, "Recursive case", [A, B, C, Tl, M]).


seg_split([], _) -> {{[], {1,0}, []},[]};
seg_split([A], _) -> {{[A], {1,1}, [A]},[]};
seg_split([A, B], _) -> {{[A,B], {1,2}, [A,B]},[]};
seg_split([A, B, C | Tl], M) ->
 seg_split_helper([A,B,C|Tl],M,{1,2},A,B).


seg_split_helper([A,B],_M,{CurStart,CurLen},FirstElement,SecondElement)-> {{[FirstElement,SecondElement],{CurStart,CurLen},[A,B]},[]};

seg_split_helper([A,B,C|Tl],M,{CurStart,CurLen},FirstElement,SecondElement)->

if (A+B )rem M ==C ->
    seg_split_helper([B,C|Tl],M,{CurStart,CurLen+1},FirstElement,SecondElement);
  true-> {{[FirstElement,SecondElement],{CurStart,CurLen},[A,B]},[B,C|Tl]}end.



% fibm_segs(List, M) -> ListOfSegments
%   Return a list of segment descriptions for the fibonacci sequences of List.
%   For example:
%     fibm_segs([0,1,1,2,3,6,9,15,24,39,63,100], 1000) ->
%       [ {[0,1], {1,5}, [2,3]},
%         {[3,6], {5,7}, [39,63]},
%         {[63,100], {11,2}, [63,100]}
%       ].
%fibm_segs(List, M) when is_list(List) -> 
  % Hint:  my implementation ensures that List is an integer list and
  % then calls fibm_segs(List, 1, M).
%  you_need_to_write_this(fibm_segs, "function body", [List, M]).


fibm_segs(List,M) ->
  case  is_integer_list(List) of 
    true-> fibm_segs(List, 1, M);
    false->error(" List has element is not an integer.") end.



fibm_segs(List, StartOffset, M)-> 
    L = seg_split(List,M),
    Prefix = element(1,L),
    NewPrefix = {seg_prefix(Prefix),{StartOffset, seg_length(Prefix)},seg_suffix(Prefix)},
    RestList = element(2,L),
    case RestList ==[ ] of
      true-> [NewPrefix ];
      false -> NewStartOffset = seg_lastPos(NewPrefix),
      [NewPrefix ]++fibm_segs(RestList,NewStartOffset, M) end.


% fibm_segs(List, StartOffset, M) -> ListOfSegments
%   A short description of how my solution worked.  Of course, you're
%   welcome to use a different approach.  I wrote fibm_segs/3 as a
%   helper function.

% fibm_segs_example() -> ok
%   A simple test case that I wrote.  You can use when you write your
%   implementation of fibm_segs.
fibm_segs_example() ->
  Arg = [0,1,1,2,3,6,9,15,24,39,63,100],
  M = 1000,
  Expected = [ {[0,1], {1,5}, [2,3]},
		{[3,6], {5,7}, [39,63]},
		{[63,100], {11,2}, [63,100]}
	     ],
  io:format("fibm_segs(~w, ~b) ->~n", [Arg, M]),
  io:format("  ~w~n", [fibm_segs(Arg, M)]),
  io:format("expected result: ~w~n", [Expected]).


% fibm_leaf(List, M) -> [FirstSeg, LongestSeg, LastSeg]
%   The leaf function for reduce.  Data is a list of integers.
%   We return the first, longest, and last segment descriptions for Data.
%   Example:
%     fibm_leaf([0,1,2,4,6,10,10,20,30,50,80,30,9,48,57,5,62,0], 100) ->
%       [ {[0,1], {1,2}, [0,1]}  %% first
%         {[10,10], {6,7}, [80,30]}  %% longest
%         {[62,0], {17,2}, [62,0]}  %% last
%       ].
fibm_leaf(List, M) ->  L= fibm_segs(List, M),
      A = longest_and_last(L),
      [hd(L),element(1,A),element(2,A)]
      .
  


longest_and_last(L)->longest_and_last_hepler(L, hd(L)).


longest_and_last_hepler(List,Longest) when length(List)==1->{Longest ,hd(List)};
longest_and_last_hepler(List,Longest) ->
   NewLongest = seg_longest(Longest,hd(tl(List))),
    longest_and_last_hepler(tl(List),NewLongest).




% fibm_combine(Left, Right, M)
%   We first handle two minor cases:
%     the Left list is empty, 
%     the Right list is empty.
%   Try a simple example (if you need to), and you'll find that the
%   code to complete these cases is *really* simple.
fibm_combine([{[],_,[]},_,_], Right, _) -> Right;
fibm_combine(Left, [{[],_,[]},_,_], _) ->  Left;
% Now we get to the main case: both Left and Right are non-empty.
fibm_combine(Left, Right, M) ->
  [LeftFirst, LeftLongest, LeftLast] = Left,
  [RightFirst, RightLongest, RightLast] = seg_shift(LeftLast, Right),
  Mid = fibm_combine_help(LeftLast, RightFirst, M),
  FirstSeg =  first_seq_helper(LeftFirst,Mid),
  LongestSeg = longest_seq_helper(LeftLongest, Mid, RightLongest),
  LastSeg =  last_seq_helper(Mid, RightLast),
  [FirstSeg, LongestSeg, LastSeg].


  first_seq_helper(LeftFirst,Mid)->
    LS= seg_startPos(LeftFirst),
    MS = seg_startPos(Mid),
     if
        LS == MS ->
        Mid;
         true-> LeftFirst
     end.

    last_seq_helper(Mid, RightLast)->
    RS= seg_lastPos(RightLast),
    MS = seg_lastPos(Mid),
     if
        RS == MS ->
        Mid;
         true-> RightLast
     end.   



longest_seq_helper(LeftLongest, Mid, RightLongest)-> 
NewLongest = seg_longest(LeftLongest, Mid),
NewLongest2 = seg_longest(Mid, RightLongest),
seg_longest(NewLongest,NewLongest2).

% When combining the results from two subtrees, we first shift the starting postion
%   for the segments from the right subtree by the number of list elements in the
%   left subtree.  seg_shift does this shifting.
% seg_shift comes in three forms:
%     seg_shift(Offset, SegmentDescription): shift the starting position of a single SegmentDescription
%     seg_shift(Offset, ListOfSegmentDescriptions): shift the starting position of each
%                                                   segment description in the list.
%     seg_shift(LeftSeg, RightSeg): LeftSeg is segment a segment description.
%                                   RightSeg is a segment description or a list of
%                                   segment descriptions.
%                                   Shift the starting postition of RightSeg by the
%                                   length of LeftSeg.
%   The call from fibm_leaf is of the form
%     seg_shift(LeftSeg, ListOfSegmentDescriptions)
% Examples:
%   seg_shift(1234, {[3,8], {500,82}, [19,11]}) -> {[3,8], {1734,82}, [19,11]};
%   seg_shift(1000, [{[0,1], {   1,2}, [0,1]}, {[10,10], {   6,7}, [80,30]}, {[62,0], {  17,2}, [62,0]}]) ->
%                   [{[0,1], {1001,2}, [0,1]}, {[10,10], {1006,7}, [80,30]}, {[62,0], {1017,2}, [62,0]}];
%   seg_shift({[3,8], {500,100}, [19,11]}, [{[0,1], {  1,2}, [0,1]}, {[10,10], {   6,7}, [80,30]}]) ->
%                                          [{[0,1], {600,2}, [0,1]}, {[10,10], { 605,7}, [80,30]}].
seg_shift(Offset, {L, {LongestStart,LongestLength}, R}) when is_integer(Offset) ->
   {L,{LongestStart+Offset,LongestLength},R} ;
seg_shift(Offset, []) when is_integer(Offset) -> [];
  % hint: my solution breaks this into two patterns: List = [] and List = [Hd | Tl].
  seg_shift(Offset, [Hd|Tl]) when is_integer(Offset) ->
  [{seg_prefix(Hd),{Offset+seg_startPos(Hd),seg_length(Hd)}, seg_suffix(Hd)}]++seg_shift(Offset,Tl);
seg_shift({_, {LeftStart, LeftLength}, _}, X) ->
  Offset = LeftStart +LeftLength-1,
  seg_shift(Offset, X).


% fibm_combine_help(LeftLastSegment, RightFirstSegment, M) -> MergedSegment
%   The "usual" case is when the Left and Right subtrees each had at least
%   two elements.  We can recognize this because the suffix of
%   LeftLastSegment will be a two element list.  In the code, Left matches
%   the pattern
%     {L,_,[A,B]}
%   Likewise, we can see that the Right subtree has at least two elements
%   if Right matches
%     {[C,D],_,R}
%   Now, we check to see if the M-Fibonacci condition applies where the
%   segments meet:
%     *  If (A+B) rem M =:= C, then we can extend the left sequence with C.
%     *  If (B+C) rem M =:= D, then we can prepend B to the right sequence.
%     *  If both conditions hold, then we can merge the two sequences. 
%     *  If neither condition holds, then [B,C] is a maximal M-Fibonacci sequence.
%   Examples: T/T
%     fibm_combine_help({[1,3],{1,10},[65,1]},{[66,67],{11,11},[64,29]}, 100) ->
%       {[1,3],{1,21},[64,29]};
%       %% Note, this corresponds to a case where
%       %%   LeftList =  [1,3,4,7,11,18,29,36,65,1],
%       %% and
%       %%   RightList = [66,67,33,0,33,33,66,99,65,64,29]
%       %% where LeftList is the list of numbers for the left subtree, and
%       %% RightList is the list of numbers for the right subtree.
%            T/F
%     fibm_combine_help({[1,3],{1,10},[65,1]},{[66,99],{11,10},[37,52]}, 100) ->
%       {[1,3],{1,11},[1,66]};
%       %% This corresponds to a case where
%       %%   LeftList =  [1,3,4,7,11,18,29,36,65,1],
%       %%   RightList = [66,99,65,64,29,93,22,15,37,52]
%            F/T
%     fibm_combine_help({[45,84],{1,10},[49,1]},{[66,67],{11,10},[64,29]}, 100) ->
%       {[1,66],{10,11},[64,29]}
%       %% This corresponds to a case where
%       %%   LeftList = [45,84,29,13,42,55,97,52,49,1],
%       %%   RightList = [66,67,33,0,33,33,66,99,65,64,29]
%            F/F
%     fibm_combine_help({[45,84],{1,10},[49,1]},{[66,99],{11,10},[37,52]}, 100) ->
%       {[1,66],{10,2},[1,66]}
%       %% This corresponds to a case where
%       %%   LeftList = [45,84,29,13,42,55,97,52,49,1],
%       %%   RightList = [66,99,65,64,29,93,22,15,37,52]


fibm_combine_help(Left={L,_,[A,B]}, Right={[C,D],_,R}, M) ->
  case {(A+B) rem M =:= C, (B+C) rem M =:= D} of
    {true,  true}  -> {L,{seg_startPos(Left),seg_length(Left)+seg_length(Right)},R};
    {true,  false} -> {L,{seg_startPos(Left),seg_length(Left)+1},[B,C]};
    {false, true}  -> {[B,C],{seg_startPos(Right)-1,seg_length(Right)+1},R }  ;
    {false, false} -> {[B,C],{seg_startPos(Right)-1,2},[B,C]}
  end;
% Now we look at the cases where one or both of the Left and Right subtrees
%   has fewer than two elements in its list.  We've already handled the case
%   that one (or both) are empty in the patterns for fibm_combine.  This means
%   that (seg_length(Left) =:= 1) or (seg_length(Right) =:= 1).  These cases
%   are tedious.  I'll provide the code.
fibm_combine_help(Left, Right, M) ->
  case seg_suffix(Left) ++ seg_prefix(Right) of
    [A,B] ->
      {[A,B], {seg_startPos(Left),2}, [A,B]};
    [A,B,C] ->
      case (A+B) rem M =:= C of
  true ->
    L = case seg_prefix(Left) of
      [A] -> [A,B];  % Left is a singleton list,
      LL  -> LL      % Left is not a singleton, our two leftmost are the same as for Left
    end,
    R = case seg_suffix(Right) of
      [C] -> [B,C];  % Right is a singleton list,
      RR  -> RR      % Right is not a singlton, our two rightmost are the same as for Right
    end,
    {L, {seg_startPos(Left), seg_length(Left) + seg_length(Right)}, R};
  false ->
    X = [lists:last(seg_suffix(Left)), hd(seg_prefix(Right))],
    {X, {seg_lastPos(Left), 2}, X}
      end
  end.



% longest_fibm_seq_par(WTree, Key, M) -> {StartPosOfLongest, LengthOfLongest}
longest_fibm_seq_par(WTree, DataKey, M) ->
 wtree:reduce(WTree,
   fun(ProcState) -> fibm_leaf(wtree:get(ProcState, DataKey), M) end,
   fun(Left, Right) -> fibm_combine(Left, Right, M) end,
   fun(RootTally) ->
     longest_fibm_root(RootTally)
   end
  ).

 longest_fibm_root([_FirstSeg, LongestSeg,_LastSeg])->{seg_startPos(LongestSeg),seg_length(LongestSeg)}.

% rand_fibbish_list(N, M, AvgLen) -> IntegerList
%   We return a list of N integers, where each integer is in 0..(M-1)
%   We choose the integers to create M-Fibonacci sequence with an
%   average length of about AvgLen.  Note that AvgLen must be > 2.
%   AvgLen can be a float (e.g. 314.159).
rand_fibbish_list(N, M, AvgLen) when is_integer(N), 0 =< N, N =< 2,
                                     is_integer(M), 0 < M ,
				     is_number(AvgLen), 2 < AvgLen ->
  [rand:uniform(M) - 1 || _ <- lists:seq(1,N)];
rand_fibbish_list(N, M, AvgLen) when is_integer(N), 0 =< N, N > 2,
                                     is_integer(M), 0 < M ,
				     is_number(AvgLen), 2 < AvgLen ->
  rand_fibbish_list(N-2, M, 1.0/(AvgLen-2.0), rand_fibbish_list(2, M, AvgLen)).


rand_fibbish_list(0, _, _, Acc) -> Acc;
rand_fibbish_list(N, M, P, Acc=[B, C | _]) ->
  A = case {rand:uniform() < P, C-B >= 0} of
    {true,  _}     -> rand:uniform(M) - 1; % start a new M-Fibonacci sequence
    {false, true}  -> C-B;     % C-B is the predecessor to the current sequence
    {false, false} -> (C-B)+M  % as above, but add M to maintain 0 =< A < M.
  end,
  rand_fibbish_list(N-1, M, P, [A | Acc]).


fibm_par_test(N_Procs, N_Data, M, AvgLen) ->
  L = rand_fibbish_list(N_Data, M, AvgLen),
  W = wtree:create(N_Procs),
  wtree:update(W, fibm_par_data, misc:cut(L, W)),
  ParRaw = time_it:t(fun() -> longest_fibm_seq_par(W, fibm_par_data, M) end),
  {mean, ParTime} = lists:keyfind(mean, 1, ParRaw),
  ParValue = longest_fibm_seq_par(W, fibm_par_data, M),
  wtree:reap(W),
  SeqRaw = time_it:t(fun() -> longest_fibm_seq(L, M) end),
  {mean, SeqTime} = lists:keyfind(mean, 1, SeqRaw),
  SeqValue = longest_fibm_seq(L, M),
  io:format("N_Data = ~8b, SeqTime = ~10.3e, SeqValue = ~w~n",
            [N_Data,       SeqTime,          SeqValue]),
  io:format("                   ParTime = ~10.3e, ParValue = ~w~n",
            [                   ParTime,          ParValue]),
  io:format("                   N_Procs = ~10b, SpeedUp = ~6.3f, ValueMatch=~w~n",
            [                   N_Procs,          SeqTime/ParTime, ParValue =:= SeqValue]).
fibm_par_test() ->code:add_path("/home/c/cs418/public_html/resources/erl"), fibm_par_test(8, 100000, 1000, 10).


% you_need_to_write_this(...)
%   Two functions that raise errors if you've left part of the homework
%   incomplete.  You don't need to write these function -- I've done that.
%   You do need to replace each call to these functions in the templates
%   above with your solutions to the problems.

fibm_par_test_a()->code:add_path("/home/c/cs418/public_html/resources/erl"),
                   fibm_par_test(32, 100, 1000000000, 200),
                  fibm_par_test(32, 1000, 1000000000, 200),
                   fibm_par_test(32, 10000, 1000000000, 200),
                   fibm_par_test(32, 100000, 1000000000, 200),
                   fibm_par_test(32, 1000000, 1000000000, 200),
                   fibm_par_test(32, 10000000, 1000000000, 200) .

fibm_par_test_b()->code:add_path("/home/c/cs418/public_html/resources/erl"),
                   fibm_par_test(1, 1000000, 1000000000, 200),
                    fibm_par_test(2, 1000000, 1000000000, 200),
                      fibm_par_test(4, 1000000, 1000000000, 200),
                        fibm_par_test(8, 1000000, 1000000000, 200),
                          fibm_par_test(16, 1000000, 1000000000, 200),
                            fibm_par_test(32, 1000000, 1000000000, 200),
                              fibm_par_test(64, 1000000, 1000000000, 200),
                                fibm_par_test(128, 1000000, 1000000000, 200),
                                  fibm_par_test(256, 1000000, 1000000000, 200).


you_need_to_write_this(Who, What) ->
  error(error, [{missing_implementation, Who}, {details, What}]).

you_need_to_write_this(Who, What, _Pretend_to_use_these_variables) ->
  you_need_to_write_this(Who, What).
