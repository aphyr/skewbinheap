-module(binheap_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

% Construct a heap from a list of ints.
build_heap([]) ->
  binheap:new();
build_heap([H|T]) ->
  binheap:add_element(H, null, build_heap(T)).

% Deconstruct a heap in sorted order. n log n. :(


% Test that a new heap is a heap.
prop_new() ->
  ?FORALL({_}, {int()},
    binheap:is_binheap(binheap:new())
  ).

test_new() -> quickcheck(prop_new(), 1).

% Test that inserting elements returns the least.
prop_min() ->
  ?FORALL({Xs}, {list(int())},
  ?IMPLIES(length(Xs) > 0,
    begin
      {K, _} = binheap:min(build_heap(Xs)),
      equals(K, lists:min(Xs))
    end)).

test_min() -> quickcheck(prop_min()).

% Test that successive minimum elements are sorted.
prop_min
