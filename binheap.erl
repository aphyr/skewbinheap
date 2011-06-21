-module(binheap).

-export([
  new/0,
  add_element/3,
  delete_min/1,
  min/1,
  is_binheap/1,
  merge/2
]).

% A new binomial heap.
% A binheap is a list of bintrees, sorted in rank order.
new() ->
  {binheap, []}.

% Add the specified key and value to a heap.
add_element(Key, Value, Heap) ->
  add_tree(bintree:new(Key, Value), Heap).

% Add a tree to the heap.
add_tree(Tree, {binheap, []}) ->
  % The trivial case is an empty heap, in which case we become a single-element
  % list of the new tree.
  {binheap, [Tree]};
add_tree(New, {binheap, [First | Rest] = Trees}) ->
  case bintree:rank(New) < bintree:rank(First) of
    true ->
      % The inserted tree is less than our first tree; prepend it to the list.
      {binheap, [New | Trees]};
    false ->
      % The inserted tree is greater; link it to the first tree and recurse.
      add_tree(bintree:link(New, First), {binheap, Rest})
  end.

% Deletes the minimum element of the heap.
% Returns {K, V, Heap}
delete_min({binheap, Trees}) ->
  {Min, Rest} = remove_min_tree1(Trees),
  {bintree:key(Min), bintree:value(Min),
    {binheap, merge(lists:reverse(bintree:children(Min)), Rest)}
  }.

% Returns true if the given term is a binomial heap.
is_binheap({binheap, _}) ->
  true;
is_binheap(_) ->
  false.

% Merge two heaps.
% We walk both heaps and link trees of equal rank.
merge({binheap, _} = Heap1, {binheap, []}) ->
  Heap1;
merge({binheap, []}, {binheap, _} = Heap2) ->
  Heap2;
merge({binheap, Trees1}, {binheap, Trees2}) ->
  {binheap, merge1(Trees1, Trees2)}.

% Helper which simply merges the tree lists.
merge1([First1 | Rest1] = Trees1, [First2 | Rest2] = Trees2) ->
  {R1, R2} = {bintrees:rank(First1), bintrees:rank(First2)},
  if
    R1 < R2 -> [First1 | merge1(Rest1, Trees2)];
    R2 < R1 -> [First2 | merge1(Trees1, Rest2)];
    true    -> add_tree(bintree:link(First1, First2), merge1(Rest1, Rest2))
  end.

% Returns the minimum element.
min(Heap) ->
  {Tree, _} = remove_min_tree(Heap),
  {bintree:key(Tree), bintree:value(Tree)}.

% Remove the tree with the minimum root.
% Returns {min_tree, binheap_with_min_tree_removed}
% We walk the list of trees looking for the smallest one.
remove_min_tree({binheap, Trees}) ->
  {Removed, Trees1} = remove_min_tree1(Trees),
  {Removed, {binheap, Trees1}}.

remove_min_tree1([Tree]) ->
  {Tree, []};
remove_min_tree1([Tree | Rest]) ->
  {Tree1, Rest1} = remove_min_tree1(Rest),
  case bintree:key(Tree) =< bintree:key(Tree1) of
    true  -> {Tree, Rest};
    false -> {Tree1, [Tree | Rest1]}
  end.
