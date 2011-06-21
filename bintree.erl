-module(bintree).

-export([
  children/1,
  key/1,
  link/2,
  new/0,
  new/2,
  rank/1,
  value/1
]).

% Returns the children of a given tree.
children(Tree) ->
  element(4, Tree).

% Returns the key associated with a tree.
key(Tree) ->
  element(2, Tree).

% Link two trees together.
link(T1 = {R, K1, V1, C1}, T2 = {_, K2, V2, C2}) ->
  if
    K1 =< K2 -> 
      {R + 1, K1, V1, [T2 | C1]};
    true -> 
      {R + 1, K2, V2, [T1 | C2]}
  end.

% Create a new binomial tree.
new() ->
  new(nothing, nothing).

new(Key, Value) ->
  new(0, Key, Value).

new(Rank, Key, Value) ->
  {Rank, Key, Value, []}.

% Rank of a tree
rank(Tree) ->
  element(1, Tree).

% Get the value of a tree.
value(Tree) ->
  element(3, Tree).
