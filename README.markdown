== Skew Binomial Heaps

Skew Binomial Heaps offer amortized:

    O(1)      insertion
    O(1)      merge
    O(1)      lowest element
    O(log(N)) removing of lowest element

This implementation is straight from Okasaki's PFDS.

== Example

    > Heap = skewbinheap:new().
    []

    > H1 = skewbinheap:insert_all([2,1,4,5], Heap).
    [{0,4,[],[]},{1,1,[3],[{0,2,[],[]}]}]

    > skewbinheap:min(H1)
    1

    > skewbinheap:delete_min(H1).                             
    [{0,3,[],[]},{1,2,[],[{0,4,[],[]}]}]

== TODO
  
  - Add a custom comparator function
