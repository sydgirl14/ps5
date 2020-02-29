(*
                         CS 51 Problem Set 5
                Modules, Functors, and Priority Queues
                           Priority Queues
 *)

open Order

open Orderedcoll ;;

(*======================================================================
Priority queues

A signature for a priority queue. See the problem set specification
for more information about priority queues.

IMPORTANT: In your implementations of priority queues, the MINIMUM
valued element corresponds to the HIGHEST priority. For example, in an
integer priority queue, the integer 4 has lower priority than the
integer 2. In case of multiple elements with identical priority, the
priority queue returns them according to the normal queue discipline,
first-in-first-out.
......................................................................*)

module type PRIOQUEUE =
sig
  exception QueueEmpty

  (* The type of elements being stored in the priority queue *)
  type elt

  (* The queue itself (stores things of type `elt`) *)
  type queue

  (* Returns an empty queue *)
  val empty : queue

  (* Returns whether or not a queue is empty *)
  val is_empty : queue -> bool

  (* Returns a new queue with the element added *)
  val add : elt -> queue -> queue

  (* Returns a pair of the highest priority element in the argument
     queue and the queue with that element removed. In case there is
     more than one element with the same priority (that is, the
     elements compare `Equal`), returns the one that was added
     first. Can raise the `QueueEmpty` exception. *)
  val take : queue -> elt * queue

  (* Returns a string representation of the queue *)
  val to_string : queue -> string

  (* Runs invariant checks on the implementation of this binary tree.
     May raise `Assert_failure` exception *)
  val run_tests : unit -> unit

end

(*......................................................................
Problem 2: Implementing ListQueue

Implement a priority queue functor called `ListQueue`, which uses a
simple list to store the queue elements in sorted order. Feel free to
use anything from the `List` module.

After you've implemented `ListQueue`, you'll want to test the functor
by, say, generating an `IntString` priority queue and running the tests
to make sure your implementation works.
......................................................................*)

module ListQueue (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
  struct
    exception QueueEmpty

    type elt = C.t

    type queue = elt list

    let empty : queue =
      failwith "ListQueue empty not implemented"

    let is_empty (q : queue) : bool =
      failwith "ListQueue is_empty not implemented"

    let add (e : elt) (q : queue) : queue =
      failwith "ListQueue add not implemented"

    let take (q : queue) : elt * queue =
      failwith "ListQueue take not implemented"

    let run_tests () =
      failwith "ListQueue run_tests not implemented"

    (* IMPORTANT: Don't change the implementation of to_string. *)
    let to_string (q: queue) : string =
      let rec to_string' q =
        match q with
        | [] -> ""
        | [hd] -> (C.to_string hd)
        | hd :: tl -> (C.to_string hd) ^ ";" ^ (to_string' tl)
      in
      let qs = to_string' q in "[" ^ qs ^ "]"
  end

(*......................................................................
Problem 3: Implementing TreeQueue

Now implement a functor TreeQueue that generates implementations of
the priority queue signature PRIOQUEUE using a binary search tree.
Luckily, you should be able to use *a lot* of your code from the work
with `BinSTree`!

If you run into problems implementing `TreeQueue`, you can at least
add stub code for each of the values you need to implement so that
this file will compile and will work with the unit testing
code. That way you'll be able to submit the problem set so that it
compiles cleanly.
......................................................................*)

(* You'll want to uncomment this before working on this section! *)
(*
module TreeQueue (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
  struct
    exception QueueEmpty

    (* You can use the module T to access the functions defined in BinSTree,
       e.g. T.insert *)
    module T = (BinSTree(C) : (ORDERED_COLLECTION with type elt = C.t))

    (* Implement the remainder of the module. *)

  end
 *)

(*......................................................................
Problem 4: Implementing BinaryHeap

Implement a priority queue using a binary heap. See the problem set
writeup for more info.

You should implement a min-heap, i.e., the top of your heap stores the
smallest element in the entire heap (which will end up being the
element with highest priority when the heap is iused as a priority
queue).

Note that, unlike for your tree and list implementations of priority
queues, you do *not* need to worry about the order in which elements
of equal priority are removed. Yes, this means it's not really a
"queue", but it is easier to implement without that restriction.

Be sure to read the problem set spec for hints and clarifications!

Remember the invariants of the tree that make up your queue:

1) A tree is ODD if its left subtree has 1 more node than its right
subtree. It is EVEN if its left and right subtrees have the same
number of nodes. The tree can never be in any other state. This is the
WEAK invariant, and should never be false.

2) All nodes in the subtrees of a node should be *greater* than (or
equal to) the value of that node.  This, combined with the previous
invariant, makes a STRONG invariant.  Any tree that a user passes in
to your module and receives back from it should satisfy this
invariant.  However, in the process of, say, adding a node to the
tree, the tree may intermittently not satisfy the order invariant. If
so, you *must* fix the tree before returning it to the user.  Fill in
the rest of the module below!
......................................................................*)
   
module BinaryHeap (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
  struct

    exception QueueEmpty

    type elt = C.t

    (* A node in the tree is either even or odd *)
    type balance = Even | Odd

    (* A tree either:
       1) is one single element,
       2) has one branch, where:
          the first element in the tuple is the element at this node,
          and the second element is the element down the branch,
       3) or has two branches (with the node being even or odd)
    *)
    type tree =
      | Leaf of elt
      | OneBranch of elt * elt
      | TwoBranch of balance * elt * tree * tree

    (* A queue is either empty or a tree *)
    type queue =
      | Empty
      | Tree of tree

    let empty = Empty

    (* to_string q -- Prints binary heap `q` as a string - nice for
       testing! *)
    let to_string (q: queue) =
      let rec to_string' (t: tree) =
        match t with
        | Leaf e1 -> "Leaf " ^ C.to_string e1
        | OneBranch(e1, e2) ->
                 "OneBranch (" ^ C.to_string e1 ^ ", "
                 ^ C.to_string e2 ^ ")"
        | TwoBranch(Odd, e1, t1, t2) ->
                 "TwoBranch (Odd, " ^ C.to_string e1 ^ ", "
                 ^ to_string' t1 ^ ", " ^ to_string' t2 ^ ")"
        | TwoBranch(Even, e1, t1, t2) ->
                 "TwoBranch (Even, " ^ C.to_string e1 ^ ", "
                 ^ to_string' t1 ^ ", " ^ to_string' t2 ^ ")"
      in
      match q with
      | Empty -> "Empty"
      | Tree t -> to_string' t

    (* is_empty q -- Predicate returns `true` if and only if `q` is
       an empty queue *)
    let is_empty : queue -> bool =
      (=) Empty

    (* add e q -- Adds element `e` to the queue `q` *)
    let add (e : elt) (q : queue) : queue =
      
      (* Given a tree, where `e` will be inserted is deterministic based
         on the invariants. If we encounter a node in the tree where
         its value is greater than the element being inserted, then we
         place the new `elt` in that spot and propagate what used to be
         at that spot down toward where the new element would have
         been inserted *)
      let rec add_to_tree (e : elt) (t : tree) : tree =
        match t with
        (* If the tree is just a Leaf, then we end up with a OneBranch *)
        | Leaf e1 ->
           (match C.compare e e1 with
            | Equal
            | Greater -> OneBranch (e1, e)
            | Less -> OneBranch (e, e1))

        (* If the tree was a OneBranch, it will now be a TwoBranch *)
        | OneBranch (e1, e2) ->
           (match C.compare e e1 with
            | Equal
            | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
            | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

        (* If the tree was even, then it will become an odd tree (and
           the element is inserted to the left *)
        | TwoBranch (Even, e1, t1, t2) ->
           (match C.compare e e1 with
            | Equal
            | Greater -> TwoBranch (Odd, e1, add_to_tree e t1, t2)
            | Less -> TwoBranch (Odd, e, add_to_tree e1 t1, t2))

        (* If the tree was odd, then it will become an even tree (and
           the element is inserted to the right *)
        | TwoBranch (Odd, e1, t1, t2) ->
           match C.compare e e1 with
           | Equal
           | Greater -> TwoBranch (Even, e1, t1, add_to_tree e t2)
           | Less -> TwoBranch (Even, e, t1, add_to_tree e1 t2)
      in
      
      (* If the queue is empty, then `e` is the only Leaf in the tree.
         Else, insert it into the proper location in the pre-existing
         tree *)
      match q with
      | Empty -> Tree (Leaf e)
      | Tree t -> Tree (add_to_tree e t)

    (*..................................................................
    get_top t -- Returns the top element of the tree `t` (i.e., just a
    single pattern match)
    ..................................................................*)
    let get_top (t : tree) : elt =
      failwith "BinaryHeap get_top not implemented"

    (*..................................................................
    fix t -- Fixes trees whose top node is greater than its
    children. If fixing it results in a subtree where the node is
    greater than its children, then it (recursively) fixes this
    tree too. Resulting tree satisfies the strong invariant.
    ..................................................................*)
    let fix (t : tree) : tree =
      failwith "BinaryHeap fix not implemented"

    let extract_tree (q : queue) : tree =
      match q with
      | Empty -> raise QueueEmpty
      | Tree t -> t

    (*..................................................................
    get_last t -- Takes a tree, and returns the item that was most
    recently inserted into that tree, as well as the queue that
    results from removing that element. Notice that a queue is
    returned. (This happens because removing an element from just a
    leaf would result in an empty case, which is captured by the queue
    type).

    By "item most recently inserted", we don't mean the most recently
    inserted *value*, but rather the newest node that was added to the
    bottom-level of the tree. If you follow the implementation of `add`
    carefully, you'll see that the newest value may end up somewhere
    in the middle of the tree, but there is always *some* value
    brought down into a new node at the bottom of the tree. *This* is
    the node that we want you to return.
    ..................................................................*)
    let get_last (t : tree) : elt * queue =
      failwith "BinaryHeap get_last not implemented"

    (*..................................................................
    take q -- Returns the hgiest priority element from `q` and the
    queue that ersults from deleting it. Implements the algorithm
    described in the writeup. You must finish this implementation, as
    well as the implementations of get_last and fix, which take uses.
    ..................................................................*)
    let take (q : queue) : elt * queue =
      match extract_tree q with
      (* If the tree is just a Leaf, then return the value of that
         leaf, and the new queue is now empty *)
      | Leaf e -> e, Empty

      (* If the tree is a OneBranch, then the new queue is just a
         Leaf *)
      | OneBranch (e1, e2) -> e1, Tree (Leaf e2)

      (* Removing an item from an even tree results in an odd
         tree. This implementation replaces the root node with the
         most recently inserted item, and then fixes the tree that
         results if it is violating the strong invariant *)
      | TwoBranch (Even, e, t1, t2) ->
         let (last, q2') = get_last t2 in
         (match q2' with
          (* If one branch of the tree was just a leaf, we now have
             just a OneBranch *)
          | Empty -> (e, Tree (fix (OneBranch (last, get_top t1))))
          | Tree t2' -> (e, Tree (fix (TwoBranch (Odd, last, t1, t2')))))
         (* Implement the odd case! *)
          | TwoBranch (Odd, _e, _t1, _t2) ->
             failwith "BinaryHeap take incomplete"

    let run_tests () = failwith "BinaryHeap run_tests not implemented"
  end

(*......................................................................
Now to actually use the priority queue implementations for something
useful!

Priority queues are very closely related to sorts. Remember that
removal of elements from priority queues removes elements in highest
priority to lowest priority order. So, if your priority for an element
is directly related to the value of the element, then you should be
able to come up with a simple way to use a priority queue for
sorting...

In OCaml 3.12 and above, modules can be turned into first-class
values, and so can be passed to functions! Here, we're using that to
avoid having to create a functor for sort. Creating the appropriate
functor is a challenge problem :-)

The following code is simply using the functors and passing in a
COMPARABLE module for integers, resulting in priority queues tailored
for ints.
......................................................................  *)

module IntListQueue = (ListQueue(IntCompare) :
                         PRIOQUEUE with type elt = IntCompare.t)

module IntHeapQueue = (BinaryHeap(IntCompare) :
                         PRIOQUEUE with type elt = IntCompare.t)

(* Uncomment this once your TreeQueue implementation is complete

module IntTreeQueue = (TreeQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)

*)

(* Store the whole modules in these variables *)
let list_module = (module IntListQueue :
                     PRIOQUEUE with type elt = IntCompare.t)
let heap_module = (module IntHeapQueue :
                     PRIOQUEUE with type elt = IntCompare.t)
(*
let tree_module = (module IntTreeQueue :
                     PRIOQUEUE with type elt = IntCompare.t)
*)

(* Implementing sort using generic priority queues. *)
let sort (m : (module PRIOQUEUE with type elt=IntCompare.t)) (lst : int list) =
  let module P = (val (m) : PRIOQUEUE with type elt = IntCompare.t) in
  let rec extractor pq lst =
    if P.is_empty pq then lst
    else
      let (x, pq') = P.take pq in
      extractor pq' (x::lst) in
  let pq = List.fold_right P.add lst P.empty in
  List.rev (extractor pq [])


(* Now, we can pass in the modules into sort and get out different
   sorts. *)

(* Sorting with a priority queue with an underlying heap
   implementation is equivalent to heap sort! *)
let heapsort = sort heap_module ;;

(* Sorting with a priority queue with your underlying tree
   implementation is *almost* equivalent to treesort; a real treesort
   relies on self-balancing binary search trees *)

(*
let treesort = sort tree_module ;;
*)

(* Sorting with a priority queue with an underlying unordered list
   implementation is equivalent to selection sort! If your
   implementation of ListQueue used ordered lists, then this is really
   insertion sort. *)
let selectionsort = sort list_module

(* You should test that these sorts all correctly work, and that lists
   are returned in non-decreasing order. *)

(*......................................................................
Section 4: Challenge problem: Sort function

A reminder: Challenge problems are for your karmic edification
only. You should feel free to do these after you've done your best
work on the primary part of the problem set.

Above, we only allow for sorting on int lists. Write a functor that
will take a COMPARABLE module as an argument, and allows for sorting
on the type defined by that module. You should use your BinaryHeap
module.

As challenge problems go, this one is relatively easy, but you should
still only attempt this once you are completely satisfied with the
primary part of the problem set.
......................................................................*)

(*......................................................................
Section 5: Challenge problem: Benchmarking

Now that you are learning about asymptotic complexity, try to write
some functions to analyze the running time of the three different
sorts. Record in a comment here the results of running each type of
sort on lists of various sizes (you may find it useful to make a
function to generate large lists).  Of course include your code for
how you performed the measurements below.  Be convincing when
establishing the algorithmic complexity of each sort.  See the CS51
and Sys modules for functions related to keeping track of time
......................................................................*)
                         
(*======================================================================
Reflection on the problem set

     Please fill out the information about time spent and your
     reflection thereon in the file orderedcoll.ml.
 *)
