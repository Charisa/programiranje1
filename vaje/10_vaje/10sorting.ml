(* Generate a list of length len with random values up to max. Example:
utop[1]> let l = randlist 10 10 ;;
val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]

Can be used to test sorting functions as in
let l = (randlist 100 100) in selection_imperative_list l = List.sort compare l;;

 *)

let randlist len max = 
   let rec aux len max list = 
      match len with
      | 0 -> list
      | x -> aux (x-1) max [Random.int max]@list
   in aux len max []
   
(* The function insert y xs inserts y into the already sorted list xs and
   returns a sorted list.

   For example:

utop[75]> insert 9 [0; 2];;
- : int list = [0; 2; 9]
utop[76]> insert 1 [0; 2];;
- : int list = [0; 1; 2]
utop[79]> insert 1 [];;
- : int list = [1]
 *)

let insert y list = 
   let rec aux y list sublist = 
      match list with
         | [] -> [y]
         | [x] -> if x < y then List.rev sublist @ [x] @ [y] else List.rev sublist @ [y] @ [x] 
         | x::z::xs -> if x <= y && y <= z then (List.rev sublist) @ [x] @ [y] @ [z] @ xs else aux y (z::xs) (x::sublist)
      in aux y list []

(* The empty list is sorted. We can sort a list by consecutively inserting all
   of its elements into the empty list. Use this idea to write insertion_sort
   using List.fold_left and insert. *)

let ins_sort l = 
   let rec aux subl = function
      | [] -> subl
      | x::xs -> aux (insert x subl) xs 
   in aux [] l

(* Write a recursive function that takes a list l and if l is non-empty,
   returns a pair Some (z, l_without_z) such that z is the smallest element in
   l and l_without_z is l with the first occurance of z removed. *)

let remove z list =
   let rec aux z acc = function
      | [] -> []
      | x::xs -> if x = z then (List.rev acc) @ xs else aux z (x::acc) xs
   in aux z [] list

let min_and_rest l = 
   let rec aux z = function
      | [] -> Some (z, remove z l)
      | x::xs -> if x < z then aux x xs else aux z xs
   in aux (List.hd l) l

let rec select z = function
   | [] -> z, []
   | x::xs -> 
      let z, x = if z < x then z, x else x, z in
         let z, xs = select z xs in 
         z, x::xs

(* Selection sort works by keeping a list l partitioned into a sublist that is
   already sorted and a sublist of leftovers that still needs treatment, and
   consecutively moving the smallest element of the left-overs to the sorted
   part. *)

(* Use min_and_rest to implement selection sort as a recursive function. *)

let rec selection_sort = function
   | [] -> []
   | x::xs -> match select x xs with
      | x, xs -> x::selection_sort xs

(* When working with arrays instead of lists, selection sort can work
   "in-place", i.e. without creating intermediate copies of (parts of) the
   input. it still works by partitioning the input into a sorted part and a
   to-do part. The sorted part is always an initial segment of the input array,
   delimited by an index boundary_sorted. To make progress, we don't extract
   the smallest element of the leftovers, but locate its index, and then swap
   it with the element located at the boundary. When the boundary reaches the
   end of the array, the input is sorted. *)

(* Write a function swap a i j that exchanges a.(i) and a.(j) *)

let swap a i j = 
   let t = a.(i) in a.(i) <- a.(j); a.(j) <- t

(* Write a function index_min a lower upper that computes the index of the
   smallest element in a between indices lower and upper. Example:
   index_min [|0; 2; 9; 3; 6|] 2 4 = 4 *)

let index_min a lower upper = 
   let index_min = ref lower in 
      for i = lower to upper do
         if a.(i) < (!index_min) then
            index_min := i
         done;
      !index_min


(* Implement in-place selection sort. *)
let selection_imperative a = failwith "todo"

(* To test your array-function, you can turn it into a function that sorts
   lists by using Array.of_list and Array.to_list. *)
let selection_imperative_list l = failwith "todo"
