
(* ===== Exercise 1: Introduction to OCaml  ===== *)


(* The function "penultimate_element l" returns the second-to-last element of the list l. 
 If the list is too short it raises an error.
 ----------
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
 ---------- *)

let rec penultimate_element = function
	| [] -> failwith "The list is too short."
	| [x] -> failwith "The list is too short"
	| [x; y] -> x
	| _::xs -> penultimate_element xs

(* The function "get k l" returns the k-th element in the list l.
 Numbering (as usual) starts with 0.
 Suppose that k is non-negative.
 If the list is too short it raises an error.
 ----------
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
 ---------- *)

let rec get k l = 
	match (k,l) with
	| (_, []) -> failwith "The list is not long enough!"
	| (0, x::xs) -> x
	| (k, x::xs) -> get (k-1) xs 

(* The function "double l" doubles the occurences of elements in the list l.
 ----------
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
 ---------- *)

let rec double l = 
	match l with
	| [] -> []
	| [x] -> [x;x]
	| x::xs -> x :: x :: double xs


(* The function "divide k l" divides the list l into a pair of lists. The first list
 contains the first k elements of the list l and the second list contains the rest.
 When k is outside the bounds of l, the appropriate list should be [])
 ----------
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
 ---------- *)
 
let rec divide k l = 
	match (k, l) with
	| (_, []) -> ([], [])
	| (0, _) -> ([], l)
	| (k, x::xs) -> let (list1, list2) = divide (k-1) xs in (x::list1, list2) 


(* The function "delete k l" removes the k-th element of the list l.
 If the list is too short it raises an error.
 ----------
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
 ---------- *)
 
let rec delete k l = 
	match (k, l) with
	| (_, []) -> failwith "The list is too short!"
	| (0, x::xs) -> xs
	| (k, x::xs) -> x:: delete (k-1) xs 


(* The function "slice i k l" returns the sub-list of l from the i-th up
 to (excluding) the k-th element.
 Suppose that i and k are fitting.
 ----------
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
 ---------- *)
 
let rec first_k_elements k l =
	match (k, l) with
	| (0, _) | (_, []) -> []
	| (_, x::xs) ->  x :: first_k_elements (k-1) xs


let rec slice i k l = 
	match (i, l) with
	| (_, []) -> []
	| (0, _) -> first_k_elements k l
	| (_, x::xs) -> slice (i-1) (k-1) xs


(* The function "insert x k l" inserts (not replaces) x into the list l at index k.
 If k is outside of bounds of l insert at the beggining or the end instead.
 ----------
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
 ---------- *)

let rec insert y k l = 
	match (k, l) with
	| (0, _) -> y :: l
	| (_, []) -> [y]
	| (_, x::xs) -> if k <= 0 then y::l else x :: insert y (k-1) xs


(* The function "rotate n l" rotates the list l to the left by n places.
 Suppose that n is within the bounds of l.
 ----------
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
 ---------- *)

let rec rotate n l = 
	match (n, l) with
	| (_,[]) -> []
	| (0, _) -> l
	| (_, x::xs) -> rotate (n-1) (xs @ [x])

 
(* The function "remove x l" removes all occurrences of x in the list l.
 ----------
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
 ---------- *)

let rec remove y l = 
	match (y, l) with
	| (_, []) -> []
	| (_, x::xs) -> if x = y then remove y xs else x::remove y xs


(* The function "is_palindrome l" checks if a list is a palindrome.
 Hint: Use an auxiliary function that reverses a list. 
 ----------
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
 ---------- *)
 
let rec is_palindrome l = 
	match l with 
	| [] -> true
	| [x] -> true
	| x::xs -> if x = get (List.length xs - 1) xs  then is_palindrome (delete (List.length xs - 1) xs) else false
  

(* The function "max_on_components l1 l2" returns a list with the maximum element
 of lists l1 and l2 at the each index.
 The lenght of the returned list should be equal to the shorter of l1 and l2. 
 ----------
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
 ---------- *)

let rec max_on_components l1 l2 = 
	match (l1, l2) with
	| ([], _) | (_, []) -> []
	| (x::xs, y::ys) -> Pervasives.max x y :: max_on_components xs ys
  
(* The function "second_largest l" returns the second largest value in the list l.
 Multiple occurrences of the same element count as one value.
 Suppose the list contains at least two distinct values.
 Hint: Use an auxiliary function that finds the maximum element of a list. 
 ----------
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
 ---------- *)
 
let rec maximum l = 
	match l with
	| [] -> []
	| [x] -> x
	| [x; y] -> if x > y then x else y
	| x::y::xs -> if x > y then maximum (x :: xs) else maximum (y :: xs)


let second_largest list =
	let rec aux first second = function
		| [] -> []
		| [x] -> if [x] > first then first else if ([x] < first) && ([x] > second) then [x] else second
		| x::xs -> if [x] > first then aux [x] first xs else if ([x] < first) && ([x] > second) then aux first [x] xs
			else aux first second xs
	in aux [0] [0] list		