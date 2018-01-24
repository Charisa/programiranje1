(* ===== 99 PROBLEMS IN OCAML  ===== *)


(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)

let rec last = function
	| [] -> []
	| [x] -> [x]
	| _::xs -> last xs

(* 2.  Find the last but one (last and penultimate) elements of a list. (easy) *)

let rec last_two = function
	| [] -> []
	| [x; y] -> [x; y]
	| _::xs -> last_two xs

(* 3. Find the k'th element of a list. (easy) *)

let rec at k l =
	match (k, l) with
	| (_, []) -> []
	| (1, x::xs) -> [x]
	| (_, x::xs) -> if k < 0 then failwith "Negative value" else at (k-1) xs

let rec at1 k = function
	| [] -> []
	| x::xs -> if k = 1 then [x] else at1 (k-1) xs

(* 4. Find the number of elements of a list. (easy) *)

let length list = 
	let rec aux n = function
		| [] -> n
		| _::xs -> aux (n+1) xs
	in aux 0 list

(* 5. Reverse a list. (easy) *)

let rec rev = function
	| [] -> []
	| x::xs -> rev xs @ [x]

(* 6. Find out whether a list is a palindrome. (easy) *)

let is_palindrome list = 
	list = rev list
	
(* 7. Flatten a nested list structure. (medium) *)

type 'a node = 
	| One of 'a
	| Many of 'a node list

let flatten list = 
	let rec aux acc = function
		| [] -> acc
		| One x::xs -> aux (x::acc) xs
		| Many x::xs -> aux (aux acc x) xs 
	in List.rev (aux [] list)

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let rec in_list k = function
	| [] -> false
	| x::xs -> if x = k then true else in_list k xs

let compress list = 
	let rec aux acc = function 
		| [] -> acc
		| x::xs -> if x = at (List.length acc - 1) acc then aux acc xs else aux (x@acc) xs
	in List.rev (aux [] list)


let not_repeating list = 
	let rec aux acc = function
		| [] -> acc
		| x::xs -> if in_list x acc then aux acc xs else aux (x::acc) xs
	in List.rev (aux [] list)

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack list = 
	let rec aux current acc = function
		| [] -> []
		| [x] -> acc @ ([current @ [x]]) 
		| x::y::xs -> if x = y then aux (current @ [x]) acc (y::xs) else
			aux [] (acc @ ([current @ [x]])) (y::xs)
	in aux [] [] list

(* 10. Run-length encoding of a list. (easy) *)

let encode list = 	
	let rec aux count acc = function
		| [] -> []
		| [x] -> acc @ [(count + 1, x)] 
		| x::y::xs -> if x = y then aux (count + 1) acc (y::xs) else
			aux 0 (acc @ [(count + 1, x)]) (y::xs) 
	in aux 0 [] list


let encode1 list = 
	List.map (fun l -> (List.length l, List.hd l)) (pack list)

(* 11. Modified run-length encoding. (easy) *)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

 let encode2 list = 
 	let create_tupple n element = 
 		if n = 1 then One element else Many (n, element) 
 	in
 	let rec aux count acc = function
 		| [] -> []
 		| [x] -> acc @[(create_tupple (count +1) x )]
 		| x::y::xs -> if x = y then aux (count + 1) acc (y::xs) else
 			aux 0 (acc@[(create_tupple (count + 1) x)]) (y::xs)
 	in aux 0 [] list

(* 12. Decode a run-length encoded list. (medium) *)

let repeat k element = 
	let rec aux k element acc =
		if k < 0 then failwith "Not valid value!" else
			(match k with
				| 0 -> acc
				| _ -> aux (k-1) element (element::acc)
			)
	in aux k element []


let decode list = 
	let rec aux acc = function
		| [] -> acc
		| One x::xs -> aux (acc@[x]) xs
		| Many (k, element)::xs -> aux (acc@(repeat k element)) xs
	in aux [] list

(* 13. Run-length encoding of a list (direct solution). (medium) *)

let encode3 list =
    let rle count x = if count = 0 then One x else 
    	Many (count + 1, x) 
    in
    let rec aux count acc = function
      | [] -> [] 
      | [x] -> rle count x :: acc
      | x::y::xs -> if x = y then aux (count + 1) acc (y::xs)
                              else aux 0 (rle count x :: acc) (y::xs)  in
    List.rev (aux 0 [] list);;

(* 14. Duplicate the elements of a list. (easy) *)

let duplicate list = 
	let rec aux acc = function
		| [] -> acc
		| x::xs -> aux (acc@[x;x]) xs
	in aux [] list

(* 15. Replicate the elements of a list a given number of times. (medium) *)

let replicate list times = 
	let rec aux list counter new_list = 
		match list with
		| [] -> new_list
		| [x] -> if counter = times then x::new_list else aux [x] (counter + 1) (x::new_list)
		| x::xs -> if counter = times then aux xs 1 (x :: new_list) else aux (x::xs) (counter + 1) (x::new_list)
	in List.rev (aux list 1 [])

let replicate1 list n =
    let rec prepend n acc x =
      if n = 0 then acc else prepend (n-1) (x :: acc) x in
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (prepend n acc h) t  in
    (* This could also be written as:
       List.fold_left (prepend n) [] (List.rev list) *)
    aux [] (List.rev list);;

(* 16. Drop every N'th element from a list. (medium) *)

let drop list n =
	let rec aux acc new_list = function
		| [] -> new_list
		| x::xs -> if acc = n then aux 1 new_list xs else aux (acc + 1) (x::new_list) xs
	in List.rev (aux 1 [] list)

(* 17. Split a list into two parts; the length of the first part is given. (easy) *)

let split list n =
	if n > List.length list then (list, []) else (
		let rec aux l1 l2 acc = function
		    | [] -> (l1, l2)
		    | x::xs -> if acc <= n then aux (l1@[x]) l2 (acc + 1) xs else aux l1 (l2@[x]) acc xs
		 in aux [] [] 1 list
	)	

let split1 list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i-1) (h :: acc) t  in
    aux n [] list;;

(* 18. Extract a slice from a list. (medium) *)

let slice list i k =
	let rec aux counter acc = function
		| [] -> acc
		| x::xs -> if (counter < i) || (counter > k) then aux (counter + 1) acc xs else aux (counter + 1) (x::acc) xs
	in List.rev (aux 0 [] list)


let slice1 list i k =
    let rec take n = function
      | [] -> []
      | h :: t -> if n = 0 then [] else h :: take (n-1) t
    in
    let rec drop n = function
      | [] -> []
      | h :: t as l -> if n = 0 then l else drop (n-1) t
    in
    take (k - i + 1) (drop i list);;

(* 19. Rotate a list N places to the left. (medium) *)

let rotate list n = 
	let rec aux acc counter = function
		| [] -> acc
		| x::xs -> if counter = n then xs@acc@[x] else aux (acc@[x]) (counter + 1) xs
	in aux [] 1 list	


let split2 list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i-1) (h :: acc) t  in
    aux n [] list
  
  let rotate2 list n =
    let len = List.length list in
    (* Compute a rotation value between 0 and len-1 *)
    let n = if len = 0 then 0 else (n mod len + len) mod len in
    if n = 0 then list
    else let a, b = split2 list n in b @ a

(* 20. Remove the K'th element from a list. (easy) *)

let remove_at k list =
	let rec aux counter acc = function
		| [] -> acc
		| x::xs -> if k = counter then acc@xs else aux (counter + 1) (x::acc) xs
	in aux 0 [] list 

(* 21. Insert an element at a given position into a list. (easy) *)

let insert_at element k list = 
	if k = List.length list then list@[element] else (
	let rec aux counter acc = function
		| [] -> []
		| x::xs as l -> if counter = k then acc@[element]@l else aux (counter + 1) (x::acc) xs
	in aux 0 [] list)

let rec insert_at1 x n = function
    | [] -> [x]
    | h :: t as l -> if n = 0 then x :: l else h :: insert_at1 x (n-1) t

(* 22. Create a list containing all integers within a given range. (easy) *)

let rec increase i k acc =
	if i + 1 = k then k::i::acc else increase (i+1) k (i::acc)


let range i k = 
	if i <= k then (
		List.rev (increase i k [])
	) else (
		increase k i []
	)

(* 23. Extract a given number of randomly selected elements from a list. (medium) *)
(* !!!!!! *)

let rand_select list n = 
	let rec extract acc n = function
		| [] -> raise Not_found
		| x::xs -> if n = 0 then (x, acc @ xs) else extract (x::acc) (n-1) xs
	in 
	let extract_random list len = 
		extract [] (Random.int len) list 
	in 
	let rec aux n acc list len = 
		if n = 0 then acc else 
			let picked, rest = extract_random list len 
			in aux (n-1) (picked::acc) rest (len-1)
	in 
	let len = List.length list 
	in 
	aux (min n len) [] list len
	
(* 24. Lotto: Draw N different random numbers from the set 1..M. (easy) *)

let lotto_select n max = 
	let random_number max = Random.int max 
	in
	let rec aux acc n max = 
		if n = 0 then acc else aux ((random_number max)::acc) (n-1) max
	in 
	aux [] n max 

(* 25. Generate a random permutation of the elements of a list. (easy) *)
(* !!!!!! *)

let permutation list =
    let rec extract acc n = function
      | [] -> raise Not_found
      | x :: xs -> if n = 0 then (x, acc @ xs) else extract (x::acc) (n-1) xs
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux acc list len =
      if len = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (picked :: acc) rest (len-1)
    in
    aux [] list (List.length list)
	

(* 26. Generate the combinations of K distinct objects chosen from the N elements 
of a list. (medium) *)

let rec extract k list =
    if k <= 0 then [ [] ]
    else match list with
         | [] -> []
         | x :: xs ->
            let with_x = List.map (fun l -> x :: l) (extract (k-1) xs) 
        in
            let without_x = extract k xs 
        in
            with_x @ without_x






