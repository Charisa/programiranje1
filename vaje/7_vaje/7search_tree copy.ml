(* ===== Exercise 4: Search Tree  ===== *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* Standard test example.
          5
         / \
        2   7
       /   / \
      0   6   11
   *)

let leaf x = Node(Empty, x, Empty) (* Function to shorten tree creation. *)

let test_tree = Node( Node(leaf 0, 2, Empty), 5, Node(leaf 6, 7, leaf 11))

(* Funkcija "mirror t" vrne prezrcaljeno drevo. Na primeru test_tree:
       5
      / \
     7   2
    / \   \
   11  6   0
   ----------
   # mirror test_tree ;;
   - : int tree =
   Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
   Node (Empty, 2, Node (Empty, 0, Empty)))
   ---------- *)


let rec mirror = function
  | Empty -> Empty
  | Node (l, x, d) -> Node (mirror d, x, mirror l)


(* Funkcija "height t" vrne višino oz. globino drevesa, funkcija "size t" pa
   število vseh vozlišč drevesa.
   ----------
   # height test_tree;;
   - : int = 3
   # size test_tree;;
   - : int = 6
   ---------- *)

let rec height = function 
  | Empty -> 0
  | Node(l, x, d) -> 1 + max (height l) (height d)

let rec size = function
  | Empty -> 0
  | Node(l, x, d) -> 1 + size l + size d

(* Funkcija "follow directions t" tipa [direction list -> 'a tree -> 'a option]
   sprejme seznam navodil za premikanje po drevesu. Ker morda navodila ne vodijo
   do nobenega vozlišča v drevesu uporabi tip option.
   ----------
   # follow [Right;Left] test_tree;;
   - : int option = Some 6
   # follow [Right;Left;Right;Right] test_tree;;
   - : int option = None
   ---------- *)

type direction = Left | Right

let rec follow directions t : 'a option =
  match t with
  | Empty -> None
  | Node (l, x, d) -> 
    begin 
      match directions with
          | [] -> Some x
          | y::ys -> if y = Left then follow ys l else follow ys d
    end


(* Funkcija "prune directions t"  [direction list -> 'a tree -> 'a tree option]
   poišče vozlišče v drevesu glede na navodila, ter izbriše poddrevo, ki se
   začne v izbranem vozlišču.
   Opozorilo: pri uporabi Some Node(l, x, r) se OCaml pritoži, saj to prebere
   kot (Some Node)(l, x, r) zato pravilno postavi dodatne oklepaje.
   ----------
   # prune [Right] test_tree;;
   - : int tree option =
   Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
   ---------- *)

let rec prune (directions : direction list) (t : 'a tree) : 'a tree option = 
  match t with
  | Empty -> None
  | Node (l, x, d) ->  
    begin 
      match directions with
          | [] -> Some Empty
          | y::ys -> if y = Left then 
            begin
              match prune ys l with
              | None -> None
              | Some new_l -> Some (Node(new_l, x, d))
            end
          else 
            begin
              match prune ys d with
              | None -> None
              | Some new_d -> Some(Node(l, x, new_d))
            end
    end


(* Funkcija "map_tree f t"  [('a -> 'b) -> 'a tree -> 'b tree] preslika podatke
   drevesa t s funkcijo f.
   ----------
   # map_tree ((<)3) test_tree;;
   - : bool tree =
   Node (Node (Node (Empty, false, Empty), false, Empty), true,
   Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
   ---------- *)

let rec map_tree (f : ('a -> 'b)) (t : 'a tree) : 'b tree = 
  match t with
  | Empty -> Empty
  | Node(l, x, d) -> Node (map_tree f l, f x, map_tree f d)


(* Funkcija "list_of_tree t"  ['a tree -> 'a list] preslika podatke
   drevesa t v seznam. Vrstni red naj bo takšen, da v primeru binarnega
   iskalnega drevesa vrne urejen seznam.
   ----------
   # list_of_tree test_tree;;
   - : int list = [0; 2; 5; 6; 7; 11]
   ---------- *)

let rec list_of_tree (t : 'a tree) : 'a list = 
  match t with
  | Empty -> []
  | Node(l, x, d) -> (list_of_tree l) @ [x]  @ (list_of_tree d)


(* Funkcija "is_bst t" ['a tree -> bool] preveri ali je drevo binarno iskalno
   drevo (Binary Search Tree, BST). Predpostavi, da v drevesu ni ponovitev
   elementov (torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)).
   Prazno drevo naj predstavlja BST.
   ----------
   # is_bst test_tree;;
   - : bool = true
   # test_tree |> mirror |> is_bst;;
   - : bool = false
   ---------- *)

let rec is_bst (t : 'a tree) : bool = 
  let rec is_ordered = function
    | [] | _::[] -> true
    | x::y::xs -> if x <= y then is_ordered (y::xs) 
                  else false
    in 
    t |> list_of_tree |> is_ordered

(*------------------------------------------------------------------------------
   In the remaining exercises the variable name bst assumes a BST input.
  ----------------------------------------------------------------------------*)


(* Funkcija "insert x bst" ['a -> 'a tree -> 'a tree] v bst vstavi element x.
   Funkcija "member x bst" ['a -> 'a tree -> bool] preveri ali je element v bst.
   ----------
   # insert 2 (leaf 4);;
   - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
   # member 3 test_tree;;
- : bool = false
   ---------- *)

let rec insert (x : 'a) (bst : 'a tree) : 'a tree = 
  match bst with
  | Empty -> Node(Empty, x, Empty)
  | Node(l, y, d) -> if y < x then Node(l, y, insert x d) else Node(insert x l, y, d)


let rec member (x : 'a) (bst : 'a tree) : bool = 
  match bst with
  | Empty -> false
  | Node(l, y, d) -> if y = x then true else if y < x then member x d else member x l



(* Napiši še funkcijo "member2", kjer ne privzameš, da je drevo bst.
   Premisli kakšna je časovna zahtevnost funkcije "member" in kakšna funkcije
   "member2" na drevesu z n vozlišči, ki ima globino log(n). *)

let rec member2 (x : 'a) (t : 'a tree) : bool = 
  match t with
  | Empty -> false
  | Node(l, y, d) -> x = y || (member2 x l) || (member2 x d)


(* Funkcija "bst_of_list l" ['a list -> 'a tree] iz seznama naredi binarno
   iskalno drevo.
   Namig: na predavanjih je profesor najprej definiral funkcijo "insert", ki
   v bst vstavi en element.
   ----------
   # [11;6;7;0;2;5] |> bst_of_list |> is_bst;;
   - : bool = true
   ---------- *)

let bst_of_list (l : 'a list) : 'a tree = List.fold_right insert l Empty 


(* Sestavi funkcijo "tree_sort l" ['a list -> 'a list], ki uredi seznam l.
   ----------
   # tree_sort ["a";"c";"f";"b";"e";"d"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   ---------- *)

let tree_sort (l : 'a list) : 'a list = l |> bst_of_list |> list_of_tree


(* Funkcija "succ bst" ['a tree -> 'a option] vrne naslednjika korena drevesa,
   če obstaja. Torej za drevo oblike bst = Node(l, x, r) vrne najmanjši element
   drevesa, ki je večji od x.
   Funkcija "pred bst" ['a tree -> 'a option] simetrično vrne največji element,
   ki je manjši od korena, če obstaja.
   ----------
   # succ test_tree;;
   - : int option = Some 6
   # pred (Node(Empty, 5, leaf 7));;
   - : int option = None
   ---------- *)

let succ (bst : 'a tree) : 'a option = 
  let rec find_min = function
    | Empty -> None
    | Node (Empty, x, _) -> Some x
    | Node (l, _, _) -> find_min l
  in 
    match bst with
    | Empty -> None
    | Node(_, _, d) -> find_min d 


let pred (bst : 'a tree) : 'a option = 
  let rec find_max = function
    | Empty -> None
    | Node(_, x, Empty) -> Some x
    | Node(_, _, d) -> find_max d
  in
    match bst with
    | Empty -> None
    | Node(l, x, d) -> find_max l



(* Na predavanjih ste omenili dva načina brisanja elementov iz drevesa.
   Prvi uporablja "succ", drugi pa "pred".
   Napiši funkcijo "delete x bst" ['a tree -> 'a tree], ki iz drevesa izbriše
   element x, če ta v drevesu obstaja. Za vajo lahko implementiraš obe verziji
   brisanja elementov.
   ----------
   [Za delete definiran s funkcijo "succ".]
   # delete 7 test_tree;;
   - : int tree =
   Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
   Node (Node (Empty, 6, Empty), 11, Empty)
   ---------- *)

let rec delete x bst : 'a tree = 
  match bst with
  | Empty -> Empty
  | Node(l, y, d) as t -> if y < x then Node(l, y, delete x d) 
                          else if y > x then Node(delete x l, y, d) 
                          else
                          match succ t with
                          | None -> l
                          | Some s -> let clean_d = delete s d in Node(l, s, clean_d)



let rec delete1 x bst : 'a tree = 
  match bst with
  | Empty -> Empty
  | Node(l, y, d) as t -> if y < x then Node(l, y, delete1 x d) 
                          else if y > x then Node(delete1 x l, y, d) 
                          else
                          match pred t with
                          | None -> d
                          | Some s -> let clean_l = delete1 s l in Node(clean_l, s, d)


(* Dodatna možnost je, da spremenimo tip s katerim predstaviljamo drevo.
   Definiraj nov tip drevesa, ki poleg podatka, levega in desnega poddrevesa
   hrani še dodatno informacijo o stanju "state", ki je lahko "Exists" če
   vozlišče upoštevamo in pa "Ghost" če je vozlišče v drevesu le še delitveno. *)

type state = Exists | Ghost

type 'a phantom_tree = 
  | P_Empty
  | P_Node of 'a phantom_tree * 'a * 'a phantom_tree * state


(* Funkcija "phantomize t" ['a tree -> 'a phantom_tree], ki navadnemu drevesu
   priredi fantomsko drevo.
   Nato napiši funkcijo "kill x pt" ['a -> 'a phantom_tree -> 'a phantom_tree],
   ki izbriše element v drevesu tako, da njegovo stanje nastavi na Ghost.
   ----------
   # phantomize test_tree;;
   - : int phantom_tree =
   P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
   P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
   P_Node (P_Empty, 11, P_Empty, Exists), Exists),
   Exists)
   # bst_of_list [3;4;2] |> phantomize |> kill 3 |> kill 6;;
   - : int phantom_tree =
   P_Node (P_Empty, 2,
   P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
   ---------- *)

let rec phantomize (t : 'a tree) : 'a phantom_tree = 
  match t with
  | Empty -> P_Empty
  | Node(l, x, d) -> let p_l = phantomize l in let p_d = phantomize d 
      in P_Node(p_l, x, p_d, Exists)

let rec kill x = function
  | P_Empty -> P_Empty
  | P_Node(l, y, d, s) -> if y < x then P_Node(l, y, kill x d, s) 
                          else if y > x then P_Node(kill x l, y, d, s) 
                        else P_Node (l, y, d, Ghost)


(* Funkcija "unphantomize pt" ['a phantom_tree -> 'a tree], ki fantomskemu
   drevesu priredi navadno drevo, ki vsebuje le vozlišča, ki še obstajajo.
   Vrstni red vozlišč v končnem drevesu ni pomemben.
   Namig: lahko si pomagaš z vmesnim prehodom na drugo podatkovno strukturo.
   ----------
   # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
   - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
   ---------- *)

let unphantomize t = 
  let rec list_of_phantom_tree = function
  | P_Empty -> []
  | P_Node(l, x, d, Ghost) -> (list_of_phantom_tree l) @ (list_of_phantom_tree d) 
  | P_Node(l, x, d, Exists) -> (list_of_phantom_tree l) @ [x] @ (list_of_phantom_tree d) 
in t |> list_of_phantom_tree |> bst_of_list

(*========== Ideas for additional exercises ==========*)
(*
1.) Change the functions "insert" and "member" to work with phantom trees.
2.) Create a more general decision tree, that additionally contains a
    decision function in every node that decides in which subtree an element
    belongs. [Such trees are used in machine learning, where comparing vectors
    only uses a chosen component of the vector.]
3.) By using exercise 2 you can define dictionaries based on trees, where a pair
    (data, key) is saved according to key value. Define a new type of nodes
    that keeps the value of data and an additional key and define some of the
    function "member", "insert", "dict_of_list", ... so that they coincide with
    the expected behaviour on dictionaries.
*)
