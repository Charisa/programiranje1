(* A priority queue (PQ) is a data structure that stores elements in order of
   "priority". Elements with high priority are available before elements with
   low priority. *)
type comparison = LT | EQ | GT

(* To talk about priorities, we define a type of comparisons, and a signature
   of types with comparison.

   Note that we do not want to hide the implementation of the type t here
   because we want to use the compare function on values of type t *outside the
   module*. The reason that we leave it abstract here is because it is simply
   unknown at this time. *)

module type Comparable = sig
    type t
    val compare : t -> t -> comparison
  end

(* Implement a module that can compare integers. *)


module Cmp_Int = struct
  type t = int
  let compare x y = if x < y then LT else if x = y then EQ else GT
end
 
(* let _ = Cmp_Int.compare 9000 42;; *)


(* Ko modulu predpišemo signaturo, je potrebno paziti katere tipe naredimo
   abstraktne. Če je tip abstrakten v signaturi, ga lahko kljub temu
   'razkrijemo' s pomočjo "with type t = int", ko to potrebujemo. 
   Primer:
   module Cmp_Int_prescribed = (Cmp_Int : Comparable with type t = int) *)

(* Primer uporabe: *)
(* let _ = Cmp_Int_prescribed.compare (-9000) 42;; *)





(* Sedaj napiši modul, ki primerja nize. Pri pisanju primerjalne funkcije
   si pomagaj s funkcijo "compare" iz vgrajenega modula "Pervasives".
   Funkcija Pervasives.compare s t vrne -1 če je s<t, 0 če s=t in 1 za s>t. *)


module Cmp_String = struct
  type t = string
  let compare x y = 
    match Pervasives.compare x y with
    | v when v < 0 -> LT
    | 0 -> EQ
    | _ -> GT
end

let _ = Cmp_String.compare "a" "b"


(* Funktor je preslikava iz modula v modul. Sedaj definiraj funktor,
   ki sprejme modul, ki ustreza Comparable signaturi in vrne nov Comparable
   modul na istem osnovnem tipu, vendar z obrnjeno funkcijo primerjanja. *)
   
(* Spodnja definicija uporabla oznake za tipe. Brez oznak bi bila zgolj
   "module Cmp_inv (Cmp)" vendar z oznako tipov povemo, da se tip končnega 
   modula ujema s tipom modula, ki ga podamo kot argument. *)


module Cmp_inv (Cmp : Comparable) : Comparable with type t = Cmp.t  = struct
  type t = Cmp.t
  let compare x y = match Cmp.compare x y with
  | LT -> GT
  | EQ -> EQ
  | GT -> LT
end


(* Funktor uporabljamo podobno kot funkcije, le da v tem primeru potrebujemo
   oklepaje okrog argumentov. *)


module Cmp_Int_inv = Cmp_inv (Cmp_Int)
let _ = Cmp_Int.compare (-9000) 42;;
let _ = Cmp_Int_inv.compare (-9000) 42;;


(* Sedaj napišemo signaturo kopice [heap] (oz. prioritetne vrste). Imamo tip kopice,
   ki ga označimo s "h", tip elementov "el", prazno kopico in pa operacijo "push", ki na
   kopico doda element in operacijo "pop", ki iz kopice odvzame prvi element po prioriteti. 
   Ker je kopica lahko prazna, nam "pop" vrne opcijski tip. *)


module type Heap = sig
    type h
    type el
    val empty : h
    val pop : h -> (h * el) option
    val push : h -> el -> h
  end


(* Kopico bomo implementirali kot urejen seznam. Napiši funktor, ki sprejme Comparable
   modul in s pomočjo primerjave v tem modulu naredi kopico preko urejenega seznama.
   Seznam vsebuje elementa tipa Cmp.t *)


module Sorted_List_Heap ... = struct
  ...
end






(* Apply your functor to build a priority queue of integers, and a priority queue of strings. *)


(* Write some examples using push and pop! *)


(* Write a functor To_List that takes an implementation of Priority_Queue as an argument
   and returns a module with a "to_list" function, that takes a priority queue and yields
   all of its elements as a list. *)

(* module To_List ... *)


(* Let's test your To_List functor! Like any other values, modules can also be
   used locally. We just have to distinguish the let binding with an additional
   "module" keyword. Short variable and module names at toplevel are a mistake
   but are acceptable locally when we don't have to remember what they mean for
   too long. *)
(*
let _ =
  let h = List.fold_left IntH.push IntH.empty [1; 0; 9; 2] in
  let module TL = To_List(IntH) in
  TL.to_list h

let _ =
  let module H = Sorted_List_Priority_Queue (Cmp_inv(Cmp_Int)) in
  let module L = To_List(H) in
  let h = List.fold_left H.push H.empty [1; 0; 9; 2] in
  L.to_list h
 *)
