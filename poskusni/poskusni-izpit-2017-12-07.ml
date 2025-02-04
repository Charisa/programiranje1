(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame dve celi števili ter vrne njuno vsoto.
   Primer: /sestej 2 3 = 5/ *)

   let sestej a b = a + b

(* 1.2) Definirajte funkcijo, ki svojemu argumentu prišteje 3.
   Primer: /pristej_tri 10 = 13/ *)

   let pristej_tri n = n + 3

(* 1.3) Definirajte funkcijo, ki vsem elementom seznama prišteje 5.
   Primer: /vsem_pristej_pet [1; 2] = [6; 7]/ *)

   let vsem_pristej_pet list = 
	let rec aux acc = function
		| [] -> acc
		| x::xs -> aux ((x+5)::acc) xs
	in List.rev(aux [] list)

(* 1.4) Definirajte funkcijo, ki vrne zadnjo komponento nabora s tremi elementi.
   Primer: /tretji (1, "horse", [None]) = [None]/ *)

   let tretji (_,_,a) = a

(* 1.5) Definirajte funkcijo, ki vzame dve funkciji ter vrne njun kompozitum.
   Primer: /kompozitum succ string_of_int 5 = "6"/ *)

   let kompozitum f g a =  g ( f a)


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)


(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)

	type 'a drevo = T of 'a * 'a drevo list

   let test_tree1 = T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])]);;
   let test_tree2 = T(2, [T(-3,[T(1,[])]); T(2,[]); T(-8,[T(4,[]); T(5,[])])]);;

(* 2.2) Napišite funkcijo, ki vrne koren danega rožnega drevesa. *)

	let koren = function 
      T(x, _) -> x

(* 2.3) Napišite funkcijo, ki preveri, ali drevo celih števil vsebuje kakšno negativno število. *)

   let vsa_vozlisca drevo = 
      let rec aux acc = function
         | T(x, y) -> aux (x::acc) 
      in aux [] drevo




   let kaksno_negativno = failwith "dopolni me"




(* 2.4) Sestavite funkcijo, ki sprejme naravno število ter sestavi (poljubno)
   drevo, ki ima toliko otrok.
   Namig: napišite pomožno funkcijo, ki ustvari poljuben seznam dane dolžine. *)
let drevo_z_veliko_otroci = failwith "dopolni me"

(* 2.5) Sestavite funkcijo, ki izračuna število vseh vozlišč v drevesu.
   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let velikost = failwith "dopolni me"
