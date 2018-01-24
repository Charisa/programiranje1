
(* ===== Vaja: Dinamično programiranje  ===== *)

(* Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
   samo za eno polje navzdol ali za eno polje na desno in na koncu prispeti v
   desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
   različne (pozitivne) mase. Miška bi se rada kar se da nažrla, zato jo zanima,
   katero pot naj ubere.

   Napišite funkcijo "max_cheese cheese_matrix", ki dobi matriko z
   masami sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
   optimalni poti.

   ----------
   # max_cheese cheese_matrix;;
   - : int = 13
   ----------*)

let test_matrix = [| [| 1 ; 2 ; 0 |];
                     [| 2 ; 4 ; 5 |];
                     [| 7 ; 0 ; 8 |]  |]


let max_cheese cost = 
   let n = Array.length cost in
   let m = Array.length cost.(0) in 
   let rec najboljsa_pot x y =
      let trenutno_polje = cost.(x).(y) in
      let desno = if (x + 1) = n then 0 
                  else najboljsa_pot (x + 1) y in 
      let dol = if (y + 1) = m then 0
                  else najboljsa_pot x (y + 1) in 
      trenutno_polje + max desno dol;
   in
   najboljsa_pot 0 0 

let min_cheese cost = 
   let n = Array.length cost - 1 in
   let m = Array.length cost.(0) -1 in  
   let rec najboljsa_pot x y =
      match x, y with
      | 0, 0 -> cost.(x).(y)
      | 0, y when y > 0 -> cost.(x).(y) + najboljsa_pot x (y-1)
      | x, 0 when x > 0 -> cost.(x).(y) + najboljsa_pot (x-1) y
      | _, _ -> cost.(x).(y) + min (najboljsa_pot (x-1) y) (najboljsa_pot x (y-1))
   in
   najboljsa_pot n m 


(* Rešujemo problem stolpov, ko smo ga spoznali na predavanjih.
   Imamo štiri različne tipe gradnikov, dva modra in dva rdeča.
   Modri gradniki so višin 2 in 3, rdeči pa višin 1 in 2.

   Napiši funkcijo "alternating_towers height", ki za podano višino "height"
   izpiše število različnih stolpov podane višine, kjer se barva gradnikov
   izmenjuje (rdeč na modrem, moder na rdečem itd.).

   Namig: Uporabi dve pomožni funkciji. Za medsebojno rekurzijo uporabi
          ukaz "and".
   ----------
   # alternating_towers 10;;
   - : int = 35
   ---------- *)


let alternating_towers height =
  let rec redtop height =
    if height <= 0 then 0
    else if height <= 2 then 1
    else
      bluetop (height-1) + bluetop (height-2)
  and bluetop height =
    if height <= 0 then 0
    else if height = 2 then 1
    else if height = 3 then 2
    else
      redtop (height-2) + redtop (height-3)
  in
  redtop height + bluetop height








