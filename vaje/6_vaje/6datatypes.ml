(* The wizard wars are raging. *)

(* The wizards combating in this war are of these races.  *)

type race = Orc | Hobbit | Human

(* There are spells of three schools of magic: firewall and blaze are fire
   spells, resurrect and cripple are of the necrotic school, and renounce and
   banish are angelic.

   Define a type to represent the schools, and a type of spells.
*)

type school = Fire | Necrotic | Angelic


type spell = Firewall | Blaze | Resurrect | Cripple | Renounce | Banish

(* The skills a wizard has mastered are the list of spells he can cast in one
   round. Define a type `skills'. *)

type mana = int
type health = int

(* A wizard has to be given a name, a number of hitpoints (hp), an ability
   level of mana, a race, and the skills he has mastered. Use a record to
   represent this. *)
   
type wizard = {name : string; hp : health; ability : mana; race : race; skills : spell list}

let string_of_spell = function
  | Blaze -> "Blaze"
  | Firewall -> "Firewall"
  | Renounce -> "Renounce"
  | Banish -> "Banish"
  | Resurrect -> "Resurrect"
  | Cripple -> "Cripple"


(* Write a function that indicates for each spell which school it belongs to. *)

let school_of_spell = function
  | Firewall | Blaze -> Fire
  | Resurrect | Cripple -> Necrotic
  | Renounce | Banish -> Angelic


(* Write a function that computes the mana each spell uses. The values are:
blaze : 420
firewall : 35
renounce : 17
banish : 103
resurrect : 178
cripple : 250
Hint: use regex-replace in Notepad++
 *)

let mana_of_spell = function
  | Blaze -> 420
  | Firewall -> 35
  | Renounce ->  17
  | Banish -> 103
  | Resurrect -> 178
  | Cripple -> 250

(* Use regex-replace in Notepad++ to build a few example wizards, like merlin *)
(*
name : "Frodo",      ability : 53,   hp : 1000,  skills : [Renounce],                      race : Hobbit
name : "Ajitam",     ability : 1337, hp : 7331,  skills : [Firewall; Resurrect; Firewall], race : Hobbit
name : "Mr Duck",    ability : 7,    hp : 90000, skills : [Cripple],                       race : Orc
name : "Kylo Ren",   ability : 589,  hp : 90,    skills : [Resurrect],                     race : Human
name : "Snoop Dogg", ability : 420,  hp : 4000,  skills : [Blaze],                         race : Orc
*)

let merlin = {name = "Merlin";   ability = 1832; hp = 9001; skills = [Renounce; Banish];  race = Human}
let frodo =  {name = "Frodo"; ability = 53; hp = 1000; skills = [Renounce];  race = Hobbit}
let ajitam = {name = "Ajitam"; ability = 1337; hp = 7331; skills = [Firewall; Resurrect; Firewall]; race = Hobbit}
let mrDuck = {name = "Mr Duck"; ability = 7; hp = 90000; skills = [Cripple]; race = Orc}
let kyloReN = {name = "Kylo Ren"; ability = 589; hp = 90; skills = [Resurrect]; race = Human}
let snoop_dogg = {name = "Snoop Dogg"; ability = 420; hp = 4000; skills = [Blaze]; race = Orc}


(* Write a function that computes the wizard with the most mana. *)

let rec strongest_wizard (wizards : wizard list) : wizard option =
  match wizards with
  | [] -> None
  | x::xs -> 
    begin 
    	match strongest_wizard xs with
      	| None -> Some x
      	| Some x' -> if x.ability >= x'.ability then Some x
                  else Some x'
    end


(* Posploši funkcijo strongest_wizard na funkcijo max_list, ki sprejme seznam
   in dodatno funkcijo dveh elementov max : 'a -> 'a -> 'a in vrne maksimalni element seznama
   glede na funkcijo max.
*)

let rec max_list (xs : 'a list) (max : 'a -> 'a -> 'a) : 'a option = 
	match xs with
	| [] -> None
	| x::xs -> 
		begin
			match max_list xs max with
			| None -> Some x
			| Some y -> Some (max x y)
		end



(* Rase imajo različno občutljivost [vulnerability] na določene šole magije.
   Napiši tip s katerim lahko izraziš kdaj ima rasa visoko [High], navadno [Normal]
   ali pa nizko [Low] občutljivost na urok. *)

type vulnerability = 
	| High
	| Normal
	| Low 


(* Napiši funkcijo, ki glede na šolo uroka in raso izračuna občutljivost.
   Low za:     orcs:necrotic, hobbits:fire, humans:angelic,
   High za:    hobbit:necrotic, human:fire, orc:angelic
   Sicer vrne Normal
*)

let effectiveness (school : school) (race : race) : vulnerability = 
	match school, race with
	| Fire, Hobbit | Necrotic, Orc | Angelic, Human -> Low
	| Necrotic, Hobbit | Fire, Human | Angelic, Orc -> High
	| _, _ -> Normal



(* Zapiši funkcijo, ki za čarodeja izračuna njegovo občutljivost na podani urok. *)

let vulnerable spell wizard = effectiveness (school_of_spell spell) wizard.race


(* Občutljivost se v boju izrazi kot koeficient škode, ki jo utrpi čarodej, če ga urok zadane.
   Zapiši funkcijo, ki glede na občutljivost vrne primeren koeficient, tako da čarodej z nizko
   občutljivostjo utrpi le pol škode, čarodej z visoko občutljivostjo pa dvakratnik.*)

let coef = function
	| Low -> 0.5
	| High -> 2.
	| Normal -> 1.

(* Vsak urok naredi toliko škode, kot je potrebnih točk mane za izvršitev uroka.
   Napiši funkcijo, ki glede na urok in čarodeja izračuna koliko škode utrpi,
   če ga urok zadane.
   Namig: za pretvarjanje med int in float se uporabljata funkciji float_of_int in
   int_of_float.
*)

let damage_caused spell target =
	int_of_float (coef (vulnerable spell target) *. (float_of_int(mana_of_spell(spell))))
		

(* Zapiši funkcijo, ki vrne novo stanje čarodeja (z znižanimi življenskimi točkami [hp]),
   po tem, ko ga je zadel izbrani urok.
   (Novo stanje čarodeja je prav tako tipa wizard)
*)

let attack wizard spell = {wizard with hp = wizard.hp - damage_caused spell wizard}


(* Napiši funkcijo, ki za danega čarovnika izvršuje uroke, dokler ne izvede vseh urokov
   na seznamu, ali pa mu zmanjka točk mane. *)

let cast_spells (caster : wizard)  = 
	let m = caster.ability in
	let (available, spells_cast) =
		List.fold_left 
			(fun (available, spells_cast) spell -> 
				let cost = mana_of_spell spell in 
					if cost < available then (available - cost, spell :: spells_cast)
					else (available, spells_cast)) 
			(m, [])
			caster.skills
		in ({caster with ability = available}, spells_cast)


(* Write a function that stands off two wizard in a duel. If the attacker is
   dead, the defender wins. The attacker, if he is still alive, casts his
   spells. If he cannot cast any spells, he loses. After the attacker casts
   his spells, the roles change and the defender takes his turn to attack. *)


let rec duel (attacker : wizard) (defender : wizard) : wizard =
	let defender = 
		if attacker.hp > 0 then 
			begin
				let (attacker, spells) = cast_spells attacker 
				in
				match spells with
				| [] -> defender
				| _ -> let defender	= List.fold_left attack defender spells in
					duel defender attacker
			end
		else
			defender
		in defender
		 

let _ = duel frodo snoop_dogg
