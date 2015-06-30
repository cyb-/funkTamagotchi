(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   pet.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gchateau <gchateau@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/29 02:58:45 by gchateau          #+#    #+#             *)
(*   Updated: 2015/06/29 20:41:46 by gchateau         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class pet =
  object (self : 'self)
	val _health:int  = 0
	val _energy:int  = 0
	val _hygiene:int = 0
	val _happy:int   = 0

	method get_health = _health
	method get_energy = _energy
	method get_hygiene = _hygiene
	method get_happy = _happy

	method set_health x = {< _health = (self#add 0 x); _energy = _energy; _hygiene = _hygiene; _happy = _happy >}
	method set_energy x = {< _health = _health; _energy = (self#add 0 x); _hygiene = _hygiene; _happy = _happy >}
	method set_hygiene x = {< _health = _health; _energy = _energy; _hygiene = (self#add 0 x); _happy = _happy >}
	method set_happy x = {< _health = _health; _energy = _energy; _hygiene = _hygiene; _happy = (self#add 0 x) >}

	method is_dead = (_health = 0 || _energy = 0 || _hygiene = 0 || _happy = 0)

	method is_alive = (not self#is_dead)

	method from_string str : 'self =
	  let len = String.length str in
	  let recovered =
		let rec loop i (result:'self) =
		  let setVal n = function
			| "health"	-> result#set_health n
			| "energy"	-> result#set_energy n
			| "hygiene"	-> result#set_hygiene n
			| "happy"	-> result#set_happy n
			| _			-> result
		  in
		  let rec getVal i key s =
			if i < len then match String.get str i with
			  | '0' .. '9'	-> getVal (i + 1) key (s ^ (String.sub str i 1))
			  | ' ' | '\t'	-> getVal (i + 1) key s
			  | ';' | '\n'	-> loop (i + 1) (setVal (int_of_string s) key) 
			  | _			-> loop (i + 1) result
			else result
		  in
		  let rec getKey i s =
			if i < len then match String.get str i with
			  | 'a' .. 'z' | 'A' .. 'Z' | '_'	-> getKey (i + 1) (s ^ (String.sub str i 1))
			  | ' ' | '\t'						-> getKey (i + 1) s
			  | ':'								-> getVal (i + 1) s ""
			  | ';' | '\n'						-> loop (i + 1) result
			  | _								-> result
			else result
		  in
			getKey i ""
		in
		  loop 0 {< _health = 0; _energy = 0; _hygiene = 0; _happy = 0 >}
	  in
		if recovered#is_dead then self#regenerate
		else recovered

	method to_string =
	  let values = [
		("health", _health);
		("energy", _energy);
		("hygiene", _hygiene);
		("happy", _happy)
	  ] in
	  let concat a b = a ^ ":" ^ b in
	  let rec loop result = function
		| []				-> result
		| [x]				-> result ^ (concat (fst x) (string_of_int (snd x)))
		| hd::(x::_ as tl)	-> loop (result ^ (concat (fst hd) (string_of_int (snd hd))) ^ ";") tl
	  in
		loop "" values

	method decr =
      {< _health = (self#sub _health 1); _energy = _energy; _hygiene = _hygiene; _happy = _happy >}

	method regenerate =
	  {< _health = 100; _energy = 100; _hygiene = 100; _happy = 100 >}

	method call = function
	  | "eat"		-> (self#eat, "sprites/eat.gif")
	  | "thunder"	-> (self#thunder, "sprites/thunder.gif")
	  | "bath"		-> (self#bath, "sprites/bath.gif")
	  | "kill"		-> (self#kill, "sprites/kill.gif")
	  | _			-> (self, "sprites/current.gif")

	method private eat =
	  {< _health = (self#add _health 25); _energy = (self#sub _energy 10); _hygiene = (self#sub _hygiene 20); _happy = (self#add _happy 5) >}
		
	method private thunder =
	  {< _health = (self#sub _health 20); _energy = (self#add _energy 25); _hygiene = _hygiene; _happy = (self#sub _happy 20) >}

	method private bath =
	  {< _health = (self#sub _health 20); _energy = (self#sub _energy 10); _hygiene = (self#add _hygiene 25); _happy = (self#add _happy 5) >}

	method private kill =
	  {< _health = (self#sub _health 20); _energy = (self#sub _energy 10); _hygiene = _hygiene; _happy = (self#add _happy 20) >}

	method private add a b =
      if a + b > 100 then 100
      else a + b

	method private sub a b =
      if a - b < 0 then 0
      else a - b

  end

(* ************************************************************************** *)
