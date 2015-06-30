(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   game.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gchateau <gchateau@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/29 00:30:54 by gchateau          #+#    #+#             *)
(*   Updated: 2015/06/29 19:19:15 by gchateau         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Save =
  struct
	let default_filename = "save.itama"

	let recover filename =
	  try
		let chan = open_in filename in
		let rec loop result =
		  try
			let input = input_line chan in
			  loop (result ^ input ^ ";")
		  with
			| End_of_file	-> close_in chan; result ^ ";"
		in loop ""
	  with
		| Sys_error msg	-> prerr_endline ("\nError while recovering game datas:\n" ^ msg); ""

	let save datas filename =
	  try
		let chan = open_out filename in
		  flush chan;
		  output_string chan (datas ^ "\n");
		  close_out chan
	  with
		| Sys_error msg	-> prerr_endline ("\nError while saving game datas:\n" ^ msg)
  end

(* ************************************************************************** *)
(*                                     UI                                     *)
(* ************************************************************************** *)

class ui window bar_box pet_box btn_box =
  object (self : 'self)
	val mutable _pet:Pet.pet = ((new Pet.pet)#from_string (Save.recover Save.default_filename))
    val _window			= window
    val _dialog			= GWindow.dialog ~parent:window ~destroy_with_parent:true ~modal:true ~show:false ~resizable:false ~height:100 ~title:"Game Over !!" ()
    val _health_bar		= GRange.progress_bar ~packing:bar_box#add ()
    val _energy_bar     = GRange.progress_bar ~packing:bar_box#add ()
    val _hygiene_bar    = GRange.progress_bar ~packing:bar_box#add ()
    val _happy_bar      = GRange.progress_bar ~packing:bar_box#add ()
    val _eat_btn		= GButton.button ~label:"EAT" ~packing:btn_box#add ()
    val _thunder_btn	= GButton.button ~label:"THUNDER" ~packing:btn_box#add ()
	val _bath_btn		= GButton.button ~label:"BATH" ~packing:btn_box#add ()
	val _kill_btn		= GButton.button ~label: "KILL" ~packing:btn_box#add ()
	val _pet_img		= GMisc.image ~file:"sprites/current.gif" ~packing:pet_box#add ~show:true ()

    method init =
      let continueButton = GButton.button ~label:"REVIVES" ~packing:_dialog#action_area#add () in
      let exitButton = GButton.button ~label:"QUIT" ~packing:_dialog#action_area#add () in
		ignore (GMisc.label ~text:"You killed your tama !!!" ~packing:_dialog#vbox#add ());
		ignore (continueButton#connect#clicked ~callback:self#retry);
		ignore (exitButton#connect#clicked ~callback:self#destroy);

		_health_bar#set_text  "HEALTH";
		_energy_bar#set_text  "ENERGY";
		_hygiene_bar#set_text "HYGIENE";
		_happy_bar#set_text   "HAPPY";
		self#refresh;

		(* button callbacks *)
		ignore (_eat_btn#connect#clicked     ~callback: (self#callback "eat"));
		ignore (_thunder_btn#connect#clicked ~callback: (self#callback "thunder"));
		ignore (_bath_btn#connect#clicked    ~callback: (self#callback "bath"));
		ignore (_kill_btn#connect#clicked    ~callback: (self#callback "kill"));

		(* event loop *)
		ignore (_window#connect#destroy ~callback:self#destroy);
		ignore (_dialog#connect#destroy ~callback:self#destroy);
		self

	method timer () = ignore (GMain.Timeout.add ~ms:1000 ~callback:self#callback_timer)

	method run () =
	  _window#show();
	  self#timer ();
	  GMain.Main.main ()

    method destroy () =
	  Save.save _pet#to_string Save.default_filename;
      GMain.Main.quit ()

	method gameover () =
      ignore (_dialog#show ())

	method retry () =
	  _pet <- _pet#regenerate;
	  self#timer ();
	  ignore (_dialog#misc#hide ())

	method callback action () =
	  let (pair:Pet.pet * string) = _pet#call action in
		_pet <- (fst pair);
		_pet_img#set_file (snd pair);
		self#refresh;
		self#animate [_eat_btn; _thunder_btn; _bath_btn; _kill_btn]

	method callback_timer () =
	  _pet <- _pet#decr;
	  self#refresh;
	  _pet#is_alive

	method private refresh =
	  let bars = [
		(_health_bar, _pet#get_health);
		(_energy_bar, _pet#get_energy);
		(_hygiene_bar, _pet#get_energy);
		(_happy_bar, _pet#get_happy)
	  ] in
	  let rec loop = function
		| []		-> ()
		| hd::tl	-> (fst hd)#set_fraction (self#fraction_of (snd hd)); loop tl
	  in
		loop bars;
		if _pet#is_dead then self#gameover ()

    method private animate btns =
	  let stop () =
		_pet_img#set_file (snd (_pet#call "current"));
		let rec loop = function
          | []		-> false
          | hd::tl	-> hd#misc#set_sensitive true; loop tl
		in loop btns
	  in
      ignore (GMain.Timeout.add ~ms:3000 ~callback:stop);
      let rec loop = function
        | []		-> ()
        | hd::tl	-> hd#misc#set_sensitive false; loop tl
      in loop btns

	method private fraction_of n = ((float_of_int n) /. 100.)

  end

(* ************************************************************************** *)
