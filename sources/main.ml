(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gchateau <gchateau@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2015/06/29 00:28:26 by gchateau          #+#    #+#             *)
(*   Updated: 2015/06/29 15:52:51 by gchateau         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let main () =
  ignore (GtkMain.Main.init ());
  let window = GWindow.window ~border_width:10
							  ~width:300
							  ~height:200
							  ~title:"Instant Tama"
							  ~resizable:false
							  ~position:`CENTER ()
  in
  let container = GPack.vbox ~packing:window#add () in
  let barBox = GPack.hbox ~packing:container#add () in
  let petBox = GPack.hbox ~packing:container#add ~height:100 ~width:250 () in
  let btnBox = GPack.hbox ~packing:container#add () in
  let ui = new Game.ui window barBox petBox btnBox in
	ui#init#run ()

(* ************************************************************************** *)
let () = main ()
