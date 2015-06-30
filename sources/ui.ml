class ui (pet: Pet.pet) =
  object (self)

    val mutable window          = GWindow.window ()
    val mutable dialog          = GWindow.dialog ()
    val mutable vbox            = GPack.vbox ()
    val mutable sbox            = GPack.hbox ()
    val mutable pbox            = GPack.hbox ()
    val mutable stat_health     = GRange.progress_bar ()
    val mutable stat_energy     = GRange.progress_bar ()
    val mutable stat_hygiene    = GRange.progress_bar ()
    val mutable stat_happy      = GRange.progress_bar ()
    val mutable petImage        = GMisc.image ()
    val mutable _action         = false


    method init =
      ignore (GtkMain.Main.init ());

      window <- GWindow.window
                ~border_width: 10
                ~width: 250
                ~height: 200
                ~title: "Instant Tama"
                ~resizable: false
                ~position: `CENTER ();

      dialog <- GWindow.dialog
                ~parent: window
                ~destroy_with_parent: true
                ~border_width: 10
                ~width: 250
                ~height: 100
                ~modal: true
                ~show: false
                ~resizable: false ();

      vbox <- GPack.vbox ~packing:window#add ();
      sbox <- GPack.hbox ~packing:vbox#add ();
      pbox <- GPack.hbox ~height:100 ~width:250 ~packing:vbox#add ();

      let continueButton = GButton.button ~label:"REVIVES" ~packing:dialog#action_area#add () in
      let exitButton = GButton.button ~label:"QUIT" ~packing:dialog#action_area#add () in
      ignore (GMisc.label ~text:"You killed your tama !!!" ~packing:dialog#vbox#add ());
      ignore (continueButton#connect#clicked ~callback:(fun () -> pet#regeneration; ignore(dialog#misc#hide ())));
      ignore (exitButton#connect#clicked ~callback:self#destroy);

      self#createStatBar ;
      self#createPetZone ;
      self#createActionButtons ;

      (* event loop *)
      ignore (GMain.Timeout.add ~ms:1000 ~callback:(self#loopHandler));

      ignore (window#connect#destroy ~callback:self#destroy);
      window#show();
      GMain.Main.main()


    method createStatBar =

      stat_health  <- GRange.progress_bar ~packing:sbox#add ();
      stat_energy  <- GRange.progress_bar ~packing:sbox#add ();
      stat_hygiene <- GRange.progress_bar ~packing:sbox#add ();
      stat_happy   <- GRange.progress_bar ~packing:sbox#add ();

      stat_health#set_text  "HEALTH";
      stat_energy#set_text  "ENERGY";
      stat_hygiene#set_text "HYGIENE";
      stat_happy#set_text   "HAPPY";

      stat_health#set_fraction ((float_of_int pet#get_health) /. 100.);
      stat_energy#set_fraction ((float_of_int pet#get_energy) /. 100.);
      stat_hygiene#set_fraction ((float_of_int pet#get_hygiene) /. 100.);
      stat_happy#set_fraction ((float_of_int pet#get_happy) /. 100.);


    method createActionButtons =

      let bbox = GPack.hbox ~packing:vbox#add () in
      let button_eat = GButton.button ~label:"EAT" ~packing:bbox#add () in
      let button_thunder = GButton.button ~label:"THUNDER" ~packing:bbox#add () in
      let button_bath = GButton.button ~label:"BATH" ~packing:bbox#add () in
      let button_kill = GButton.button ~label: "KILL" ~packing:bbox#add () in

      let actionCallback action (actionName: string) = fun () -> if _action = false then
        begin
          _action <- true;
          self#anim [button_eat; button_thunder; button_bath; button_kill];
          action();
          self#drawImage (pet#get_sprite actionName);
          self#refresh
        end
      in
      ignore (button_eat#connect#clicked     ~callback: (actionCallback pet#eat "eat"));
      ignore (button_thunder#connect#clicked ~callback: (actionCallback pet#thunder "thunder"));
      ignore (button_bath#connect#clicked    ~callback: (actionCallback pet#bath "bath"));
      ignore (button_kill#connect#clicked    ~callback: (actionCallback pet#kill "kill"));


    method anim btns =
      ignore (GMain.Timeout.add ~ms:3000 ~callback:(self#end_anim btns));

      let rec loop = function
        | []    -> ()
        | hd::tl  -> hd#misc#set_sensitive false; loop tl
      in loop btns


    method end_anim btns () =
      self#drawImage (pet#get_sprite "current");
      _action <- false;

      let rec loop = function
        | []    -> false
        | hd::tl  -> hd#misc#set_sensitive true; loop tl
      in loop btns


    method createPetZone =
      petImage <- GMisc.image ~file:"sprites/current.gif" ~packing:pbox#add  ~show:true ()


    method drawImage link =
      petImage#clear ();
      petImage <- GMisc.image ~file:link ~packing:pbox#add  ~show:true ()


    method refresh =
      if pet#is_dead then
        self#gameover ()
      else
        begin
            stat_health#set_fraction  ((float_of_int pet#get_health)  /. 100.);
            stat_energy#set_fraction  ((float_of_int pet#get_energy)  /. 100.);
            stat_hygiene#set_fraction ((float_of_int pet#get_hygiene) /. 100.);
            stat_happy#set_fraction   ((float_of_int pet#get_happy)   /. 100.);
        end


    method gameover () =
      ignore (dialog#show ());


    method loopHandler () =
      pet#decr;
      self#refresh;
      true


    method destroy () =
      pet#save;
      GMain.Main.quit ()

  end
