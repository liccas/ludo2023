open UniverseJsLocal
open World
open ImageFloat
open Color
open Position

(* operation_t : 手動か自動(CPU)かを表す型 *)
type operation_t = Hum   (* 手動 *)
                 | Cpu   (* 自動(CPU) *)

(* piece_t : 駒を表す型 *)
type piece_t = int   (* 相対位置 *)
             * int   (* 移動先がいくつ先か *)

(* player_t : プレイヤーのステータスを表す型 *)
type player_t = {
  number : int;              (* プレイヤー番号 *)
  operation : operation_t;   (* 手動or自動 *)
  pieces : piece_t list;     (* 駒のリスト *)
  prestart : int;            (* スタートしていない駒の数 *)
  home : int;                (* ゴールした駒の数 *)
}

(* phase_t : ゲームの進行状態を表す型 *)
type phase_t = Start          (* スタート画面 *)
             | DiceWaiting    (* サイコロ振り待ち *)
             | DiceRolling    (* サイコロのアニメーション *)
             | PieceSelecting (* 駒を選択 *)
             | PieceMoving    (* 駒を動かすアニメーション *)
             | Finish         (* 終了画面 *)

(* world_t : 世界を表す型 *)
type world_t =
  phase_t                           (* フェーズ *)
* int                               (* 現在プレイヤー番号 *)
* int                               (* サイコロの目(ないなら0) *)
* int                               (* 選択されている駒の絶対位置(ないなら-1) *)
* player_t list                     (* プレイヤーのステータスのリスト(4人分) *)
* (int * ((int * int) list)) list   (*  ブロック状態管理用リスト
                                        (絶対位置(キー), 
                                        (プレイヤー番号, 個数)のリスト のリスト)*)
* int                               (* tick数 *)
* string list                       (* メッセージのリスト *)
* bool                              (* ルールを表示するかどうか *)
* bool                              (* 操作方法を表示するかどうか *)
* (int * int)                       (* 手動の人数と自動の人数の組 *)


(* -----ユーティリティ----- *)
  
(* elementがn個入ったリストを返す *)
(* list_elements : 'a -> int -> 'a list *)
let rec list_elements element n =
  if n <= 0 then []
  else element :: list_elements element (n - 1)

(* n番目までの要素を含んだリストを返す *)
(* リストの最初の要素は0番目 *)
(* list_first_half : int -> 'a list -> 'a list *)
let rec list_first_half n lst = match lst with
    [] -> []
  | first :: rest ->
    if n = 0 then [first]
    else if n < 0 then []
    else first :: list_first_half (n - 1) rest

(* プレイヤー番号nのプレイヤーのみに関数fを適用したリストを返す *)
(* player_map : int -> (player_t -> 'a) -> player_t list -> player_t list *)
let player_map n f plst =
  let hojo player = match player with
      {number = num} ->
      if num = n then f player
      else player in
  List.map hojo plst

(* 与えられたプレイヤーがCPUかどうかを返す *)
(* is_cpu : player_t -> bool *)
let is_cpu player = match player with
    {operation = ope} -> ope = Cpu

(* プレイヤー番号numのプレイヤーを返す *)
(* return_player : int -> status_t list -> status_t *)
let return_player num plst = List.find (fun {number = num'} -> num' = num) plst

(* -----ユーティリティここまで----- *)


(* -----画像関係----- *)

let width       = 720.   (* プレイ画面の幅 *)
let height      = 720.   (* プレイ画面の高さ *)
let menu_width  = 200.   (* メニュー画面の幅 *)

let background  = read_image "images/background.png"      (* 背景画像 *)
                                  width height
let start_back  = read_image "images/start.png"           (* スタート画面の画像 *)
                                  width height

(* ルール *)
let rule_image  = read_image "images/rule.png" 650. 650.     

(* 操作方法 *)
let guide_image = read_image "images/guide.png" 650. 406.25
    
(* 更新履歴 *)
let version_image = read_image "images/version.png" menu_width height  

(* 駒の画像 *)
let pl_images = [|
  read_image "images/pl0_image.png" 50. 50.;   (* pl0 *)
  read_image "images/pl1_image.png" 50. 50.;
  read_image "images/pl2_image.png" 50. 50.;
  read_image "images/pl3_image.png" 50. 50.;
|]

(* 小さな駒の画像 *)
let pl_images2 = [|
  read_image "images/pl0_image.png" 25. 25.;   (* pl0 *)
  read_image "images/pl1_image.png" 25. 25.;
  read_image "images/pl2_image.png" 25. 25.;
  read_image "images/pl3_image.png" 25. 25.;
|]

(* サイコロの画像 *)
let dice_images = [|
  read_image "images/dice0.png" 50. 50.;
  read_image "images/dice1.png" 50. 50.;
  read_image "images/dice2.png" 50. 50.;
  read_image "images/dice3.png" 50. 50.;
  read_image "images/dice4.png" 50. 50.;
  read_image "images/dice5.png" 50. 50.;
  read_image "images/dice6.png" 50. 50.;
|]

(* -----画像関係ここまで----- *)


(* -----初期値関係----- *)

(* プレイヤー番号とoperationを受け取ったら、初期状態のプレイヤーを返す *)
(* init_player : int -> operation_t -> player_t *)
let init_player num ope = {
  number = num;
  operation = ope;
  pieces = [];
  prestart = 4;
  home = 0;
}

(* noplを受け取ったら、その人数の組み合わせが許可されているかどうかを返す *)
(* is_allowed : int * int -> bool *)
let is_allowed (noh, noc) =
  let nopl = noh + noc in
    noh >= 1 && (nopl = 2 || nopl = 4)

(* noplを受け取ったら、それに合わせて初期化したプレイヤーリストを返す *)
(* initialize_plst : int * int -> player_t list *)
let initialize_plst nopl = match nopl with
    (1, 1) -> [init_player 0 Hum; init_player 2 Cpu]
  | (1, 3) -> [init_player 0 Hum; init_player 1 Cpu;
               init_player 2 Cpu; init_player 3 Cpu]
  | (2, 0) -> [init_player 0 Hum; init_player 2 Hum]
  | (2, 2) -> [init_player 0 Hum; init_player 1 Cpu;
               init_player 2 Hum; init_player 3 Cpu]
  | (3, 1) -> [init_player 0 Hum; init_player 1 Hum;
               init_player 2 Hum; init_player 3 Cpu]
  | (4, 0) -> [init_player 0 Hum; init_player 1 Hum;
               init_player 2 Hum; init_player 3 Hum]
  (* デバッグ用 *)
  | (0, 4) -> [init_player 0 Cpu; init_player 1 Cpu;
               init_player 2 Cpu; init_player 3 Cpu]
  | _ -> assert false

(* worldの初期値 *)
let initial_world = (
  Start,   (* フェーズ *)
  0,       (* 現在プレイヤー番号 *)
  0,       (* サイコロの目(ないなら0) *)
  -1,      (* 選択されている駒の絶対位置(ないなら-1) *)
  [],      (* プレイヤーのステータスのリスト(最大4人分) *)
  [],      (* ブロック状態管理用リスト(絶対位置(キー)、
              (プレイヤー番号,個数)のリスト のリスト *)
  0,       (* tick数 *)
  [],      (* メッセージのリスト *)
  false,   (* ルールを表示するかどうか *)
  false,   (* 操作方法を表示するかどうか *)
  (1, 3)   (* 手動の人数と自動の人数の組 *)
)


(* 新しいブロックリストを作る *)
(* make_blst : player_t list -> (int * (int * int) list) list *)
let make_blst plst =
  let hojo2 blst player = match player with
      {number = num; pieces = pie} ->
      let hojo blst' (rel, _) =
        let ab = return_ab num rel in
        try
          let entrys = List.assoc ab blst' in
          let new_entrys =
            try
              let nop = List.assoc num entrys
              in (num, nop + 1) :: List.remove_assoc num entrys
            with Not_found -> (num, 1) :: entrys
          in (ab, new_entrys) :: List.remove_assoc ab blst'
        with Not_found -> (ab, [(num, 1)]) :: blst' in
      List.fold_left hojo blst pie in
  List.fold_left hojo2 [] plst

(* デバッグ用初期値 *)
let debug_player0 = {
  number = 0;
  operation = Hum;
  pieces = [];
  prestart = 4;
  home = 0;
}

let debug_player1 = {
  number = 1;
  operation = Hum;
  pieces = [];
  prestart = 4;
  home = 0;
}

let debug_player2 = {
  number = 2;
  operation = Hum;
  pieces = [];
  prestart = 4;
  home = 0;
}

let debug_player3 = {
  number = 3;
  operation = Hum;
  pieces = [];
  prestart = 4;
  home = 0;
}

let debug_player_list = [debug_player0;
                         debug_player1;
                         debug_player2;
                         debug_player3]

let debug_world = (
  Start,
  0,
  0,
  -1,
  debug_player_list,
  make_blst debug_player_list,
  0,
  [],
  false,
  false,
  (1, 3))

(* -----初期値関係ここまで----- *)


(* -----描画関係----- *)

(* 「現在のターン」を表示する位置 *)
let turn_pos = (width +. menu_width /. 2., 50.)

(* 「OK」ボタン関連 *)
let ok_button_pos = (width +. menu_width /. 2., height /. 2. +. 70.)
let ok_button_width = 100.
let ok_button_height = 30.
let ok_text_pos = (width +. menu_width /. 2. +. ok_button_width /. 2.,
                   height /. 2. +. 70. +. ok_button_height /. 2.)

(* 「サイコロを振る」ボタン関連 *)
let roll_button_pos = (width +. menu_width /. 2., height /. 2.)
let roll_button_width = 170.
let roll_button_height = 30.
let roll_text_pos = (width +. menu_width /. 2. +. roll_button_width /. 2.,
                     height /. 2. +. roll_button_height /. 2.)

(* 「ルール」ボタン関連 *)
let rule_button_pos = (width +. menu_width /. 2., height /. 2. +. 210.)
let rule_button_width = 100.
let rule_button_height = 30.

(* 「操作方法」ボタン関連 *)
let guide_button_pos = (width +. menu_width /. 2., height /. 2. +. 280.)
let guide_button_width = 100.
let guide_button_height = 30.

(* サイコロの位置 *)
let dice_pos = (width +. menu_width /. 2., height /. 2. -. 50.)

(* アナウンスの位置 *)
let announce_pos = (width +. menu_width /. 2., 100.)

(* メッセージの位置 *)
let messages_pos = [(width +. menu_width /. 2., 150.);
                    (width +. menu_width /. 2., 200.);
                    (width +. menu_width /. 2., 250.)]

(* スタート画面の各ボタンの横幅(縦幅) *)
let button_width_start = 277.4

(* スタート画面のHUMの人数の位置 *)
let noh_pos = (527.5, 654.3)

(* スタート画面のCPUの人数の位置 *)
let noc_pos = (635.1, 654.3)

(* スタート画面の人数設定ボタン関連 *)
let hum_button_x = 527.5
let cpu_button_x = 635.1
let up_button_y = 607.6
let down_button_y = 700.9
let arrow_button_width = 54.7
let arrow_button_height = 22.8

(* アナウンスとメッセージを描画する *)
(* draw_messages : world_t -> Image.t *)
let draw_messages (phs, num, pip, sel, plst, blst,
                   tick, messages, rule, guide, nopl) =
  let boo = not (is_cpu (return_player num plst)) in
  let announce = match phs with
      DiceWaiting -> if boo then "サイコロを振ってください" else ""
    | DiceRolling -> "ダイスロール中..."
    | PieceSelecting -> if boo then "動かす駒を選んでください" else ""
    | PieceMoving -> "駒を動かしています..."
    | _ -> "" in
  let mse =
    if is_cpu (return_player num plst) then messages @ ["CPUのターン..."]
    else messages in
  let pos = list_first_half (List.length mse - 1) messages_pos in
   place_image (text announce ~size:15 black)
               announce_pos
  (place_images (List.map (fun s -> text s ~size:15 black) mse)
                pos
  (empty_scene (width +. menu_width) height))

(* 画面右側のメニューを描画する *)
(* draw_menu : world_t -> Image.t*)
let draw_menu ((phs, num, pip, sel, plst, blst,
                tick, messages, rule, guide, nopl) as world) =
   place_image (text ("現在のターン:プレイヤー" ^ string_of_int num) ~size:15 black)
               turn_pos
  (place_image dice_images.(pip)
               dice_pos
  (place_image (text "A" ~size:15 white)
               roll_text_pos
  (place_image (rectangle 15. 15. ~fill:true ~outline_size:2.5 brown)
               roll_text_pos
  (place_image (text "サイコロを振る" ~size:25 black)
               roll_button_pos
  (place_image (rectangle roll_button_width roll_button_height ~fill:false black)
               roll_button_pos
  (place_image (text "ルール" ~size:25 black)
               rule_button_pos
  (place_image (rectangle rule_button_width rule_button_height ~fill:false black)
               rule_button_pos
  (place_image (text "操作方法" ~size:25 black)
               guide_button_pos
  (place_image (rectangle guide_button_width guide_button_height
                          ~fill:false black)
               guide_button_pos
  (place_image (text "D" ~size:15 white)
               ok_text_pos
  (place_image (rectangle 15. 15. ~fill:true ~outline_size:2.5 brown)
               ok_text_pos
  (place_image (text "OK" ~size:25 black)
               ok_button_pos
  (place_image (rectangle ok_button_width ok_button_height ~fill:false black)
               ok_button_pos
  (draw_messages world))))))))))))))

(* 各駒とゴールした個数を描画する *)
(* draw_pieces : player_t -> Image.t -> (int * (int * int) list) list)
                 -> Image.t *)
let draw_pieces player back blst = match player with
    {number = num; pieces = pie; prestart = pre; home = hom;} ->
    let image = pl_images.(num) in
    let hojo (rel, _) back' =
      if ((rel mod 12 = 1 && rel <= 37) || rel = 0)  &&
         ((try
             List.length (List.assoc (return_ab num rel) blst)
           with Not_found -> 0) >= 2)
      then place_image pl_images2.(num) (return_pos2 num rel) back'
      else place_image image (return_pos num rel) back' in
    let rec hojo2 pre' back'=
      if pre' = 0 || pre' > 4 then back'
      else hojo2 (pre' - 1)
                 (place_image image (return_pre_pos num pre') back') in
    hojo2 pre (List.fold_right hojo pie
                 (place_image (text (string_of_int hom) ~size:35 white)
                              pos_table.(52 + 5 * num) back))

(* 一つのマスのブロック状態を描画する *)
(* draw_blocks : int * (int * int) list -> Image.t -> Image.t *)
let draw_blocks (ab, entrys) back =
  let (size, f) =
    if ab mod 12 = 1 && ab <= 37 && List.length entrys >= 2
    then (20, return_pos2_ab)
    else (35, return_pos_ab) in
  let blocks = List.filter (fun (_, nop) -> nop >= 2) entrys in
  let text_lst = List.map (fun (_, nop) -> text (string_of_int nop)
                                                ~size:size black)
                          blocks in
  let pos_lst = List.map (fun (pl, _) -> f pl ab) blocks in
  place_images text_lst pos_lst back

(* ゲーム画面を描画する *)
(* draw_game : world_t -> Image.t *)
let draw_game ((phs, num, pip, sel, plst, blst,
                tick, messages, rule, guide, nopl) as world) =
  let base = List.fold_right draw_blocks blst
             (List.fold_right (fun x y -> draw_pieces x y blst) plst
             (place_image background (width /. 2., height /. 2.)
             (draw_menu world))) in
  if phs = PieceSelecting && sel >= 0
  then
    let (size, o_size, f) =
      if sel mod 12 = 1 && sel <= 37 &&
         ((try
             List.length (List.assoc sel blst)
           with Not_found -> 0) >= 2)
      then (12.5, 2.5, return_pos2_ab)
      else (25., 5., return_pos_ab) in
      place_image (circle size ~fill:false ~outline_size:o_size black)
                  (f num sel) base
  else (* どの駒も選択されていない *) base

(* スタート画面を描画する *)
(* draw_start : world_t -> Image.t *)
let draw_start (phs, num, pip, sel, plst, blst,
                tick, messages, rule, guide, (noh, noc)) =
  let base =
    place_image version_image
                (width +. menu_width /. 2., height /. 2.)
   (place_image (text (string_of_int noh) ~size:45 black)
                noh_pos
   (place_image (text (string_of_int noc) ~size:45 black)
                noc_pos
   (place_image start_back (width /. 2., height /. 2.)
                (empty_scene (width +. menu_width) height)))) in
  if is_allowed (noh, noc) then base
  else place_image (text "この組み合わせではプレイできません。" ~size:15 red)
                   (width -. button_width_start /. 2., 510.)
      (place_image (text "HUMとCPUの合計が2か4になるように" ~size:15 red)
                   (width -. button_width_start /. 2., 525.)
      (place_image (text "設定してください。" ~size:15 red)
                   (width -. button_width_start /. 2., 540.) base))
  
(* 描画を行う *)
(* draw : world_t -> Image.t *)
let draw ((phs, num, pip, sel, plst, blst,
           tick, messages, rule, guide, nopl) as world) =
  let base = match phs with
      Start -> draw_start world
    | _ -> draw_game world in
  if rule then place_image rule_image (width /. 2., height /. 2.) base
  else if guide then place_image guide_image (width /. 2., height /. 2.) base
  else base
  
(* -----描画関係ここまで----- *)

(*
(* messagesにメッセージを追加 *)
(* 最大3件 *)
(* add_message : string -> string list -> string list *)
let add_message string messages =
  let rest =
    if List.length messages >= 3
    then [List.nth messages 0; List.nth messages 1]
    else messages in
  string :: rest
*)

(* マウスの座標と駒の絶対位置を受け取ったら、クリックされたかどうか返す *)
(* is_clicked_piece : int -> int -> int -> bool *)
let is_clicked_piece mx my ab =
  let (px, py) = pos_table.(ab) in 
  (px -. float_of_int mx) *. (px -. float_of_int mx)
  +. (py -. float_of_int my) *. (py -. float_of_int my) <= 25. *. 25.

(* OKボタンがクリックされたかどうか返す *)
(* is_clicked_ok : int -> int -> bool *)
let is_clicked_ok mx my = match ok_button_pos with (bx, by) ->
  bx -. ok_button_width /. 2. <= float_of_int mx
  && float_of_int mx <= bx +. ok_button_width /. 2.
  && by -. ok_button_height /. 2. <= float_of_int my
  && float_of_int my <= by +. ok_button_height /. 2.

(* 「サイコロを振る」ボタンがクリックされたかどうか返す *)
(* is_clicked_roll : int -> int -> bool *)
let is_clicked_roll mx my = match roll_button_pos with (bx, by) ->
  bx -. roll_button_width /. 2. <= float_of_int mx
  && float_of_int mx <= bx +. roll_button_width /. 2.
  && by -. roll_button_height /. 2. <= float_of_int my
  && float_of_int my <= by +. roll_button_height /. 2.

(* ゲーム画面でルールボタンの上にマウスがあるかどうか返す *)
(* is_on_rule : int -> int -> bool *)
let is_on_rule mx my = match rule_button_pos with (bx, by) ->
  bx -. rule_button_width /. 2. <= float_of_int mx
  && float_of_int mx <= bx +. rule_button_width /. 2.
  && by -. rule_button_height /. 2. <= float_of_int my
  && float_of_int my <= by +. rule_button_height /. 2.

(* ゲーム画面で操作方法ボタンの上にマウスがあるかどうか返す *)
(* is_on_guide : int -> int -> bool *)
let is_on_guide mx my = match guide_button_pos with (bx, by) ->
  bx -. guide_button_width /. 2. <= float_of_int mx
  && float_of_int mx <= bx +. guide_button_width /. 2.
  && by -. guide_button_height /. 2. <= float_of_int my
  && float_of_int my <= by +. guide_button_height /. 2.

(* スタート画面でルールボタンの上にマウスがあるかどうか返す *)
(* is_on_rule_start : int -> int -> bool *)           
let is_on_rule_start mx my =
  width -. button_width_start <= float_of_int mx
  && float_of_int mx <= width
  && 0. <= float_of_int my
  && float_of_int my <= button_width_start

(* スタート画面で操作方法ボタンの上にマウスがあるかどうか返す *)
(* is_on_guide_start : int -> int -> bool *)
let is_on_guide_start mx my =
  0. <= float_of_int mx
  && float_of_int mx <= button_width_start
  && height -. button_width_start <= float_of_int my
  && float_of_int my <= height

(* スタート画面でスタートボタンがクリックされたかどうか返す *)
(* is_clicked_start : int -> int -> bool *)
let is_clicked_start mx my =
  0. <= float_of_int mx
  && float_of_int mx <= button_width_start
  && 0. <= float_of_int my
  && float_of_int my <= button_width_start

(* スタート画面でHUMの上ボタンがクリックされたかどうか返す *)
(* is_clicked_hum_up : int -> int -> bool *)
let is_clicked_hum_up mx my =
  hum_button_x -. arrow_button_width /. 2. <= float_of_int mx
  && float_of_int mx <= hum_button_x +. arrow_button_width /. 2.
  && up_button_y -. arrow_button_height /. 2. <= float_of_int my
  && float_of_int my <= up_button_y +. arrow_button_height /. 2.

(* スタート画面でHUMの下ボタンがクリックされたかどうか返す *)
(* is_clicked_hum_down : int -> int -> bool *)
let is_clicked_hum_down mx my =
  hum_button_x -. arrow_button_width /. 2. <= float_of_int mx
  && float_of_int mx <= hum_button_x +. arrow_button_width /. 2.
  && down_button_y -. arrow_button_height /. 2. <= float_of_int my
  && float_of_int my <= down_button_y +. arrow_button_height /. 2.

(* スタート画面でCPUの上ボタンがクリックされたかどうか返す *)
(* is_clicked_cpu_up : int -> int -> bool *)
let is_clicked_cpu_up mx my =
  cpu_button_x -. arrow_button_width /. 2. <= float_of_int mx
  && float_of_int mx <= cpu_button_x +. arrow_button_width /. 2.
  && up_button_y -. arrow_button_height /. 2. <= float_of_int my
  && float_of_int my <= up_button_y +. arrow_button_height /. 2.

(* スタート画面でCPUの下ボタンがクリックされたかどうか返す *)
(* is_clicked_cpu_down : int -> int -> bool *)
let is_clicked_cpu_down mx my =
  cpu_button_x -. arrow_button_width /. 2. <= float_of_int mx
  && float_of_int mx <= cpu_button_x +. arrow_button_width /. 2.
  && down_button_y -. arrow_button_height /. 2. <= float_of_int my
  && float_of_int my <= down_button_y +. arrow_button_height /. 2.

(* 指定された絶対位置がブロックされていないかどうかを返す *)
(* is_not_blocked : int -> int -> (int * (int * int) list) list -> bool *)
let is_not_blocked num ab blst =
  try
    let entrys = List.assoc ab blst in
    List.length (List.filter (fun (pl, nop) -> num <> pl && nop >= 2) entrys) = 0
  with Not_found -> true

(* 与えられた駒が動けるかどうか, つまり次のマスがブロックされていないかどうかを返す *)
(* can_move : int -> piece_t -> (int * (int * int) list) list -> bool *)
let can_move num piece blst =
  match piece with (rel, _) ->
    (* 相対位置0の次のマスは相対位置2 *)
    let next = if rel = 0 then 2 else rel + 1 in
    is_not_blocked num (return_ab num next) blst

(* 指定された絶対位置から移動先までのマスがブロックされていないかどうかを返す *)
(* can_move_ab_to : int -> int -> int -> (int * (int * int) list) list -> bool *)
let can_move_ab_to num ab pip blst =
  let rec hojo i =
    if i <= 0 then true
    else
      let a = return_rel num ab + i in
      let goal = if a > 52 then 52 - (a - 52) else a in
      is_not_blocked num (return_ab num goal) blst && hojo (i - 1) in
  hojo pip

(* 与えられた駒に対し、移動先までのマスがブロックされていないかどうかを返す *)
(* can_move_to : int -> piece_t -> int -> (int * (int * int) list) list 
                 -> bool *)
let can_move_to num piece pip blst =
  match piece with (rel, _) ->
    can_move_ab_to num (return_ab num rel) pip blst

(* 動かす駒のtarをセットする *)
(* set_target : player_t -> int -> int -> (int * (int * int) list) list
                -> player_t *)
let set_target player pip sel blst = match player with
    {number = num; operation = ope; pieces = pie;
     prestart = pre; home = hom} ->
    let rel_sel = return_rel num sel in
    let rest = List.filter (fun (rel, _) -> return_ab num rel <> sel) pie in
    (* 重なっている他の駒は動かさない *)
    let entrys =
      try
        List.assoc sel blst
      with Not_found -> [] in
    let nop =
      try
        List.assoc num entrys
      with Not_found -> 0 in
    let rlst =
      if sel = return_ab num 1 then   (* スタート地点の駒を選んでいる時 *)
        let zero_lst = List.filter (fun (rel, _) -> rel = 0) pie in
        let one_lst_len = List.length (List.filter (fun (rel, _) -> rel = 1)
                                                   pie) in
        if one_lst_len > 0
        then zero_lst @ (list_elements (1, 0) (one_lst_len - 1)) @ rest
        else (list_elements (0, 0) (List.length zero_lst - 1)) @ rest
      else (list_elements (rel_sel, 0) (nop - 1)) @ rest in 
    {number = num; operation = ope; pieces = (rel_sel, pip) :: rlst;
     prestart = pre; home = hom}

(* 選択された駒が動かせる場合、tarをセットして駒を動かすアニメーションを開始する *)
(* prepare : world_t -> world_t *)
let prepare (phs, num, pip, sel, plst, blst,
             tick, messages, rule, guide, nopl) =
  if can_move_ab_to num sel pip blst then
    let new_plst = player_map num (fun x -> set_target x pip sel blst) plst in
    (PieceMoving, num, pip, sel, new_plst, blst, 0, [], rule, guide, nopl)
  else
    let new_message = ["選択中の駒は動かせません"] in
    (phs, num, pip, sel, plst, blst, tick, new_message, rule, guide, nopl)

(* 動かせる駒を持っているかを判定 *)
(* has_movable_piece : player_t -> (int * (int * int) list) list -> bool *)
let has_movable_piece player blst = match player with
    {number = num; pieces = pie} ->
    List.length (List.filter (fun x -> can_move num x blst) pie) > 0

(* 与えられた出目で動かせる駒を持っているかを判定 *)
(* has_movable_piece_pip : player_t -> int -> (int * (int * int) list) list 
                           -> bool *)
let has_movable_piece_pip player pip blst = match player with
    {number = num; pieces = pie} ->
    List.length (List.filter (fun x -> can_move_to num x pip blst) pie) > 0

(* スタート前の駒を持っているか判定 *)
(* has_prestart_piece : player_t -> bool *)
let has_prestart_piece player = match player with
    {prestart = pre} -> pre > 0

(* 次にダイスを振るプレイヤーの番号を返す *)
(* next_player : int -> player_t list -> (int * (int * int) list) list -> int *)
let rec next_player num plst blst =
  let a = (num + 1) mod 4 in
  try 
    let pl = return_player a plst in
    if has_movable_piece pl blst || has_prestart_piece pl
    then a
    else next_player a plst blst
  with Not_found -> next_player a plst blst

(* プレイヤーが相対位置0の駒を持っていたら1にして返す *)
(* zero_to_one : player_t -> player_t *)
let zero_to_one player = match player with
    {number = num; operation = ope; pieces = pie;
     prestart = pre; home = hom} ->
    let hojo (rel, tar) = if rel = 0 then (1, tar) else (rel, tar) in
     {number = num; operation = ope; pieces = List.map hojo pie;
      prestart = pre; home = hom}

(* prepare_plst : int -> int -> player_t list *)
(* numとnext_numは同じでもよい *)
let prepare_plst num next_num plst =
  let next_num' = if num >= next_num then next_num + 4 else next_num in
  let rec hojo i =
    let a = num + i in
    try
      let player = return_player (a mod 4) plst in
      if i > 4 then []
      else if a <= next_num' then zero_to_one player :: hojo (i + 1)
      else player :: hojo (i + 1)
    with Not_found -> hojo (i + 1) in
  hojo 1

(* phsをDiceRollingにし、tickをランダムにセット *)
(* prepare_roll : world_t -> world_t *)
let prepare_roll (phs, num, pip, sel, plst, blst,
                  tick, message, rule, guide, nopl) =
   (DiceRolling, num, pip, sel, plst, blst,
    Random.int 6 + 12, message, rule, guide, nopl)

(* phsがStartの時のマウスクリックによる処理 *)
(* on_mouse_start : world_t -> int -> int -> world_t *)
let on_mouse_start ((phs, num, pip, sel, plst, blst,
                     tick, messages, rule, guide, nopl) as world) mx my =
  if is_clicked_start mx my && is_allowed nopl then  
    let init_plst = initialize_plst nopl in (* 動作デバッグ時はplstにする！ *)
    let next_num = next_player (Random.int 4) init_plst blst in
    (DiceWaiting, next_num, pip, sel, init_plst, blst,
     0, messages, rule, guide, nopl)
  else match nopl with (noh, noc) ->
    (* HUMをいじった時にはCPUも自動で変わることがあるが、逆はない *)
    if is_clicked_hum_up mx my && noh <= 3 then
      if noh + noc <= 3
      then (phs, num, pip, sel, plst, blst,
            tick, messages, rule, guide, (noh + 1, noc))
      else (* noh + noc = 4のときで、noh <= 3よりnoc >= 1 *)
        (phs, num, pip, sel, plst, blst,
         tick, messages, rule, guide, (noh + 1, noc - 1))
    else if is_clicked_hum_down mx my && noh >= 2 then
      if noh + noc >= 3
      then (phs, num, pip, sel, plst, blst,
            tick, messages, rule, guide, (noh - 1, noc))
      else (* noh + noc = 2のときで、noh >= 2よりnoc = 0 *)
        (phs, num, pip, sel, plst, blst,
         tick, messages, rule, guide, (noh - 1, noc + 1))
    else if is_clicked_cpu_up mx my && noc <= 2 && noh + noc <= 3
    then (phs, num, pip, sel, plst, blst,
          tick, messages, rule, guide, (noh, noc + 1))
    else if is_clicked_cpu_down mx my && noc >= 1 && noh + noc >= 3
    then (phs, num, pip, sel, plst, blst,
          tick, messages, rule, guide, (noh, noc - 1))
    else world

(* phsがPieceSelectingの時のマウスクリックによる処理 *)
(* on_mouse_select : world_t -> int -> int -> world_t *)
let on_mouse_select ((phs, num, pip, sel, plst, blst,
                      tick, messages, rule, guide, nopl) as world) mx my =
  if is_clicked_ok mx my && sel >= 0 then prepare world
  else
    let hojo player = match player with
        {number = num'; pieces = pie} ->
        List.map (fun (rel, _) -> return_ab num' rel) pie in
    let ab_lst = hojo (return_player num plst) in
    let new_sel =
      try
        List.find (fun ab -> is_clicked_piece mx my ab) ab_lst
      with Not_found -> sel in
    (phs, num, pip, new_sel, plst, blst,
     tick, messages, rule, guide, nopl)
  
(* マウスイベントによる処理 *)
(* on_mouse : world_t -> int -> int -> string -> world_t *)
let on_mouse ((phs, num, pip, sel, plst, blst,
               tick, message, rule, guide, nopl) as world)
             mx my event =
  (* ゲーム開始後はCPUのターンだったらマウスクリックは受け付けない *)
  let not_cpu_turn =
    try
      not (is_cpu (return_player num plst))
    with Not_found -> true in
  if event = "button_down" && not_cpu_turn then
    match phs with
      Start -> on_mouse_start world mx my
    | DiceWaiting ->
      (if is_clicked_roll mx my
       then prepare_roll world
       else world)
    | PieceSelecting -> on_mouse_select world mx my
    | _ -> world
  else if event = "move" then
    match phs with
      Start ->
      (phs, num, pip, sel, plst, blst, tick, message,
       is_on_rule_start mx my, is_on_guide_start mx my, nopl)
    | _ ->
      (phs, num, pip, sel, plst, blst, tick, message,
       is_on_rule mx my, is_on_guide mx my, nopl)
  else world

(* キーが押された時の処理 *)
(* on_key : world_t -> world_t *)
let on_key ((phs, num, pip, sel, plst, blst,
             tick, message, rule, guide, nopl) as world) key =
  (* CPUのターンだったら何もしない *)
  let cpu_turn =
    try
      is_cpu (return_player num plst)
    with Not_found -> true in
  if cpu_turn then world
  else if key = "A" && phs = DiceWaiting
  then prepare_roll world
  else if key = "D" && phs = PieceSelecting && sel >= 0 then prepare world
  (* デバッグ用 *)(*
  else if key = "0" && phs = DiceWaiting then
    (DiceRolling, num, pip, sel, plst, blst, 30, message, rule, guide, nopl)
  else if key = "1" && phs = DiceWaiting then
    (DiceRolling, num, pip, sel, plst, blst, 31, message, rule, guide, nopl)
  else if key = "2" && phs = DiceWaiting then
    (DiceRolling, num, pip, sel, plst, blst, 32, message, rule, guide, nopl)
  else if key = "3" && phs = DiceWaiting then
    (DiceRolling, num, pip, sel, plst, blst, 33, message, rule, guide, nopl)
  else if key = "4" && phs = DiceWaiting then
    (DiceRolling, num, pip, sel, plst, blst, 34, message, rule, guide, nopl)
  else if key = "5" && phs = DiceWaiting then
    (DiceRolling, num, pip, sel, plst, blst, 35, message, rule, guide, nopl)*)
  else world

(* それぞれの駒のtarの値に応じて駒を動かした後のリストと、まだ動かすべき駒があるかどうかを返す *)
(* move_pieces_on_tick : piece_t list -> piece_t list * bool *)
let rec move_pieces_on_tick pie = match pie with
    [] -> ([], false)
  | (rel, tar) :: rest ->
    if tar = 0 then
      match move_pieces_on_tick rest with (lst, boo) ->
        ((rel, tar) :: lst, boo)
    else if rel < 52 then
      if tar > 0 then ((rel + 1, tar - 1) :: rest, true)
      else (* tar < 0 *) ((rel - 1, tar + 1) :: rest, true)
    else (* rel = 52 *) ((rel - 1, 1 - tar) :: rest, true)

(* プレイヤーを受け取ったら、持っている駒を動かして、さらにまだ動かすべき駒があるかどうかを返す *)
(* 動かすべき駒がなくなったら、ゴールした駒を取り除く *)
(* return_new_pl : player_t -> player_t * bool *)
let return_new_pl player = match player with
    {number = num; operation = ope; pieces = pie; prestart = pre; home = hom} ->
    match move_pieces_on_tick pie with (pie', boo) ->
      if boo then ({number = num; operation = ope; pieces = pie';
                    prestart = pre; home = hom}, true)
      else
        let new_pie = List.filter (fun (rel, _) -> rel < 52) pie' in
        let new_hom = if List.length new_pie = List.length pie' then hom
                      else hom + 1 in
        ({number = num; operation = ope; pieces = new_pie;
          prestart = pre; home = new_hom}, false)

(* 現在のプレイヤーの駒だけを動かしたプレイヤーのリストと、
　　まだ動かすべき駒があるかどうかを返す *)
(* return_new_plst : int -> player_t list -> player_t list * bool *)
let rec return_new_plst num plst = match plst with
    [] -> assert false
  | first :: rest ->
    if first.number = num
    then let (pl, boo) = return_new_pl first in (pl :: rest, boo)
    else match return_new_plst num rest with
        (plst', boo) -> (first :: plst', boo)

(* 絶対位置abにnumの駒が何個あるかを返す *)
(* is_there_blst : int -> int -> (int * (int * int) list) list -> int *)
let is_there_blst num ab blst =
  try
    let entrys = List.assoc ab blst in
    List.assoc num entrys
  with Not_found -> 0
    
(* プレイヤーを受け取ったら、絶対位置abにある駒をいくつ持っているか返す *)
(* is_there_pl : player_t -> int -> int *)
let is_there_pl player ab = match player with {number = num; pieces = pie} ->
  let rec hojo pie' = match pie' with
      [] -> 0
    | (rel, _) :: rest ->
      if return_ab num rel = ab then 1 + hojo rest
      else hojo rest
  in hojo pie

(* はじき飛ばされた駒を駒置き場に戻す *)
(* pi1は駒を動かしたプレイヤー *)
(* pl1とpl2のnumberが一致する場合は何もせず返す *)
(* remove_piece : player_t -> player_t -> player_t *)
let remove_piece pl1 pl2 blst =
  match pl1 with {number = num1; pieces = pie1} ->
  match pl2 with {number = num2; operation = ope; pieces = pie2;
                  prestart = pre; home = hom} ->
    if num1 = num2 then pl2
    else
      let ab_lst = List.map (fun (rel, _) -> return_ab num1 rel) pie1 in
      let hojo (rel, _) =
        let ab = return_ab num2 rel in
        not (List.mem ab ab_lst)
        || return_ab num1 1 = ab   (* 場に出たばかりの駒には弾き飛ばされない *)
        || rel = 0                 (* 場に出たばかりなら弾き飛ばされない *)
        (* すでに同じマスにいるなら弾き飛ばされない *)
        || (rel = 1 && is_there_blst num1 ab blst >= is_there_pl pl1 ab) in 
      let new_pie2 = List.filter hojo pie2 in
      let new_pre = pre + List.length pie2 - List.length new_pie2 in
      {number = num2; operation = ope; pieces = new_pie2;
       prestart = new_pre; home = hom}

(* サイコロの目が出た後の処理 *)
(* after_roll : world_t -> world_t *)
let after_roll (phs, num, pip, sel, plst, blst,
                tick, messages, rule, guide, nopl) =
  let player = return_player num plst in
  if pip = 6 && player.prestart > 0   (* 出目が6で場に出ていない駒がある場合 *)
  then
    let hojo p = match p with
        {number = num'; operation = ope; pieces = pie;
         prestart = pre; home = hom} ->
        {number = num; operation = ope; pieces = (0, 0) :: pie;
         prestart = pre - 1; home = hom} in
    let new_plst = player_map num hojo plst in
    let new_blst = make_blst new_plst in
    let new_player = hojo player in
    let next_num =
      if new_player.prestart > 0 || has_movable_piece new_player new_blst
      then num
      else next_player num new_plst new_blst in
    let prepared_plst =
      if next_num = num then new_plst
      else prepare_plst num next_num new_plst in
    (DiceWaiting, next_num, pip, sel, prepared_plst, new_blst,
     0, [], rule, guide, nopl)
  else if has_movable_piece_pip player pip blst
  (* その出目で動かせる駒があるかどうか *)
  (* 出目が6で、全ての駒が場に出ていて、動かせる駒があるときも含む *)
  then
    (PieceSelecting, num, pip, -1, plst, blst,
     0, [], rule, guide, nopl)
  else if pip = 6 && has_movable_piece player blst
  (* 出目が6で、全ての駒が場に出ているが動かせず、他の出目だった場合は動かせるとき *)
  then
    let new_message =["もう1回振ってください"] in
    (DiceWaiting, num, pip, sel, plst, blst,
     0, new_message, rule, guide, nopl)
  else (* 動かせる駒がないので次の人の番へ *)
    let next_num = next_player num plst blst in
    let prepared_plst = prepare_plst num next_num plst in
    (DiceWaiting, next_num, pip, sel, prepared_plst, blst,
     0, [], rule, guide, nopl)

(* phsがPieceSelectingのときの時間経過による処理 *)
(* on_tick_select : world_t -> world_t *)
let on_tick_select ((phs, num, pip, sel, plst, blst,
                     tick, messages, rule, guide, nopl) as world) =
  if is_cpu (return_player num plst) then   (* CPUだったら自動で駒を選ぶ *)
    if tick >= 5 then
      let hojo player = match player with
          {number = num'; pieces = pie} ->
            List.map (fun (rel, _) -> return_ab num' rel) pie in
      let ab_lst = List.filter (fun ab -> can_move_ab_to num ab pip blst)
          (hojo (return_player num plst)) in
      let new_sel = List.nth ab_lst (Random.int (List.length ab_lst)) in
      prepare (phs, num, pip, new_sel, plst, blst,
               tick, messages, rule, guide, nopl)
      else (phs, num, pip, sel, plst, blst,
            tick + 1, messages, rule, guide, nopl)
  else world

(* phsがPieceMovingのときの時間経過による処理 *)
(* on_tick_move : world_t -> world_t *)
let on_tick_move (phs, num, pip, sel, plst, blst,
                  tick, messages, rule, guide, nopl) =
  if tick mod 5 = 0 then
    let (lst, boo) = return_new_plst num plst in
    if boo
    then (PieceMoving, num, pip, sel, lst, blst,
          tick + 1, messages, rule, guide, nopl)
    else
      let new_plst =
        List.map
          (fun x -> remove_piece (return_player num lst) x blst)
          lst in
      let new_blst = make_blst new_plst in
      let next_num =
        if pip = 6 then num
        else next_player num new_plst new_blst in
      let prepared_plst =
        if next_num = num then new_plst
        else prepare_plst num next_num new_plst in
      (DiceWaiting, next_num, pip, sel, prepared_plst, new_blst,
       0, messages, rule, guide, nopl)
  else (phs, num, pip, sel, plst, blst,
        tick + 1, messages, rule, guide, nopl)

(* 時間経過による処理 *)
(* on_tick : world_t -> world_t *)
let on_tick ((phs, num, pip, sel, plst, blst,
              tick, message, rule, guide, nopl) as world) =
  match phs with
    DiceWaiting ->
    if is_cpu (return_player num plst) then   (* CPUだったら自動でサイコロを振る *)
      if tick >= 5
      then prepare_roll world
      else (phs, num, pip, sel, plst, blst,
            tick + 1, message, rule, guide, nopl)
    else world
  | DiceRolling ->
    if tick <= 0 then after_roll world
    else (phs, num, pip mod 6 + 1, sel, plst, blst,
          tick - 1, message, rule, guide, nopl)
  | PieceSelecting -> on_tick_select world
  | PieceMoving -> on_tick_move world
  | _ -> world
       

(* -----終了判定関係----- *)

(* 誰かが4つ駒をゴールさせたら終了する *)
(* stop_when : world_t -> bool*)
let stop_when (phs, num, pip, sel, plst, blst,
               tick, messages, rule, guide, nopl) =
  List.exists (fun {home = hom} -> hom >= 4) plst

(* ゲーム終了画面を表示する *)
(* draw_last : world_t -> Image.t *)
let draw_last (phs, num, pip, sel, plst, blst,
               tick, message, rule, guide, nopl) =
  let winner_num = match List.find (fun {home = hom} -> hom >= 4) plst with
    {number = num'} -> num' in
  let last_announce = "プレイヤー" ^ string_of_int winner_num ^ "の勝ちです" in
  let last_world =
    (Finish, winner_num, pip, sel, plst, blst,
     tick, [], rule, guide, nopl) in
   place_image (text last_announce ~size:15 black)
               announce_pos
  (place_image (text "Finished!" ~size:100 black)
               (width /. 2., height /. 2.)
               (draw last_world))

(* -----終了判定関係ここまで----- *)


(* ゲームの動く速さ（単位：ミリ秒）*)
(* rate : int *)
let rate = 100 (* ms *)

(* ゲーム開始 *)
let _ =
  big_bang initial_world
           ~name:"Ludo"
           ~width:(int_of_float (width +. menu_width))
           ~height:(int_of_float height)
           ~to_draw:draw
           ~on_key_press:on_key
           ~on_mouse:on_mouse
           ~on_tick:on_tick
           ~rate:rate
           ~stop_when:stop_when
           ~to_draw_last:draw_last
