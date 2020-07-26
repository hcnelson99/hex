open Core

(* TODO: Make this be a module argument? *)
let board_size = 5

module Hex : sig
  type t [@@deriving sexp, hash, compare]

  include Comparable.S with type t := t

  (* val ( + ) : t -> t -> t *)
  (* val of_row_col : row:int -> col:int -> t option *)

  val of_row_col_exn : row:int -> col:int -> t
  val row : t -> int
  val col : t -> int
  val adjacent : t -> t list
  val of_move_string : string -> t option
end = struct
  module T = struct
    type t = int * int [@@deriving sexp, hash, compare]
  end

  include T

  let in_bounds (row, col) = 0 <= row && row < board_size && 0 <= col && col < board_size

  let of_row_col ~row ~col =
    let hex = row, col in
    if in_bounds hex then Some hex else None
  ;;

  let of_row_col_exn ~row ~col =
    match of_row_col ~row ~col with
    | Some hex -> hex
    | None -> failwith "Hex coords out of bounds"
  ;;

  let row (row, _) = row
  let col (_, col) = col
  let adjacent_directions = [ -1, 0; -1, 1; 0, 1; 1, 0; 1, -1; 0, -1 ]
  let ( + ) (r1, c1) (r2, c2) = r1 + r2, c1 + c2

  let adjacent t =
    adjacent_directions |> List.map ~f:(fun dir -> t + dir) |> List.filter ~f:in_bounds
  ;;

  let safe_int_of_string s =
    try Some (int_of_string s) with
    | Failure _ -> None
  ;;

  let of_move_string s =
    let len = String.length s in
    if len = 0
    then None
    else
      let open Option.Let_syntax in
      let first_char = s.[0] in
      let rest = String.sub s ~pos:1 ~len:(len - 1) in
      let%bind row = safe_int_of_string rest in
      let%bind col = String.index "abcdefghijklmnopqrstuvwxyz" first_char in
      of_row_col ~row:(row - 1) ~col
  ;;

  include Comparable.Make (T)
end

let%expect_test _ =
  let hex = Hex.of_row_col_exn ~row:0 ~col:0 in
  let adjacent = Hex.adjacent hex in
  print_s [%message (hex : Hex.t) (adjacent : Hex.t list)];
  [%expect {| ((hex (0 0)) (adjacent ((0 1) (1 0)))) |}]
;;

let%expect_test _ =
  let strings = [ "c3"; "a4"; "b2"; "d1"; "%3"; "a100" ] in
  strings
  |> List.map ~f:(fun s -> s, Hex.of_move_string s)
  |> List.map ~f:(fun (s, h) ->
         s
         ^ " -> "
         ^
         match h with
         | None -> "fail"
         | Some h -> Sexp.to_string (Hex.sexp_of_t h))
  |> List.iter ~f:print_endline;
  [%expect
    {|
    c3 -> (2 2)
    a4 -> (3 0)
    b2 -> (1 1)
    d1 -> (0 3)
    %3 -> fail
    a100 -> fail |}]
;;

module Board : sig
  type t

  module Turn : sig
    type t =
      | X
      | O
    [@@deriving equal, compare]

    val to_string : t -> string
  end

  module Won_board : sig
    type t

    val to_string : t -> string
  end

  val starting_board : t
  val to_string : t -> string

  val make_move
    :  t
    -> Hex.t
    -> [ `Invalid_move | `Next_board of t | `Winner of Turn.t * Won_board.t ]

  val turn : t -> Turn.t
  val moves : t -> Hex.t list
  val random_move : t -> Hex.t
end = struct
  module Turn = struct
    type t =
      | X
      | O
    [@@deriving equal, compare]

    let flip = function
      | X -> O
      | O -> X
    ;;

    let to_string = function
      | X -> "X"
      | O -> "O"
    ;;
  end

  type t =
    { turn : Turn.t
    ; board : Turn.t Hex.Map.t
    }

  let turn { turn; board = _ } = turn
  let starting_board = { turn = Turn.X; board = Hex.Map.empty }
  let repeat s ~count = List.init count ~f:(fun _ -> s) |> String.concat

  let board_to_string board =
    "O goes from top to bottom"
    :: ("   "
       ^ ("abcdefghijklmnopqrstuvwxyz"
         |> String.sub ~pos:0 ~len:board_size
         |> String.to_list
         |> List.map ~f:Char.to_string
         |> String.concat ~sep:"   "))
    :: List.init board_size ~f:(fun row ->
           Printf.sprintf "%2d  " (row + 1)
           ^ repeat " " ~count:(row * 2)
           ^ String.concat
               (List.init board_size ~f:(fun col ->
                    let display =
                      Hex.of_row_col_exn ~row ~col
                      |> Map.find board
                      |> Option.value_map ~default:"-" ~f:Turn.to_string
                    in
                    display ^ "   ")))
    |> String.concat ~sep:"\n"
  ;;

  let to_string { turn; board } =
    Turn.to_string turn ^ "'s turn -- " ^ board_to_string board
  ;;

  module Won_board = struct
    type t = Turn.t Hex.Map.t

    let to_string = board_to_string
  end

  let%expect_test _ =
    let example_board =
      { turn = Turn.O
      ; board =
          Hex.Map.empty
          |> Hex.Map.set ~key:(Hex.of_row_col_exn ~row:0 ~col:1) ~data:Turn.X
          |> Hex.Map.set ~key:(Hex.of_row_col_exn ~row:2 ~col:3) ~data:Turn.O
          |> Hex.Map.set ~key:(Hex.of_row_col_exn ~row:4 ~col:3) ~data:Turn.X
      }
    in
    example_board |> to_string |> print_endline;
    [%expect
      {|
        O's turn -- O goes from top to bottom
           a   b   c   d   e
         1  -   X   -   -   -
         2    -   -   -   -   -
         3      -   -   -   O   -
         4        -   -   -   -   -
         5          -   -   -   X   - |}]
  ;;

  let has_won ~player board =
    let of_player_color h =
      match Map.find board h with
      | Some c -> Turn.equal c player
      | None -> false
    in
    let starting_hexes =
      (match player with
      | Turn.X -> List.init board_size ~f:(fun row -> Hex.of_row_col_exn ~row ~col:0)
      | Turn.O -> List.init board_size ~f:(fun col -> Hex.of_row_col_exn ~row:0 ~col))
      |> List.filter ~f:of_player_color
    in
    let visited = Hex.Set.of_list starting_hexes in
    let rec go (visited, work_list) =
      match work_list with
      | [] -> false
      | h :: work_list ->
        let is_target =
          match player with
          | X -> Hex.col h = board_size - 1
          | O -> Hex.row h = board_size - 1
        in
        if is_target
        then true
        else (
          let visited = Set.add visited h in
          let unvisited_adjacent =
            Hex.adjacent h
            |> List.filter ~f:(fun h -> of_player_color h && not (Set.mem visited h))
          in
          let work_list = unvisited_adjacent @ work_list in
          go (visited, work_list))
    in
    go (visited, starting_hexes)
  ;;

  let make_move { board; turn } hex =
    match Map.find board hex with
    | None ->
      let board = Map.set board ~key:hex ~data:turn in
      if has_won ~player:turn board
      then `Winner (turn, board)
      else `Next_board { board; turn = Turn.flip turn }
    | Some _ -> `Invalid_move
  ;;

  let moves { board; turn = _ } =
    List.init board_size ~f:(fun row ->
        List.init board_size ~f:(fun col -> Hex.of_row_col_exn ~row ~col))
    |> List.concat
    |> List.filter_map ~f:(fun hex ->
           match Map.find board hex with
           | None -> Some hex
           | Some _ -> None)
  ;;

  let random_choice l =
    let i = Random.int (List.length l) in
    List.nth_exn l i
  ;;

  let random_move t = List.random_element_exn (moves t)
end

module MCTS = struct
  type node_data =
    | UpperConfidenceBound of { children : t list }
    | MonteCarlo of
        { board : Board.t
        ; unexplored_moves : Hex.t list
        ; explored_moves : t list
        }

  and t =
    { wins : int
    ; simulations : int
    ; move : Hex.t option
    ; turn : Board.Turn.t
    ; node_data : node_data
    }

  let create ?move board =
    { wins = 0
    ; simulations = 0
    ; move
    ; turn = Board.turn board
    ; node_data =
        MonteCarlo { board; unexplored_moves = Board.moves board; explored_moves = [] }
    }
  ;;

  let rec rollout board =
    let move = Board.random_move board in
    match Board.make_move board move with
    | `Invalid_move -> assert false
    | `Winner (winner, _) -> winner
    | `Next_board next -> rollout next
  ;;

  let update_node t ~winner =
    { t with
      simulations = t.simulations + 1
    ; wins = (t.wins + if Board.Turn.equal t.turn winner then 1 else 0)
    }
  ;;

  let max_by_exn l ~score ~greater =
    let rec go (max, max_score, seen, l) =
      match l with
      | [] -> max, seen
      | x :: l ->
        let score = score x in
        if greater score max_score
        then go (x, score, max :: seen, l)
        else go (max, max_score, x :: seen, l)
    in
    match l with
    | [] -> assert false
    | max :: l ->
      let max_score = score max in
      go (max, max_score, [], l)
  ;;

  let rec iterate' { wins; simulations; move; turn; node_data } =
    match node_data with
    | MonteCarlo { board; unexplored_moves; explored_moves } ->
      (match unexplored_moves with
      | [] ->
        iterate'
          { wins
          ; simulations
          ; move
          ; turn
          ; node_data = UpperConfidenceBound { children = explored_moves }
          }
      | move :: unexplored_moves ->
        (match Board.make_move board move with
        | `Invalid_move -> assert false
        | `Winner (winner, _) ->
          (* If a move off our board won, we know our opponent won so just
           * increment the simulation count *)
          ( { wins
            ; simulations
            ; move = Some move
            ; turn
            ; node_data = MonteCarlo { board; unexplored_moves; explored_moves }
            }
            |> update_node ~winner
          , winner )
        | `Next_board child_board ->
          let winner = rollout child_board in
          let explored_move = child_board |> create |> update_node ~winner in
          ( { wins
            ; simulations
            ; move = Some move
            ; turn
            ; node_data =
                MonteCarlo
                  { board
                  ; unexplored_moves
                  ; explored_moves = explored_move :: explored_moves
                  }
            }
            |> update_node ~winner
          , winner )))
    | UpperConfidenceBound { children } ->
      let score_child node =
        Float.(
          (of_int node.wins / of_int node.simulations)
          + (sqrt (of_int 2) * sqrt (log (of_int simulations) / of_int node.simulations)))
      in
      let best_child, children =
        max_by_exn children ~score:score_child ~greater:Float.( > )
      in
      let best_child, winner = iterate' best_child in
      ( { wins
        ; simulations
        ; move
        ; turn
        ; node_data = UpperConfidenceBound { children = best_child :: children }
        }
        |> update_node ~winner
      , winner )
  ;;

  let iterate t = iterate' t |> fst

  let best_move { node_data; _ } =
    match node_data with
    | MonteCarlo _ -> failwith "Not enough simulations"
    | UpperConfidenceBound { children } ->
      let best_move, _ =
        max_by_exn children ~score:(fun node -> node.simulations) ~greater:Int.( > )
      in
      best_move.move |> Option.value_exn
  ;;
end

let mcts_player board =
  let rec go mcts i =
    if i = 100000 then MCTS.best_move mcts else go (MCTS.iterate mcts) (i + 1)
  in
  go (MCTS.create board) 0
;;

let rec human_player board =
  print_string (Board.Turn.to_string (Board.turn board) ^ ": ");
  Out_channel.flush stdout;
  match
    In_channel.input_line In_channel.stdin |> Option.value_exn |> Hex.of_move_string
  with
  | Some move -> move
  | None ->
    print_endline "Invalid move. Try again.";
    human_player board
;;

let better_starting_board =
  match Board.make_move Board.starting_board (Hex.of_row_col_exn ~row:2 ~col:2) with
  | `Next_board b -> b
  | _ -> assert false
;;

let play () =
  let rec go ~x_player ~o_player board =
    print_endline (Board.to_string board);
    let move =
      match Board.turn board with
      | Board.Turn.X -> x_player board
      | Board.Turn.O -> o_player board
    in
    match Board.make_move board move with
    | `Invalid_move -> failwith "Invalid move"
    | `Next_board board' -> go ~x_player ~o_player board'
    | `Winner (winner, board) ->
      print_endline (Board.Won_board.to_string board);
      print_endline ("Player " ^ Board.Turn.to_string winner ^ " won!")
  in
  go ~x_player:mcts_player ~o_player:mcts_player better_starting_board
;;

let main () = play ()
