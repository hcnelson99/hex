open Core

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
    c3 -> (3 2)
    a4 -> (4 0)
    b2 -> (2 1)
    d1 -> (1 3)
    %3 -> fail
    a100 -> fail |}]
;;

module Board : sig
  type t

  val starting_board : t
  val to_string : t -> string
  val make_move : t -> Hex.t -> t option
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

  let starting_board = { turn = Turn.X; board = Hex.Map.empty }
  let repeat s ~count = List.init count ~f:(fun _ -> s) |> String.concat

  let to_string { board; turn } =
    (Turn.to_string turn ^ "'s turn -- O goes from top to bottom")
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

  let make_move { board; turn } hex =
    match Map.find board hex with
    | None ->
      let board = Map.set board ~key:hex ~data:turn in
      Some { board; turn = Turn.flip turn }
    | Some _ -> None
  ;;

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

  let has_won { board; turn } =
    let starting_hexes =
      (match turn with
      | Turn.X -> List.init board_size ~f:(fun row -> Hex.of_row_col_exn ~row ~col:0)
      | Turn.O -> List.init board_size ~f:(fun col -> Hex.of_row_col_exn ~row:0 ~col))
      |> List.filter ~f:(fun h ->
             match Map.find board h with
             | Some c -> Turn.equal c turn
             | None -> false)
    in
    let visited = Hex.Set.of_list starting_hexes in
    let rec go (visited, work_list) =
      match work_list with
      | [] -> false
      | h :: work_list ->
        let is_target =
          match turn with
          | X -> Hex.col h = board_size - 1
          | O -> Hex.row h = board_size - 1
        in
        if is_target
        then true
        else (
          let visited = Set.add visited h in
          let work_list = Hex.adjacent h @ work_list in
          go (visited, work_list))
    in
    go (visited, starting_hexes)
  ;;
end

let rec get_move () =
  match
    In_channel.input_line In_channel.stdin |> Option.value_exn |> Hex.of_move_string
  with
  | Some move -> move
  | None -> get_move ()
;;

let play () =
  let rec go board =
    print_endline (Board.to_string board);
    let move = get_move () in
    match Board.make_move board move with
    | None -> go board
    | Some board' -> go board'
  in
  go Board.starting_board
;;

let main () = play ()
