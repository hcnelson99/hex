open Core

let board_size = 5

module Hex : sig
  type t [@@deriving sexp, hash, compare]

  include Comparable.S with type t := t

  (* val ( + ) : t -> t -> t *)
  (* val of_row_col : row:int -> col:int -> t option *)
  val of_row_col_exn : row:int -> col:int -> t
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
      of_row_col ~row ~col
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

module Board : sig end = struct
  module Turn = struct
    type t =
      | X
      | O
  end

  type t = Turn.t option Hex.Map.t
end

let main () = print_endline "Hello, world!"
