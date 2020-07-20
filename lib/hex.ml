open Core

module Hex : sig
  type t [@@deriving sexp]

  val ( + ) : t -> t -> t
  val of_row_col : row:int -> col:int -> t
end = struct
  type t = int * int [@@deriving sexp]

  let of_row_col ~row ~col = row, col
  let ( + ) (r1, c1) (r2, c2) = r1 + r2, c1 + c2
end

let%expect_test _ =
  print_s [%message (Hex.of_row_col ~row:3 ~col:5 : Hex.t)];
  [%expect {| ("Hex.of_row_col ~row:3 ~col:5" (3 5)) |}]
;;

let main () = print_endline "Hello, world!"
