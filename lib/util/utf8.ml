open Core

type t =
  | C1 of char
  | C2 of char * char
  | C3 of char * char * char
  | C4 of char * char * char * char
[@@deriving compare, equal]

let parse s =
  let rec parse acc s =
    match s with
    | [] -> acc
    | a :: tl when 0x00 <= Char.to_int a && Char.to_int a <= 0x7f ->
      parse (C1 a :: acc) tl
    | a :: b :: tl when 0b11000000 <= Char.to_int a && Char.to_int a <= 0b11011111 ->
      parse (C2 (a, b) :: acc) tl
    | a :: b :: c :: tl when 0b11100000 <= Char.to_int a && Char.to_int a <= 0b11101111 ->
      parse (C3 (a, b, c) :: acc) tl
    | a :: b :: c :: d :: tl
      when 0b11110000 <= Char.to_int a && Char.to_int a <= 0b11110111 ->
      parse (C4 (a, b, c, d) :: acc) tl
    | _ -> failwithf !"UTF-8 parsing failed; left: %s" (String.of_list s) ()
  in
  let s = String.to_list s in
  List.rev (parse [] s)
;;

let to_string t =
  match t with
  | C1 c -> String.of_char c
  | C2 (c1, c2) -> String.of_char c1 ^ String.of_char c2
  | C3 (c1, c2, c3) -> String.of_char c1 ^ String.of_char c2 ^ String.of_char c3
  | C4 (c1, c2, c3, c4) ->
    String.of_char c1 ^ String.of_char c2 ^ String.of_char c3 ^ String.of_char c4
;;

module Parser = struct
  type nonrec 'a t = { run : t list -> t list * 'a Or_error.t }
end
