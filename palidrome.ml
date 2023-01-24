(** Find out whether a list is a palindrome. *)

let is_palindrome list =
    (* One can use either the rev function from the previous problem, or the built-in List.rev *)
    list = List.rev list