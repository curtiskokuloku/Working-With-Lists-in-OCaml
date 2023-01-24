(** Write a function last : 'a list -> 'a option that returns the 
    last element of a list. *)

let rec last = function 
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t


(** Find the last but one (last and penultimate) elements of a list. *)

let rec last_two = function
    | [] | [_] -> None
    | [x; y] -> Some (x,y)
    | _ :: t -> last_two t