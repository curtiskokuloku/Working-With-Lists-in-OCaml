(** Find the N'th element of a list. *)

let rec at k = function
    | [] -> None
    | h :: t -> if k = 0 then Some h else at (k - 1) t


(** Find the number of elements of a list. *)

let length list =
    let rec aux n = function
      | [] -> n
      | _ :: t -> aux (n + 1) t
    in
    aux 0 list