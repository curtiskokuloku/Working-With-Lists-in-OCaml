(** Remove the K'th element from a list. 
   
  The first element of the list is numbered 0, the second 1,... *)

let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t

(** Rotate a list N places to the left. *)

let split list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (h :: acc) t  in
    aux n [] list

  let rotate list n =
    let len = List.length list in
    (* Compute a rotation value between 0 and len - 1 *)
    let n = if len = 0 then 0 else (n mod len + len) mod len in
    if n = 0 then list
    else let a, b = split list n in b @ a