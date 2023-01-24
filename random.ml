(** Create a list containing all integers within a given range. 
    
  If first argument is greater than second, produce a list in decreasing 
  order.*)

let range a b =
    let rec aux acc high low =
      if high >= low then
        aux (high :: acc) (high - 1) low
      else acc
    in
      if a < b then aux [] b a else List.rev (aux [] a b)

(** Extract a given number of randomly selected elements from a list. 
    
  The selected items shall be returned in a list. We use the Random 
  module but do not initialize it with Random.self_init for 
  reproducibility. *)

let rand_select list n =
    let rec extract acc n = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux n acc list len =
      if n = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (n - 1) (picked :: acc) rest (len - 1)
    in
    let len = List.length list in
      aux (min n len) [] list len


(** Draw N different random numbers from the set 1..M.

  The selected numbers shall be returned in a list. *)

(* [range] and [rand_select] defined in problems above *)
  let lotto_select n m = rand_select (range 1 m) n