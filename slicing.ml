(** Split a list into two parts; the length of the first part is given.
    If the length of the first part is longer than the entire list, 
    then the first part is the list and the second part is empty *)

let split list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (h :: acc) t 
    in
      aux n [] list

  
(** Given two indices, i and k, the slice is the list containing the elements
    between the i'th and k'th element of the original list (both limits 
    included). 
    
    Start counting the elements with 0 (this is the way the List module 
    numbers elements).*)

let slice list i k =
    let rec take n = function
      | [] -> []
      | h :: t -> if n = 0 then [] else h :: take (n - 1) t
    in
    let rec drop n = function
      | [] -> []
      | h :: t as l -> if n = 0 then l else drop (n - 1) t
    in
    take (k - i + 1) (drop i list)