(** Generate the combinations of K distinct objects chosen from the N 
    elements of a list.

    In how many ways can a committee of 3 be chosen from a group of 12 
    people? We all know that there are C(12,3) = 220 possibilities (C(N,K)
    denotes the well-known binomial coefficients). For pure mathematicians,
    this result may be great. But we want to really generate all the 
    possibilities in a list. *)

let rec extract k list =
    if k <= 0 then [[]]
    else match list with
         | [] -> []
         | h :: tl ->
            let with_h = List.map (fun l -> h :: l) (extract (k - 1) tl) in
            let without_h = extract k tl in
            with_h @ without_h