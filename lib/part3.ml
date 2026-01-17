open Base
open Util

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree [@@deriving show]

let rec equal_tree (equal : 'a -> 'a -> bool) (t1 : 'a tree) (t2 : 'a tree) : bool =
    match t1, t2 with
    | Leaf, Leaf -> true 
    | Node (v1, l1, r1), Node (v2, l2, r2) -> 
        if (equal v1 v2) 
        then (equal_tree equal l1 l2) && (equal_tree equal r1 r2) 
        else false
    | _, _ -> false
 
let timestamp (t : 'a tree) : (int * 'a) tree =
    let rec helper (next : int) (t : 'a tree) : ((int * 'a) tree * int) = 
        match t with
        | Leaf -> (Leaf, next)
        | Node (v, l, r) -> 
    in

