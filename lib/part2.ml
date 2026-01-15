open Base
open Util

let rec compress (equal : 'a -> 'a -> bool) (xs : 'a list) : 'a list = 
    match xs with
    | [] -> []
    | h::b::t when equal h b -> compress equal (h::t) 
    | h::t -> h::(compress equal t)
;;

let int_max x y = if x > y then x else y;;

let max (xs : int list) : int option = 
    match xs with
    | [] -> None
    | h::t -> int_max h (max t)
;;

let rec join (xs : 'a option list) : 'a list option = todo ()

let insert (key : 'k) (value : 'v) (dict : ('k * 'v) list) : ('k * 'v) list =
  (key, value) :: dict

let rec lookup (equal : 'k -> 'k -> bool) (key : 'k) (dict : ('k * 'v) list) :
    'v option =
  todo ()
