open Base
open Util

let rec compress (equal : 'a -> 'a -> bool) (xs : 'a list) : 'a list = 
    match xs with
    | [] -> []
    | a::b::rest when equal a b -> compress equal (a::rest) 
    | h::t -> h::(compress equal t)
;;

let max (xs : int list) : int option = 
    let rec helper_max (curr_max : int) (l : int list) : int = 
        match l with
        | [] -> curr_max
        | h::t -> if h > curr_max then h else (helper_max curr_max t)
    in
    match xs with
    | [] -> None
    | h::t -> Some (helper_max h t)
;;

let rec join (xs : 'a option list) : 'a list option = todo ()

let insert (key : 'k) (value : 'v) (dict : ('k * 'v) list) : ('k * 'v) list =
  (key, value) :: dict

let rec lookup (equal : 'k -> 'k -> bool) (key : 'k) (dict : ('k * 'v) list) :
    'v option =
  todo ()
