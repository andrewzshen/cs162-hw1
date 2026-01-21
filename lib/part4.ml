open Base
open Util

type expr =
  | Const of int
  | X
  | Add of expr * expr
  | Mul of expr * expr
  | Compose of expr * expr

(* Pretty-printer *)
let rec pp_expr ppf =
  let open Fmt in
  function
  | Const n -> int ppf n
  | X -> string ppf "x"
  | Add (e1, e2) -> pf ppf "@[<hov 2>(%a + %a)@]" pp_expr e1 pp_expr e2
  | Mul (e1, e2) -> pf ppf "@[<hov 2>(%a * %a)@]" pp_expr e1 pp_expr e2
  | Compose (e1, e2) -> pf ppf "@[<hov 2>(%a; %a)@]" pp_expr e1 pp_expr e2

(* Convert an expression into a pretty string *)
let show_expr (e : expr) : string = Fmt.to_to_string pp_expr e

let rec eval_expr (x : int) (e : expr) : int = 
    match e with
    | Const n -> n
    | X -> x
    | Add (e1, e2) -> (eval_expr x e1) + (eval_expr x e2)   
    | Mul (e1, e2) -> (eval_expr x e1) * (eval_expr x e2)
    | Compose (e1, e2) -> eval_expr (eval_expr x e1) e2

let rec simplify (e : expr) : expr = 
    match e with
    | Const _ -> e
    | X -> e
    | Add (e1, e2) ->
        let e1' = simplify e1 in
        let e2' = simplify e2 in
        (match e1', e2' with
        | _, Const 0 -> e1'
        | Const 0, _ -> e2'
        | Const x, Const y -> Const (x + y)
        | _, _ -> Add (e1', e2') )
    | Mul (e1, e2) ->
        let e1' = simplify e1 in
        let e2' = simplify e2 in
        (match e1', e2' with
        | _, Const 1 -> e1'
        | Const 1, _ -> e2'
        | Const x, Const y -> Const (x * y)
        | _, _ -> Mul (e1', e2') )
    | Compose (e1, e2) ->
        let rec substitute (replace : expr) (e : expr) : expr = 
            match e with
            | Const n -> Const n
            | X -> replace
            | Add (e1, e2) -> Add (substitute replace e1, substitute replace e2) 
            | Mul (e1, e2) -> Mul (substitute replace e1, substitute replace e2) 
            | Compose (e1, e2) -> substitute replace (substitute e1 e2) 
        in
        simplify (substitute e1 e2)

type poly = int list [@@deriving show]

let rec eval_poly (x : int) (p : poly) : int = 
    (* Using Horner's Method: https://en.wikipedia.org/wiki/Horner%27s_method *)
    match p with
    | [] -> 0
    | h::t -> h + x * eval_poly x t 

let rec normalize (e : expr) : poly =
    let e' = simplify e in 
    match e' with
    | Const n -> [n]
    | X -> [0; 1]
    | Add (e1, e2) ->
        let rec add_poly (p1 : poly) (p2: poly) : poly = 
            match p1, p2 with
            | [], p -> p
            | p, [] -> p
            | c1::r1, c2::r2 -> (c1 + c2)::(add_poly r1 r2)
        in
        add_poly (normalize e1) (normalize e2)
    | Mul (e1, e2) ->
        let rec mul_poly (p1 : poly) (p2 : poly) : poly = todo()
        in
        mul_poly (normalize e1) (normalize e2)
    | Compose (e1, e2) -> []

let semantic_equiv (e1 : expr) (e2 : expr) : bool = bonus ()
