(* -------- 2.5 -------- *)

(* ex.4 *)
(* times : int list -> int *)
let rec times lst = match lst with
    [] -> 1
  | 0 :: _ -> shift (fun _ -> 0)
  | first :: rest -> first * times rest ;;
let rec times = function        (* functionを使った場合 *)
    [] -> 1
  | 0 :: _ -> shift (fun _ -> 0)
  | first :: rest -> first * times rest ;;
let a = reset (fun () -> times [2; 3; 0; 5]) ;;

let times2 l = reset (fun () -> times l);; (* resetも関数に組み込んだ場合 *)
let a = times2 [2; 3; 0; 5];;

(* -------- 2.6 -------- *)

let f x = reset (fun () -> 3 + shift (fun k -> k) - 1) x ;;
let f = reset (fun () -> 3 + shift (fun k -> k) - 1) ;;

(* ex.5 *)
let a =
  reset (fun () -> 5 * (shift (fun k -> k) + 3 * 4));;
let a =
  reset (fun () -> (if shift (fun k -> k) then "hello" else "hi") ^ " world");;
let a =
  reset (fun () -> fst (let x = shift (fun k -> k) in (x, x)));;
let a =
  reset (fun () -> string_length ("x" ^ string_of_int (shift (fun k -> k))));;

(*
 * ex.6
 * id : 'a list -> 'a list
 *)
let rec id lst = match lst with
    [] -> shift (fun k -> k)
  | first :: rest -> first :: id rest ;;
let a = reset (fun () -> id [1; 2; 3]);;

(* -------- 2.7 -------- *)

type tree_t = Empty
            | Node of tree_t * int * tree_t ;;
(* walk : tree_t -> unit *)
let rec walk tree = match tree with
    Empty -> ()
  | Node (t1, n, t2) ->
     walk t1;
     print_int n;
     walk t2 ;;
let tree1 = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty)) ;;
let a = walk tree1 ;;

type 'a result_t = Done
                 | Next of int * (unit / 'a -> 'a result_t / 'a) ;;
(* yield : int => unit *)
let yield n = shift (fun k -> Next (n, k)) ;;
let rec walk tree = match tree with
    Empty -> ()
  | Node (t1, n, t2) ->
     walk t1;
     yield n;
     walk t2 ;;
(* *** Type Error !! ***
reset (fun () -> walk tree1) ;;
 *)
(* start : tree_t -> ’a result_t *)
let start tree =
  reset (fun () -> walk tree; Done) ;;

(* print_nodes : tree_t -> unit *)
let print_nodes tree =
  let rec loop r = match r with
      Done -> () (* no more nodes *)
    | Next (n, k) ->
       print_int n; (* print n *)
       loop (k ()) in (* and continue *)
  loop (start tree) ;;
let a = print_nodes tree1;;

(* add_tree : tree_t -> int *)
let add_tree tree =
  let rec loop r = match r with
      Done -> 0
    | Next (n, k) -> n + loop (k ()) in
  loop (start tree) ;;
let a = add_tree tree1;;

(* ex.7 *)
let same_fringe tree1 tree2 =
  let rec loop (r1, r2) = match (r1, r2) with
      Done, Done -> true
    | Next (n1, k1), Next (n2, k2) ->
       if n1 = n2 then loop (k1 (), k2 ())
       else false
    | Done, Next _ | Next _, Done -> false
  in
  loop (start tree1, start tree2) ;;
let tree1 = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty)) ;;
let tree2 = Node (Empty, 1, Node (Empty, 2, Node (Empty, 3, Empty))) ;;
let tree3 = Node (Empty, 1, Node (Empty, 4, Node (Empty, 3, Empty))) ;;
let a = same_fringe tree1 tree2;;
let a = same_fringe tree1 tree3;;

(* -------- 2.8 -------- *)

let f x = reset (fun () -> shift (fun k -> fun () -> k "hello") ^ " world") x ;;
let a = f ();;

(* ex.8 *)
let a = reset (fun () -> "hello " ^ shift (fun k -> fun s -> k s) ^ "!") "world" ;;
let a = reset (fun () ->
            (fun a -> fun b -> "hello " ^ a ^ b)
              (shift (fun k ->
                   fun d1 -> fun d2 -> k (string_of_int d1) (string_of_int d2))))
          1 2;;

let printf format to_s = reset (fun () -> format (shift (fun k -> to_s k))) ;;
let format = fun a -> fun b -> "hello " ^ a ^ b ;;
let to_s k = fun d1 -> fun d2 -> k (string_of_int d1) (string_of_int d2) ;;
let a = printf format to_s 1 2;;
