(*
	a grammar for the subset of OCaml we' ve seen
	
	=  checking structural equality
	== checking physical equality
	E ::= C | X E OP E
		| function X -> E
		| if E then E else E
		| match E with P -> E '|' ... '|' P->E
		| [] | E :: E
		| let P in E::E
		| (x,y)


	C ::= 0 | 1 | 2 | ... | true | false | "hi" | ...
	X ::= variable names

	OP ::= + | - | < | ...
	P ::= C | _ | X | P1::P2      (P1 head of the list, P2 the rest of the list)
*)

(* odd index *)
(* generic *)
let rec everyOther l = 
	match l with 
		[] -> []
		| first :: second :: rest -> first :: (everyOther rest)
		| [_] -> l   (* or | _ -> l  or   _::[] -> l *)
	(*	| [first :: _] -> [first]  *)


let double n = n*2;;
let quadruple n = (double n) + (double n);;

let quadruple n = 
	let d = (double n)
	in d + d;; 
	(* chain rule
	let d = (double n) in
	let q = d + d in q
	*)

(* tuple *)
(3, "hi", true)

let swap = 
	function (x,y) -> (y,x);;

swap(3, "hello world");;

();;
unit

let three() = 3;;  (* size 0 or two more *)

(* zip ([1;2;3], ["a";"b";"c"])  =  [(1;"a"); (2,"b"); (3;"c")]*)
let rec zip (l1,l2) = 
	match (l1, l2) with
	([],[]) -> []
	| (h1::t1, h2::t2) -> ([h1;h2] :: zip(t1, t2))

(* unzip *)

(* First-class functions:
	functions are like any other functions 
	- can be passed to other functions
	- can be stored in variables/lists
	- can be returned from functions
*)

let fourthPower x = square(square x);;

let twice (f,x) = f(f x);;
twice (double, 3);;
twice (double, 10);;
let quadruple x = twice(double, x);;
quadruple 10;;   (* 40 *)
let twice  = function (f,x) -> f(f,x);;
let twice  = function f -> (function x -> f(f x));;

(twice double) 3;; (* 12 *)

let twice = fun f x -> f(f x);;
let twice f x = f(f x);;

# let quadruple = twice double;;
val quadruple : int -> int = <fun>

(* twice takes a pair of arguments *)
let twice(f,x) = f(f x)

let twice' = function f -> (function x -> f(f,x));;
twice' double 3;;  |   (twice' double) 3;;    
(* 12 *)

(* shorthand 
	let twice' = fun f x -> f(f x);;
	let twice' f x = f(f x)

*)

let rec incList l = 
	match l with
		[] -> []
		| h::t = (h+1)::(incList t);;

let rec exclaimList l = 
	match l with
		[] -> []
		| h::t -> (h ^ "!")::(exclaimList t)

let rec map f l = 
	match l with
		[] -> []
		| h::t -> (f h)::(map f t);;

map (function s -> (String.length s) > 3) ["hi"; "there"];;

let incLst = map (function x -> x+1);;
incLst [1;2;3];;    (* [2;3;4] *)


map (map (function x -> x+1)) [[1;2;3];[4;5]];;

# List.filter (function x -> x>2) [1;2;3;4;5];;  
- : int list = [3; 4; 5]

let rec filter p l = 
	match l with
		[] -> []
	| h::t ->
		let rest = filter p t in 
		if p h then h::rest else rest 


































