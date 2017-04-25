(* Some and None *)
let rec nth n l = 
	match (n,l) with 
			(_,[]) -> None
		| 	(_,_)) when n<0 -> None
		| 	(0, h::t) -> Some h
		|   (_, h::t) -> nth (n-1) t;;  

type inList = Empty | Node of int = intList

let rec toIntList l = 
	match l with
		[] -> Empty
		| h::t -> Node (h, toIntList t)

let rec fromIntList l = 
	match l with
		Empty -> []
		| Node(h,t) -> h::(fromIntList t)

(* polymorphic lists *)

type 'a myList = Empty | Node of 'a * 'a myList

let rec mymap f l = 
	match l with
		[] -> Empty
		| Node (h,t) -> Node (f h, mymap f t)

(* binary trees *)

type 'a mytree = Leaf | InternalNode of 'a * 'a mytree * 'a mytree

let rec size t = 
	match t with
		Leaf -> 0
		| InternalNode(_, l, r) -> l + (size l) + (size r)