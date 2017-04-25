
(*lookup: 'a -> ('a * 'b) list -> 'b option *)
let rec lookup k l = 
	match l with 
		[] -> None
		| (key, v)::t -> 
			if key = k then Some v else lookup k t;;

(* advantage : it makes the error case very explicit 
	disadvantage: it forces every caller to deal with the errors *)

let lookupAndExclaim k l = 
	let vopt = lookup k l in
	match vopt with
		None -> None
		| Some s -> Some (s^"!")


(* lookup a bunch of keys and return None if 
		any of them is missing *)

let rec lookupAll ks l = 
	match ks with
		[] -> Some []
		| k::rest -> 
			let vopt = lookup k l in 
			match vopt with
				None -> None
				| Some v -> let vs = lookupAll rest l in 
				match vsopt with 
					None -> None
					| Some vs -> Some (v::vs)


exception NotFound of string
exception AnotherOne
let rec lookupE k l = 
	match l with 
		[] -> raise NotFound
		| (key, v)::t -> 
			if key = k then v else lookupE k l 

(* implicitly propagates the exception *)
let lookupAndExclaim k l = 
	(lookupE k l) ^ "!"

(* in this version, I want to return "error" if there is an error *)
let lookupAndExclaimTry k l = 
	try 
		(lookupE k l) ^ "!"
	with 
		None s -> "error"^s
		| AnotherOne -> "another error"
		| _ -> "others"

(* lookup a bunch of keys and return None if any of them is missing *)
let rec lookupAllE ks l = 
	List.map (fun k -> lookupE k l) ks

let lookupAllTry ks l = 
	try 
		Some (List.map(fun k -> lookupE k l) ks)
	with
		NotFound s -> None

(* Same or None for each element*)
let lookupAllTry2 ks l = 
	List.map
		(fun k -> 
			try Some (lookupE k l) with
				NotFound s -> None)


