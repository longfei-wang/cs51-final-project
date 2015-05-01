open Core.Std

(* break down text into features *)
module type TEXT_TO_FEATURE = 
sig
	(* A feature could be just a word or 2 adjacent words or anything defined here *)
	type feature with sexp
	(* a feature set that don't have duplicates *)
	type set

	(* the main function to break string into features. *)
	val break_down : string -> feature list

	(* convert feature to string *)
	val to_string : feature -> string

	(* convert string to feature *)
	val from_string : string -> feature

	(* convert feature list to a set *)
	val to_set : feature list -> set

	(* add feature to a set *)
	val add : set -> feature -> set

	(* check if a feature exists in a set *)
	val mem : set -> feature -> bool

	(* pop a feature from a set *)
	val choose : set -> feature option 

	(* remove a feature from a set *)
	val remove : set -> feature -> set

	(* combine 2 sets *)
	val union : set -> set -> set

	val length : set -> int

	val fold : set -> init:'a -> f:('a -> feature -> 'a) -> 'a

	val run_test : unit -> unit
end

(* The most simple one, each word is a feature *)
module Bag_of_words : TEXT_TO_FEATURE = 
struct

	type feature = string with sexp

	type set = String.Set.t

	let break_down text = 
		let clean_text = String.filter (String.lowercase text) ~f:(fun c -> not (List.mem (String.to_list "!?();:,.\"\'") c)) in
		String.split clean_text ~on:' '

	let to_string f = f

	let from_string s = s

	let to_set fl =
		List.fold_left ~f:(fun i x -> String.Set.add i x) ~init:String.Set.empty fl

	let add = String.Set.add

	let mem = String.Set.mem

	let choose = String.Set.choose

	let remove = String.Set.remove

	let union = String.Set.union

	let fold = String.Set.fold

	let length = String.Set.length

	let run_test () = 
		let t = "wow! this is looking great!! Do you think so?" in
		let features = break_down t in
		Printf.printf "\nTesting bag of words: %s\n" t;
		List.iter ~f:(fun x -> Printf.printf "%s; \n" x ) features

end


module Bigram : TEXT_TO_FEATURE = 
struct

	type feature = string with sexp

	type set = String.Set.t

	let break_down text = 
		let clean_text = String.filter (String.lowercase text) ~f:(fun c -> not (List.mem (String.to_list "!?();:,.\"\'") c)) in
		let words = String.split clean_text ~on:' ' in
		let rec join (s:string list): string list =
			(match s with
			|hd::md::tl -> (hd^":"^md)::(join (md::tl))
			|_::[] -> []
			|[] -> []) in
		join words

	let to_string f = f

	let from_string s = s

	let to_set fl =
		List.fold_left ~f:(fun i x -> String.Set.add i x) ~init:String.Set.empty fl

	let add = String.Set.add

	let mem = String.Set.mem

	let choose = String.Set.choose

	let remove = String.Set.remove

	let union = String.Set.union

	let fold = String.Set.fold

	let length = String.Set.length

	let run_test () = 
		let t = "wow! this is looking great!! Do you think so?" in
		let features = break_down t in
		Printf.printf "\nTesting bigram: %s\n" t;
		List.iter ~f:(fun x -> Printf.printf "%s; \n" x ) features
end