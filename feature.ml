open Core.Std

(* break down text into features *)
module type TEXT_TO_FEATURE = 
sig
	(* A feature could be just a word or 2 adjacent words or anything defined here *)
	type feature
	(* a feature set that don't have duplicates *)
	type set

	(* the main function to break string into features. *)
	val break_down : string -> feature list
	
	(* convert feature list to a set *)
	val to_set : feature list -> set

	(* pop a feature from a set *)
	val choose : set -> feature option 

	(* remove a feature from a set *)
	val remove : set -> feature -> set

	(* union 2 sets *)
	val union : set -> set -> set

	val length : set -> int

	val run_test : unit -> unit
end

(* The most simple one, each word is a feature *)
module Bag_of_words : TEXT_TO_FEATURE = 
struct

	type feature = string

	type set = String.Set.t

	let break_down text = 
		let clean_text = String.filter text ~f:(fun c -> not (List.mem (String.to_list "!?();:,.\"\'") c)) in
		String.split clean_text ~on:' '

	let to_set fl =
		List.fold_left ~f:(fun i x -> String.Set.add i x) ~init:String.Set.empty fl

	let choose s =
		String.Set.choose s

	let remove s f = 
		String.Set.remove s f

	let union s1 s2 =
		String.Set.union s1 s2

	let length s =
		String.Set.length s

	let run_test () = 
		let features = break_down "wow! this is looking great!! Do you think so?" in
		List.iter ~f:(fun x -> Printf.printf "%s; \n" x ) features

end