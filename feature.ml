open Core.Std

(* break down text into features *)
module type TEXT_TO_FEATURE = 
sig
	(* A feature could be just a word or 2 adjacent words or anything defined here *)
	type feature

	(* the main function to break string into features. *)
	val break_down : string -> feature list
	
	val run_test : unit -> unit
end

(* The most simple one, each word is a feature *)
module Bag_of_words : TEXT_TO_FEATURE = 
struct

	type feature = string

	let break_down text = 
		let clean_text = String.filter text ~f:(fun c -> not (List.mem (String.to_list "!?();:,.\"\'") c)) in
		String.split clean_text ' '

	let run_test () = 
		let features = break_down "wow! this is looking great!! Do you think so?" in
		List.iter ~f:(fun x -> Printf.printf "%s; \n" x ) features

end