open Core.Std
open Parsedata

(* the base classifier *)
module type ClASSIFIER =
sig
	type data
	(* the parameter matrix used to construct a classifier *)
	type matrix

	type prediction

	type score

	(* convert dataset to a data format that classifier can use *)
	val clean : dataset -> data

	(* fit data to classifier and come up with a matrix *)
	val fit : data -> matrix

	(* use the matrix we got to predict*)
	val predict : data -> matrix -> prediction
	
	(* this is for benchmarking *)
	val cross_validate : prediction -> prediction -> score

	val run_test : unit -> unit
end


(* A binary decision tree *)
module Decision_Tree (F: TEXT_TO_FEATURE) : ClASSIFIER =
struct

	type entry = {
		features : F.feature list;
		flag : bool;
	}

	type data = entry list

	(* The Decision Tree: 
	A) Each node is a feature 
	B) Left tree is the entries that have the feature.
	   Right tree is the entries that dont have the feature.
	C) Leaft Leaff represent 2 classes 
	*)
	type tree = 
		| Leaft
		| Leaff 
		| Node of feature * tree * tree
	
	type matrix = tree

	type prediction = bool

	type score = float

	(* use TEXT_TO_FEATURE to breakdown text into features *)
	let rec clean d = 
		match d with
		| hd::tl -> {feature=F.bread_down hd.text;flag=hd.flag}::(clean tl)
		| [] -> []

	(* split data according to a feature *)
	let split (d : data) (f : feature)  : data * data = List.partition ~f:(fun x -> List.mem x.feature feature) data

	(* calculate the gini index of given data*)
	let gini data =
		let n = float (List.length (List.filter ~f:(fun x -> x.flag = true) data)) in
		let N = float (List.length data) in
		((n /. N) *. (1. -. (n /. N))) ** 2.

	(* gini index gain to rank features *)
	let gain (d : data) (f : feature) : float = 
		let left,right = split data feature in
		let n_left = float (List.length left) in
		let n_right = float (List.length right) in
		(gini data) -. ((n_left /. (n_left +. n_right)) *. (gini left)) -. ((n_left /. (n_left +. n_right)) *. (gini right))  


	let rank (d: data) (fs : feature set) : feature option =

	(* training = building a tree *)
	let fit d = 




end