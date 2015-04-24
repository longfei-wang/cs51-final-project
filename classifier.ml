open Core.Std
open Parsedata

(* the base classifier *)
module type ClASSIFIER =
sig
	(* one instance of data *)
	type data

	(* the parameter matrix used to construct a classifier *)
	type matrix

	(* the prediction *)
	type prediction

	(* convert dataset list to a data list format that classifier can use *)
	val clean : dataset list -> data list

	(* fit data list to classifier and come up with a matrix *)
	val fit : data list -> matrix

	(* use the matrix we got to predict*)
	val predict : dataset -> matrix -> prediction
	
	val run_test : unit -> unit
end


(* A binary decision tree *)
module Decision_Tree (F: TEXT_TO_FEATURE) : ClASSIFIER =
struct

	type data = {
		features : F.feature list;
		flag : bool;
	}

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

	(* we use core_set to store features 
	we might needt to use core_map if we want
	to take word frequency into account *)
	type set = String.Set.t

	type empty =  String.Set.empty

	(* use TEXT_TO_FEATURE to breakdown text into features *)
	let rec clean d = 
		match d with
		| hd::tl -> {feature=F.bread_down hd.text;flag=hd.flag}::(clean tl)
		| [] -> []

	(* split data list according to a feature *)
	let split (d : data list) (f : feature)  : data list * data list = List.partition ~f:(fun x -> List.mem x.feature feature) d

	(* calculate the gini index of given data list*)
	let gini d =
		let n = float (List.length (List.filter ~f:(fun x -> x.flag = true) d)) in
		let N = float (List.length d) in
		((n /. N) *. (1. -. (n /. N))) ** 2.

	(* gini index gain to rank features *)
	let gain (d : data list) (f : feature) : float = 
		let left,right = split d f in
		let n_left = float (List.length left) in
		let n_right = float (List.length right) in
		(gini d) -. ((n_left /. (n_left +. n_right)) *. (gini left)) -. ((n_left /. (n_left +. n_right)) *. (gini right))  

	(* aggreate features to set *)
	let rec agg (d: data list) : F.set =
		match d with
		| [] -> None
		| hd::tl -> F.union (F.to_set hd.features) (agg tl)

	(* find the best feature based on gini gain *)
	let rec best (d: data list) (fs: F.set) (f: F.feature option) (g: float) : F.feature option = 
		(match f, F.choose fs with
		| None, None -> None
		| None, Some x -> best d (F.remove fs x) (Some x) (gain d x)
		| Some f1, None -> Some f1
		| Some _, Some x -> 
			let gx = gain d x in
			let nfs =  F.remove fs x in
			if gx > g then best d nfs (Some x) gx else best d nfs f g) in

	(* training = building a tree *)
	let fit (d: data list) : matrix = 
		let fset = agg d in
		let rec build (d: data list) (fs: F.set) : matrix =
			(match best d fs None 0. with
			| None -> if List.count (fun x -> x.flag = true) > List.count (fun x -> x.flag = false) then Leaft else Leaff 
			| Some x -> 
				let left,right = split d x in
				let nfs = F.remove fs x in
				Node (x, (build left nfs), (build right nfs))) in
		build d fset

	let predict d m =
		(let rec climb (fs: feature list) (m : matrix) : prediction =
			match m with
			| Leaff -> false
			| Leaft -> true
			| Node (f,left,right) -> if List.mem f fs then climb fs left else climb fs right) in
		climb (F.bread_down d.text) m

	let run_test () = ()
end

(* 2 constant for Random Forest *)
let numTree = 20

let ratioData = 0.2


module Random_Forest (T: Random_Forest) : ClASSIFIER =
struct

	type data = T.data

	type matrix = T.matrix list

	type precition = float

	let clean = T.clean

	(* randomly select data according to ratioData 
	to overcome imbalanced class in training data,
	 I will choose same number of data from each class*)
	let select (d: data list): data list = 
		let dt = List.filter ~f:(fun x -> x.flag = true) d in
		let df = List.filter ~f:(fun x -> x.flag = false) d in
		let nt = List.length dt in
		let nf = List.length df in
		let num = Float.to_int (ratioData *. (float (if nt > nf then nf else nt)))in
  		
  		Random.init (Float.to_int (Unix.time ()));
  		
  		let rec parse_one (f: 'a list) (t: 'a list)  (n: int) : 'a list =
    		(match n >= 1 with
    		| false -> t
    		| true -> 
      			let r = Random.int (List.length f) in
      			parse_one (List.filteri ~f:(fun i _ -> i <> r) f) (List.nth_exn f r)::t (n-1)) in

  		let dt1 = parse_one dt [] num in
  		let df1 = parse_one df [] num in
  		dt1@df1


	let fit d =
		let rec build (d:data list) (n: int) : matrix =
			(match n>=1 with
			| false -> []
			| true -> (T.fit (select d))::(build d (n-1))) in
		build d numTree

	let predict d m =
		let rec climb (d:dataset) (m:matrix) : int = 
			(match m with
			| [] -> 0
			| hd::tl -> (if T.predict d hd then 1 else 0) + (climb d tl)) in
		(float (climb d m)) /. (float (List.length m))


	let run_test () = ()
end