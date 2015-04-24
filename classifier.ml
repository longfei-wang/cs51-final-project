open Core.Std
open Parsedata
open Feature

(* the base classifier *)
module type ClASSIFIER =
sig

	(* the parameter matrix used to construct a classifier *)
	type matrix

	(* the prediction *)
	type prediction = float
	
	(* fit data list to classifier and come up with a matrix *)
	val fit : dataset list -> matrix

	(* use the matrix we got to predict*)
	val predict : dataset -> matrix -> prediction
	
	val run_test : unit -> unit

end


(* A binary decision tree *)
module Decision_Tree (F: TEXT_TO_FEATURE) : ClASSIFIER =
struct

	(* one instance of the data that classifier use *)
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
		| Node of F.feature * tree * tree
	
	type matrix = tree

	type prediction = float

	(* convert dataset list to a data list format that classifier can use *)
	(* use TEXT_TO_FEATURE to breakdown text into features *)
	let rec clean (d: dataset list) : data list = 
		match d with
		| hd::tl -> {features=F.break_down hd.text;flag=hd.flag}::(clean tl)
		| [] -> []

	(* split data list according to a feature *)
	let split (d : data list) (f : F.feature)  : data list * data list = List.partition_tf ~f:(fun x -> List.mem x.features f) d

	(* calculate the gini index of given data list*)
	let gini d =
		let n = float (List.length (List.filter ~f:(fun x -> x.flag = true) d)) in
		let t = float (List.length d) in
		((n /. t) *. (1. -. (n /. t))) ** 2.

	(* gini index gain to rank features *)
	let gain (d : data list) (f : F.feature) : float = 
		let left,right = split d f in
		let n_left = float (List.length left) in
		let n_right = float (List.length right) in
		(gini d) -. ((n_left /. (n_left +. n_right)) *. (gini left)) -. ((n_left /. (n_left +. n_right)) *. (gini right))  

	(* aggreate features to set 
	low frequency features need to be deleted from the set?*)
	let rec agg (d: data list) : F.set =
		match d with
		| [] -> F.to_set []
		| hd::tl -> F.union (F.to_set hd.features) (agg tl)

	(* find the best feature based on gini gain *)
	let rec best (d: data list) (fs: F.set) (f: F.feature option) (g: float) : F.feature option = 
		match f, F.choose fs with
		| None, None -> None
		| None, Some x -> best d (F.remove fs x) (Some x) (gain d x)
		| Some f1, None -> Some f1
		| Some _, Some x -> 
			let gx = gain d x in
			let nfs =  F.remove fs x in
			if gx >. g then best d nfs (Some x) gx else best d nfs f g

	(* training = building a tree
	there is no pruning because this tree is buit for random forest *)
	let fit (d: dataset list) : matrix = 
		let rec build (d: data list) (fs: F.set) : matrix =
			(* first test if the data is already pure *)
			(match List.count ~f:(fun x -> x.flag = true) d, List.count ~f:(fun x -> x.flag = false) d with
			| 0,_ -> Leaff
			| _,0 -> Leaft
			| tc,fc ->
				(* then choose the best feature to divide data*)
				(match best d fs None 0. with
				| None -> 
					if tc  > fc then Leaft else Leaff 
				| Some x -> 
					let left,right = split d x in
					let nfs = F.remove fs x in
					Node (x, (build left nfs), (build right nfs)))) in
		let dl = clean d in 
		let fs = agg dl in
		Printf.printf "number of features for this tree %d\n" (F.length fs); flush_all(); build dl fs

	let predict d m =
		let rec climb (fl: F.feature list) (m : matrix) : prediction =
			(match m with
			| Leaff -> 0.
			| Leaft -> 1.
			| Node (f,left,right) -> if List.mem fl f then climb fl left else climb fl right) in
		climb (F.break_down d.text) m

	let run_test () = ()
end

(* 2 constant for Random Forest *)
let numTree = 20

let ratioData = 0.2


module Random_Forest (T: ClASSIFIER) : ClASSIFIER =
struct

	type matrix = T.matrix list

	type prediction = float
	(* randomly select data according to ratioData 
	to overcome imbalanced class in training data,
	 I will choose same number of data from each class*)
	let select (d: dataset list): dataset list = 
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
      			parse_one (List.filteri ~f:(fun i _ -> i <> r) f) ((List.nth_exn f r)::t) (n-1)) in

  		let dt1 = parse_one dt [] num in
  		let df1 = parse_one df [] num in
  		dt1@df1


	let fit d =
		let rec build (d:dataset list) (n: int) : matrix =
			(match n>=1 with
			| false -> []
			| true -> (T.fit (select d))::(build d (n-1))) in
		build d numTree

	let predict d m =
		let rec climb (d:dataset) (m:matrix) : float = 
			(match m with
			| [] -> 0.
			| hd::tl -> (T.predict d hd) +. (climb d tl)) in
		(climb d m) /. (float (List.length m))


	let run_test () = ()
end