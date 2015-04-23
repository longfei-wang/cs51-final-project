open Core.Std


(* The raw data we use *)
type dataset = { 
	text:string;
	flag:string;
}

 
(* read_data from a file *)
val read : string -> dataset list

(* randomly split data according to a input ratio *)
val split_data : dataset list -> float -> (dataset list * dataset list)


(* break down text into features *)
module type Text_to_Feature = 
sig
	(* A feature could be just a word or 2 adjacent words or anything defined here *)
	type feature

	val break_down : string -> feature list
	val run_test : unit -> unit
end


(* the base classifier *)
module type Classifier =
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


(* A Simple Decision Tree will take a text_to_feature module and Dict module to construct*)
(* module Simple_Decision_Tree (TF : text_to_feature) (D : Dict) : Classifier =
struct
	......
end *)

(* A random forest classifier is constructed by a number of trees *)
(* module Random_Forest (C : Classifier) (num_of_trees : int) : Classifier =
struct
	......
end *)

(* Benchmark and then calculate sensitivity and specificity *)
module Benchmark (C : Classifier) =
struct

	val run :  dataset -> (float * float)

end













