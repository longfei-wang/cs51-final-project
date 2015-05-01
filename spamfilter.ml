open Core.Std
open Parsedata
open Feature
open Classifier
open Printf
open Benchmark

(* filename use to store matrix *)
let file_matrix = "matrix.txt" 

let number_of_tree = 20
let ratio_to_feed_tree = 0.5

(* write a string to file *)
let to_file (m: string) : unit =
	let oc = open_out file_matrix in   
	Out_channel.output_string oc m;   
  	Out_channel.close oc;
  	()

(* read a string from a file *)
let from_file () : string option =
	let ic = open_in file_matrix in
	In_channel.input_line ~fix_win_eol:true ic


module Tree = Decision_Tree(Bigram)

module Setting : Random_Forest_Setting = 
struct
	let numTree = ref number_of_tree
	let ratioData = ref ratio_to_feed_tree
end

module Forest = Random_Forest(Tree)(Setting)

module Bench = Benchmark(Forest)

let predict t = 
	match from_file () with
	| Some x -> Forest.predict {text = t; flag=false} (Forest.from_string x)
	| None ->
		let d,_ =  random_split (read training_data) 0.8 in
		let m = Forest.fit d in
		to_file (Forest.to_string m);
		Forest.predict {text = t; flag=false} m

let run_test () =
	let a  = read training_data in
	test_read a;
	Printf.printf "\ntotal number of entries %d \n" (List.length a);
	let x,y = random_split a 0.8 in
	test_split (x,y);
 	Bag_of_words.run_test ();
 	Bigram.run_test ();
 	Tree.run_test ();
 	Forest.run_test ();
 	()



let command =
  Command.basic
    ~summary:"\n##########################################################\nA Random Forest Classifier\n##########################################################"
    ~readme:(fun () -> "Input:\ntrain for training\nbenchmark for benchmarking\ntest to run test\n\nAuthor: Longfei Wang")
  Command.Spec.(
      empty
      +> flag "-n" (optional int) ~doc:"Number of trees in Random Forest"
      +> flag "-r" (optional float) ~doc:"Percentage of Data to use for each tree"
      +> anon ("text_msg" %: string)  
  )
    (fun nT rD p () -> 
    	Setting.numTree := 
			(match nT with
			| None -> number_of_tree
			| Some x -> x);
		Setting.ratioData := 
			(match rD with
			| None -> ratio_to_feed_tree
			| Some x -> x);
    	match p with
    	| "train" -> 
			printf "Training with %d number of trees and %f of data is feeded to each tree\n" !Setting.numTree !Setting.ratioData;
			flush_all ();
			let d,_ =  random_split (read training_data) 0.8 in
			let m = Forest.fit d in
			to_file (Forest.to_string m);
    	| "benchmark" ->
    		printf "\nBenchmark starting\n";
    		let t0 = Unix.time () in 
    		let d,t =  random_split (read training_data) 0.8 in
			let m = Forest.fit d in
			let t1 = Unix.time () in
			printf "time for training: %f s\n" (t1 -. t0);
			to_file (Forest.to_string m);
			Bench.run t m;
		| "test" -> run_test ();
    	| _ -> printf "%s: %f\n" p (predict p);
    )



let () =
  Command.run ~version:"1.0" ~build_info:"N/A" command

