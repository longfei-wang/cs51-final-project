open Core.Std
open Parsedata
open Feature
open Classifier
open Printf

(* let a  = read training_data in
Printf.printf "total %d \n" (List.length a);
test_split (random_split a 0.2);
 

Bag_of_words.run_test (); *)

module Tree = Decision_Tree(Bag_of_words)
module Forest = Random_Forest(Tree)
let d,_ =  random_split (read training_data) 0.5 in
let m = Forest.fit d in
printf "Mom. this is great!    %f\n" (Forest.predict 
	{text="Mom. this is great!";flag=false} m);
printf "Congratulation! You've won our top prize! Call 754-212-0270 to have your prize sent    %f\n" (Forest.predict 
	{text="Congratulation! You've won our top prize! Call 754-212-0270 to have your prize sent";flag=false} m);

