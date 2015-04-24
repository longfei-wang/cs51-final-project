open Parsedata
open Feature
open Classifier

let a  = read training_data in
Printf.printf "total %d \n" (List.length a);
test_split (random_split a 0.2);
 

Bag_of_words.run_test ();