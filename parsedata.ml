open Core.Std
open Printf

let training_data = "SMSSpamCollection"

type dataset = { 
  text:string;
  flag:bool;
}

(* Read training data from a file *)
let read (file: string) : dataset list =
  let ic = open_in file in
  In_channel.fold_lines ic ~f:(fun i line -> 
    match String.get line 0 with
    | 'h' -> {text =  (String.slice line 4 0); flag = false }::i
    | 's' -> {text = (String.slice line 5 0); flag = true }::i
    | _ -> i
  ) ~init:[]


(* test function for read *)
let rec test_read (input: dataset list): unit = 
  match input with
  | hd::tl -> printf "%s" hd.text ; flush_all (); test_read tl;
  | [] -> ()


(* randomly split data into 2 dataset with a given ratio *)
let random_split (input: 'a list) (ratio: float) : ('a list * 'a list) = 
  Random.init (Float.to_int (Unix.time ()));
  let len = List.length input in
  let num = Float.to_int (ratio *. (float len)) in
  let rec parse_one (i: 'a list * 'a list) (n: int) : ('a list * 'a list) =
    (match n >= 1 with
    | false -> i
    | true -> 
      let (t,f) = i in
      let r = Random.int (List.length f) in
      parse_one ((List.nth_exn f r)::t, (List.filteri ~f:(fun i _ -> i <> r) f)) (n-1)) in
  parse_one ([], input) num



(* test function for split *)
let test_split (i: 'a list * 'a list) : unit =
  let (t,f) = i in
  printf "first %d second %d \n" (List.length t) (List.length f)






