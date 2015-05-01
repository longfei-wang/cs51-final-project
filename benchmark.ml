open Core.Std
open Classifier
open Parsedata


(* Benchmark and then calculate sensitivity and specificity *)
module Benchmark (C : CLASSIFIER) =
struct

	let run (d: dataset list) (m: C.matrix) = 
		let n = float (List.length d) in
		let tp = ref 0 in
		let fp = ref 0 in
		let tn = ref 0 in
		let fn = ref 0 in
		let s = List.fold_left ~init:0. d ~f:(fun i x ->
			let p = C.predict x m in
			(match p > 0.5, x.flag with
			| true, true -> tp := !tp + 1
			| true, false -> fp := !fp + 1
			| false, true -> fn := !fn + 1
			| false, false -> tn := !tn + 1);
			let f = (if x.flag = true then 1.0 else 0.0) in
			((p -. f) ** 2.) +. i 
		) in
		Printf.printf "True Positive(Sensitivity): %f\nTrue Negative(Specificity): %f\nPrecision: %f\nNegative predictive power: %f\nDeviation: %f\n" 
		((float !tp) /. (float (!tp + !fn))) 
		((float !tn) /. (float (!tn + !fp))) 
		((float !tp) /. (float (!tp + !fp))) 
		((float !tn) /. (float (!tn + !fn))) 
		(s /. n)

end

