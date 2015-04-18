open Printf
  
let file = "SMSSpamCollection"

  (* Read file and display the first line *)
  let ic = open_in file in
  try 
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    print_endline line;          (* write the result to stdout *)
    flush stdout;                (* write on the underlying device now *)
    close_in ic                  (* close the input channel *) 
  
  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e                      (* exit with error: files are closed but
                                    channels are not flushed *)
  
  (* normal exit: all channels are flushed and closed *)