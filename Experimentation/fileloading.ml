open Printf

let extract_filename s =
	let len = String.length s in  
        let extract_within = String.sub s 1 (len-2) in
        let filename = (extract_within ^ ".pl") in
	filename

let load_file s =
	let input_file = open_in s in 
	let file_content = really_input_string input_file (in_channel_length input_file) in file_content

let save_to_txt s filename = 
	let write_file = open_out filename in 
	fprintf write_file "%s\n" s;
	close_out write_file 

let _ =
	let input = read_line () in  
	let filename = extract_filename input in 
	let readcontent = load_file filename in 
	let len = String.length filename in 
	let out_filename = ((String.sub filename 0 (len-3)) ^ ".txt") in 
	save_to_txt readcontent out_filename

