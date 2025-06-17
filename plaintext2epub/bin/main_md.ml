open Printf

let run cmd = 
  let code = Sys.command cmd in
  if code <> 0 then
    (eprintf "Command failed (%d): %s\n%!" code cmd; exit code)

let write path contents = 
  let oc = open_out_bin path in
  output_string oc contents; close_out oc

let () =
  (* --- 1. CLI parsing --- *)
  let infile = ref "" in
  let outfile = ref "book.epub" in
  let speclist = [
    ("-o", Arg.Set_string outfile, "Output EPUB name (default: book.epub)");
  ] in
  Arg.parse speclist (fun s -> infile := s) "usage: plaintext2epub [-o out.epub] input.md";
  if !infile = "" then (Arg.usage speclist "missing input"; exit 1);

  let parent_dir = Filename.dirname (Sys.getcwd ()) in
  let temp = Filename.concat parent_dir "pte_build" in
  let cmd = Printf.sprintf "rm -rf %s && mkdir -p %s/META-INF %s/OEBPS" temp temp temp in
  run cmd;