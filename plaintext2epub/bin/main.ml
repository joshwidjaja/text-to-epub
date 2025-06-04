open Printf

let run cmd = 
  let code = Sys.command cmd in
  if code <> 0 then
    (eprintf "Command failed (%d): %s\n%!" code cmd; exit code)
  
let write path contents = 
  let oc = open_out_bin path in
  output_string oc contents; close_out oc

let xhtml_of_lines lines =
  let body = lines
  |> List.map (fun l -> Printf.sprintf " <p>%s</p>" (String.escaped l))
  |> String.concat "\n" in
  Printf.sprintf {|<?xml version="1.0 encoding="utf-8"?>
  <html xmlns="http://www.w3.org/1999/xhtml">
  <head><title>Chapter 1</title></head>
  <body>
  %s
  </body>
  </html>|} body