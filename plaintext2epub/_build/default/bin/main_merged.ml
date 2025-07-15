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
  Printf.sprintf {|<?xml version="1.0" encoding="utf-8"?>
  <html xmlns="http://www.w3.org/1999/xhtml" lang="en">
    <head>
        <title>Chapter 1</title>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width" />
    </head>
    <body>
      %s
    </body>
  </html>|} body

let xhtml_of_md md_string = 
  let body_frag = Omd.to_html (Omd.of_string md_string) in
  Printf.sprintf {|<?xml version="1.0" encoding="utf-8"?>
  <html xmlns="http://www.w3.org/1999/xhtml" lang="en">
    <head>
      <title>Chapter 1</title>
      <meta charset="utf-8"/>
      <link rel="stylesheet" type="text/css" href="stylesheet.css"/>
    </head>
    <body>
      %s
    </body>
  </html>|} body_frag

(* --- Chapter abstraction --- *)
module type SOURCE_WITH_SPLIT = sig
  type source (* the chunk of data being handled *)
  val read_file: string -> source
  val split: source -> source list
  val to_xhtml: source -> string
end

module PlainText: SOURCE_WITH_SPLIT with type source = string list = struct
  type source = string list

  let read_file path =
    let ic = open_in path in
    let rec gather acc =
      match input_line ic with
      | line -> gather (line :: acc)
      | exception End_of_file ->
        close_in ic; List.rev acc
    in
    gather []

  (* split at \f (form-feed) *)
  let split (doc: string list) =
    let rec aux current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | "" :: rest -> aux [] (List.rev current :: acc) rest
    | line :: rest when String.starts_with ~prefix:"\012" line
      -> aux [] (List.rev current :: acc) rest
    | line :: rest -> aux (line :: current) acc rest
    in
    aux [] [] doc
  
  let to_xhtml lines = xhtml_of_lines lines
end

module Markdown: SOURCE_WITH_SPLIT with type source = string = struct
  type source = string

  let read_file path =
    In_channel.with_open_bin path In_channel.input_all

  (* split at H1 headings *)
  let split (doc: string) = 
    let re = Str.regexp "^# " in
    let buf = Buffer.create 256 in
    let flush acc =
      let frag = Buffer.contents buf in
      Buffer.clear buf;
      if frag = "" then acc else frag :: acc
    in
    let lines = String.split_on_char '\n' doc in
    List.fold_left
      (fun acc l ->
        if Str.string_match re l 0
        then (Buffer.add_string buf (l ^ "\n"); flush acc)
        else (Buffer.add_string buf (l ^ "\n"); acc))
      [] lines
    |> flush |> List.rev

  let to_xhtml md = xhtml_of_md md
end

module GenEPub (S: SOURCE_WITH_SPLIT) = struct
  let get_time () =
    let tm = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
      tm.tm_hour tm.tm_min tm.tm_sec

  let build ~(title: string) ~(outfile: string) (doc: S.source) =
    (* --- 2. Setup temp dir --- *)
    let parent_dir = Filename.dirname (Sys.getcwd ()) in
    let tmp = Filename.concat parent_dir "pte_build" in
    run (Printf.sprintf
      "rm -rf %s && mkdir -p %s/META-INF %s/OEBPS"
      tmp tmp tmp);

    let meta = Filename.concat tmp "META-INF" in
    let oebps = Filename.concat tmp "OEBPS" in

    write (Filename.concat tmp "mimetype") "application/epub+zip";

    write (Filename.concat oebps "stylesheet.css") "";

    write (Filename.concat meta "container.xml")
      {|<?xml version="1.0" encoding="UTF-8"?>
  <container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
  <rootfiles>
    <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
  </rootfiles>
  </container>|};

    (* --- 3. Generate chapter xhtml --- *)
    let chapters: S.source list = S.split doc in
    List.iteri
      (fun idx chap ->
        let fname = Printf.sprintf "chapter%02d.xhtml" (idx + 1) in
        let path = Filename.concat oebps fname in
        write path (S.to_xhtml chap))
      chapters;
    
    (* --- 4. DYNAMIC support files --- *)
    let nav_items =
      List.mapi
        (fun idx _ ->
          let href = Printf.sprintf "chapter%02d.xhtml" (idx + 1) in
          Printf.sprintf "<li><a href=\"%s\">Chapter&#160;%d</a></li>"
                          href (idx + 1))
        chapters
      |> String.concat "\n"
    in
    write (Filename.concat oebps "nav.xhtml")
      (Printf.sprintf {|<?xml version="1.0" encoding="utf-8"?>
<html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops">
  <head><title>%s</title></head>
  <body>
    <nav epub:type="toc" id="toc">
      <ol>
%s
      </ol>
    </nav>
  </body>
</html>|} title nav_items);

    let uuid =
      let g = Uuidm.v4_gen (Random.State.make_self_init ()) in
      Uuidm.to_string (g ())
    in
    let manifest, spine =
      List.mapi
        (fun idx _ ->
          let id = Printf.sprintf "c%02d" (idx + 1) in
          let href = Printf.sprintf "chapter%02d.xhtml" (idx + 1) in
          (Printf.sprintf
            {|<item id="%s" href="%s" media-type="application/xhtml+xml"/>|} id href,
          Printf.sprintf {|<itemref idref="%s"/>|} id))
        chapters
      |> List.split
    in
    let opf =
      Printf.sprintf {|<?xml version="1.0" encoding="utf-8"?>
<package version="3.0"
         xmlns="http://www.idpf.org/2007/opf"
         unique-identifier="bookid">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:identifier id="bookid">urn:uuid:%s</dc:identifier>
    <dc:title>%s</dc:title>
    <dc:language>en</dc:language>
    <meta property="dcterms:modified">%s</meta>
  </metadata>
  <manifest>
%s
    <item id="nav" href="nav.xhtml"
          media-type="application/xhtml+xml" properties="nav"/>
    <item id="style" href="stylesheet.css" media-type="text/css" />
  </manifest>
  <spine>
%s
  </spine>
</package>|}
      uuid title
      (get_time ())
      (String.concat "\n" manifest)
      (String.concat "\n" spine)
    in
    write (Filename.concat oebps "content.opf") opf;

    (* --- 5. Zip + validate --- *)
    let abs_out = Filename.concat (Sys.getcwd ()) outfile in
    run (Printf.sprintf
          "cd %s && \
          zip -X0 %s mimetype && \
          zip -r9D %s META-INF OEBPS"
          tmp abs_out abs_out);

    run (Printf.sprintf "epubcheck %s" abs_out)
  
  let build_file ~title ~outfile path =
    let doc = S.read_file path in
    build ~title ~outfile doc
  end

let () =
  (* --- 1. CLI parsing --- *)
  let infile = ref "" in
  let outfile = ref "book.epub" in
  let title = ref "Untitled" in
  let speclist = [
    "-o", Arg.Set_string outfile, "Output EPUB name (default: book.epub)";
    "-t", Arg.Set_string title, "Book title (default: Untitled)"
  ] in
  let usage = "Usage: text2epub [options] input{txt,md}" in
  Arg.parse speclist (fun s -> infile := s) usage;
  if !infile = "" then (Arg.usage speclist usage; exit 1);

  (* --- 2. Check for file type --- *)
  (* --- note: try abstracting input routines for source ---*)
  if Filename.check_suffix !infile ".md" then
    (let module B = GenEPub (Markdown) in
    B.build_file ~title:!title ~outfile:!outfile !infile)
  else
    (let module B = GenEPub (PlainText) in
    B.build_file ~title:!title ~outfile:!outfile !infile)