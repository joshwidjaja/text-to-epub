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

  (* --- 2. Setup temp dir --- *)
  let parent_dir = Filename.dirname (Sys.getcwd ()) in
  let temp = Filename.concat parent_dir "pte_build" in
  let cmd = Printf.sprintf "rm -rf %s && mkdir -p %s/META-INF %s/OEBPS" temp temp temp in
  run cmd;

  (* --- 3. Generate chapter xhtml --- *)

  (* --- 4. Static support files --- *)
  write (temp ^ "/mimetype") "application/epub+zip";
  write (temp ^ "/META-INF/container.xml") 
  {|<?xml version="1.0" encoding="UTF-8"?>
  <container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
  <rootfiles>
    <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
  </rootfiles>
  </container>|};

  write (temp ^ "/OEBPS/content.opf")
  {|<?xml version="1.0" encoding="UTF-8"?>
  <package version="3.0"
        xmlns="http://www.idpf.org/2007/opf"
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:dcterms="http://purl.org/dc/terms/"
        unique-identifier="bookid">
    <metadata>
        <dc:identifier id="bookid">urn:uuid:12345678-1234-1234-1234-123456789abc</dc:identifier>
        <dc:title>Output</dc:title>
        <dc:language>en</dc:language>
        <meta property="dcterms:modified">2025-06-12T03:15:00Z</meta>
    </metadata>

    <manifest>
        <item id="nav" properties="nav" href="nav.xhtml" media-type="application/xhtml+xml"/>
        <item id="chapter1" href="chapter01.xhtml" media-type="application/xhtml+xml"/>
        <item id="css" href="stylesheet.css" media-type="text/css"/>
    </manifest>

    <spine page-progression-direction="ltr">
        <itemref idref="chapter1"/>
    </spine>
  </package>|};

  write (temp ^ "/OEBPS/nav.xhtml")
  {|<?xml version="1.0" encoding="UTF-8"?>
  <html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops" lang="en">
      <head>
        <title>Contents</title>
          <meta charset="UTF-8"/>
          <meta name="viewport" content="width=device-width"/>
      </head>
      <body>
          <nav epub:type="toc" id="toc">
              <h2>Contents</h2>
              <ol>
                  <li><a href="chapter01.xhtml">Test</a></li>
              </ol>
          </nav>
      </body>
  </html>|};

  write (temp ^ "/OEBPS/stylesheet.css") ("");

  (* --- 5. Zip + validate --- *)
  let cwd = Sys.getcwd () in
  Sys.chdir temp;
  run (sprintf "zip -X0 ../%s mimetype" !outfile);
  run (sprintf "zip -Xr9D ../%s *" !outfile);
  run (sprintf "epubcheck ../%s" !outfile);
  Sys.chdir cwd;
  printf "Wrote %s\n%!" !outfile