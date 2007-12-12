(* Cgi-bin *)

open Aurochs_pack
open Cgi
open Xml
open Pffpsf

let error_xml msg =
  html_xml
    (
      N("html",
        [{n="xmlns";    v=S"http://www.w3.org/1999/xhtml"};
         {n="lang";     v=S"en"};
         {n="xml:lang"; v=S"en"}],
        [
          N("head", [],
            [
              U(N("title", [], [D"The Aurochs parser generator"]));
              O("meta",
                [{n="http-equiv"; v=S"Content-Type"};
                 {n="content";    v=S"text/html; charset=utf-8"}]);
              O("meta",
                [{n="name";       v=S"keyword"};
                 {n="content";    v=S"Aurochs, parse expression grammar, PEG, parsing, Ocaml, Java, C, memoization"}]);
              O("link",
                [{n="rel";       v=S"stylesheet"};
                 {n="type";      v=S"text/css"};
                 {n="href";      v=S"default.css"};
                 {n="media";     v=S"screen"}]);
              O("link",
                [{n="rel";       v=S"icon"};
                 {n="type";      v=S"image/png"};
                 {n="href";      v=S"/icon.png"}])
            ]
          );
          N("body", [],
            [
              U(N("h1", [], [D"Error"]));
              U(N("p", [],  [D msg]))
            ]
          );
        ]
      )
    )

let _ =
  match invocation_method () with
  | GET | POST ->
      reply_html
        (fun oc ->
          let msg = sf "Unhandled invocation method" in
          output_xml oc (error_xml msg))

  (*
  let peg = Sys.argv.(1) in
  for i = 2 to Array.length Sys.argv - 1 do
    begin
      let fn = Sys.argv.(i) in
      Printf.printf "%s\n%!" fn;
      try
        let t = Aurochs.read_positioned ~grammar:(`Binary(`File peg)) ~text:(`File fn) in
        let fn' = (Filename.chop_extension (Filename.basename fn))^".bin" in
        Util.with_binary_file_output fn' (fun oc -> Marshal.to_channel oc t []);
      with
      | Aurochs.Parse_error n -> Printf.printf "%s: parse error at %d\n%!" fn n
    end;
    Gc.compact ()
  done
;;*)
