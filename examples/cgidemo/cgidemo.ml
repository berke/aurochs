(* Cgi-bin *)

open Aurochs_pack
open Cgi
open Xml
open Pffpsf

let make_html x =
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
                 {n="href";      v=S"/default.css"};
                 {n="media";     v=S"screen"}]);
              O("link",
                [{n="rel";       v=S"icon"};
                 {n="type";      v=S"image/png"};
                 {n="href";      v=S"/icon.png"}])
            ]
          );
          N("body", [], x);
        ]
      )
    )

let error_html msg =
  [
    U(N("h1", [], [D"Error"]));
    U(N("p", [],  [D msg]))
  ]

let br = C("br",[])

let paragraph x = N("p",[],x)

let textarea ~name ~rows ~cols ?(content="") () =
  U(N("textarea",
    [{n="rows"; v=I rows};
     {n="cols"; v=I cols};
     {n="name"; v=S name}],
     [D content]))

let submit ~name ~value () =
  U(N("input",
    [{n="type";  v=S"submit"};
     {n="value"; v=S value};
     {n="name";  v=S name}],
     []))

type model =
  {
    m_grammar : string;
    m_input   : string;
    m_message : string
  }

let reply h = reply_html (fun oc -> output_xml oc (make_html h))

let view model =
  reply
    [
      U(N("h1", [], [D"Aurochs parser"]));
      N("form", [{n="action"; v=S"demo.cgi"}; {n="method";v=S"post"}],
        [
          paragraph [D"PEG grammar:"];
          textarea ~name:"grammar" ~rows:5 ~cols:80 ~content:model.m_grammar ();
          br;
          paragraph [D"Input:"];
          textarea ~name:"input"   ~rows:5 ~cols:80 ~content:model.m_input ();
          br;
          paragraph [D model.m_message];
          br;
          submit ~name:"submit" ~value:"Parse" ()
        ]
      )
    ]

let _ =
  let host = remote_host in
  match invocation_method () with
  | GET ->
      let model = 
        { m_grammar = "start ::= \"foo\" [ ]* \"bar\" EOF;" ;
          m_input   = "foo  bar";
          m_message = "Welcome to the Aurochs parser generator on-line demonstration!  Please feel comfortable and try a few grammars."
        }
      in
      view model
  | POST ->
      let form = Form.parse_form_from_stream (Stream.of_channel stdin) in
      let grammar = Form.get_value form Form.to_string "grammar" in 
      let input = Form.get_value form Form.to_string "input" in 
      let message = sf "Your grammar has length %d" (String.length grammar) in
      let model =
        {
          m_grammar = grammar;
          m_input   = input;
          m_message = message
        }
      in
      view model

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
