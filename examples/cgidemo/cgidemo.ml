(* Cgi-bin *)

open Aurochs_pack
open Peg
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

let br = O("br",[])

let paragraph x = N("p",[],x)

let div taxon child = N("div", [{n="class"; v=S taxon}], child)

let span taxon child = N("span", [{n="class"; v=S taxon}], child)

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

type 'a model =
  {
    m_grammar : string;
    m_input   : string;
    m_output  : 'a
  }

let model0 =
  {
    m_grammar = "\
foo ::= \"foo\" | \"bar\";
word ::= <Foo> kind:foo </Foo> | <Word> { [a-zA-Z]+ } </Word> ;
space ::= [\\n\\t\\r ]+;
junk ::= <Junk> { sigma * } </Junk>;
punctuation ::= <Punctuation>kind:[.,;!?]+</Punctuation>;
phrase ::= <Phrase> word ( space word )* punctuation? </Phrase>;
start ::= space* (phrase space*)* junk EOF;";
    m_input = "A bar is a particular kind of foo.
This is true regardless of the phase of the moon.";
    m_output = paragraph [D"Welcome to the Aurochs parser generator on-line demonstration!  Please feel comfortable and try a few grammars."]
  }

let reply h = reply_html (fun oc -> output_xml oc (make_html h))

let view model =
  reply
    [
      U(N("h1", [], [D"Aurochs parser"]));
      N("form", [{n="action"; v=S"demo.cgi"}; {n="method";v=S"post"}],
        [
          paragraph [D"PEG grammar:"];
          textarea ~name:"grammar" ~rows:25 ~cols:80 ~content:model.m_grammar ();
          br;
          paragraph [D"Input:"];
          textarea ~name:"input"   ~rows:5 ~cols:80 ~content:model.m_input ();
          br;
          submit ~name:"submit" ~value:"Parse" ()
        ]
      );
      model.m_output
    ]

let grammar_limit = 1000
let input_limit = 1000

let compute ~grammar ~input () =
  let model =
    {
      model0 with
      m_grammar = grammar;
      m_input   = input;
    }
  in
  if String.length grammar > grammar_limit then
    { model with m_output = paragraph [D"Grammar too big for on-line version"] }
  else if String.length input > input_limit then
    { model with m_output = paragraph [D"Input too big for on-line version"] }
  else
    try
      let t = Aurochs.see ~grammar:(`Source(`String grammar)) ~text:(`String input) in
      let rec loop = function
        | Token t -> div "token" [D t]
        | Node(name, attrs, child) ->
            let attrs' =
              List.map
                (fun (aname, aval) -> span "attribute" [D aname; D"="; D aval])
                attrs
            in
            match child with
            | [] ->
                div "node"
                  [span "node-name"
                    (List.concat[
                      [D("<" ^ name)];
                      attrs';
                      [D("/>")]
                    ])
                  ]
            | _ ->
                div "node"
                  (List.concat
                    [
                      [span "node-name"
                        (List.concat[
                          [D("<" ^ name)];
                          attrs';
                          [D">"]
                        ])];
                      List.map loop child;
                      [span "node-name" [D("</" ^ name ^">")]];
                    ]
                  )
      in
      let output = loop t in
      { model with m_output  = div "tree" [output] }
    with
    | Aurochs.Parse_error n ->
        { model with m_output  = paragraph [D(sf "Parse error at %d" n)] }
    | x ->
        { model with m_output  = paragraph [D(sf "Exception: %s" (Printexc.to_string x))] }

let _ =
  (*let host = remote_host in*)
  match invocation_method () with
  | GET -> view model0
  | POST ->
      let form = Form.parse_form_from_stream (Stream.of_channel stdin) in
      let gs key = Form.get_value form Form.to_string key in
      let model = compute ~grammar:(gs "grammar") ~input:(gs "input") () in
      view model
