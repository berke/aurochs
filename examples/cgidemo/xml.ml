(* Xml *)

type 'a t =
  | O of name * attribute list             (** <foo> *)
  | C of name * attribute list             (** <foo/> *)
  | N of name * attribute list * 'a t list (** <foo> bar </foo> *)
  | U of 'a t                              (** Unindented *)
  | D of 'a                                (** data node *)
and attribute = { n : name; v : value }
and name = string
and value =
  | I of int
  | S of string
  | B of bool

type 'a xml = {
  xml_version : string;
  xml_doctype : string;
  xml_data : 'a t
}

let fp = Printf.fprintf

let output_name oc n = output_string oc n

let output_indent oc indent =
  match indent with
  | None -> ()
  | Some n ->
      for i = 1 to n do
        fp oc "  "
      done

let output_quoted_string oc u =
  let m = String.length u in
  for i = 0 to m - 1 do
    match u.[i] with
    | '"' -> fp oc "&quot;"
    | '<' -> fp oc "&lt;"
    | '>' -> fp oc "&gt;"
    | c   -> output_char oc c
  done
  
let output_value oc = function
  | I x -> fp oc "\"%d\"" x
  | B b -> fp oc "\"%b\"" b
  | S u -> fp oc "\"%a\"" output_quoted_string u

let output_node oc (nm, al) =
  fp oc "%s" nm;
  match al with
  | [] -> ()
  | _ -> List.iter (fun a -> fp oc " %a=%a" output_name a.n output_value a.v) al

let output_data oc x = output_quoted_string oc x;;

let next_indent = function
  | None -> None
  | Some n -> Some(n + 1)

let output_nl oc = function
  | None -> ()
  | Some _ -> fp oc "\n"

let rec output_t n oc x =
  match x with
  | O(nm, al) -> fp oc "%a<%a>%a" output_indent n output_node (nm, al) output_nl n
  | C(nm, al) -> fp oc "%a<%a/>%a" output_indent n output_node (nm, al) output_nl n
  | D d -> fp oc "%a%a%a" output_indent n output_quoted_string d output_nl n
  | N(nm, al, yl) ->
      fp oc "%a<%a>%a" output_indent n output_node (nm, al) output_nl n;
      List.iter (output_t (next_indent n) oc) yl;
      fp oc "%a</%a>%a" output_indent n output_name nm output_nl n
  | U x ->
      output_indent oc n;
      output_t None oc x;
      output_nl oc n

let output_xml oc x =
  fp oc "<?xml version=\"%s\">\n" x.xml_version;
  fp oc "<!DOCTYPE %s>\n" x.xml_doctype;
  output_t (Some 0) oc x.xml_data

let html_xml x =
  {
    xml_version = "1.0";
    xml_doctype = "html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"";
    xml_data    = x
  }

module Test =
  struct
    let xml0 =
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
                  U(N("h1", [], [D "Introduction"]));
                  U(N("p", [],
                    [D"Since the dawn of time, humans have done this and that. \
                       Not only this, but also that! \
                       Guess what. \
                       They were wrong. \
                       I am right. \
                       Not only this, but also that! \
                       Guess what. \
                       They were wrong. \
                       I am right. \
                       Not only this, but also that! \
                       Guess what. \
                       They were wrong. \
                       I am right. \
                       Not only this, but also that! \
                       Guess what. \
                       They were wrong. \
                       I am right. \
                       Not only this, but also that! \
                       Guess what. \
                       They were wrong.";
                     N("a",[{n="href";v=S"foobar.html"}],[D"Click here to win $500!"]);
                     D"I am right. \
                       Not only this, but also that! \
                       Guess what. \
                       Not only this, but also that! \
                       Guess what. \
                       They were wrong. \
                       I am right. \
                       Not only this, but also that! \
                       Guess what. \
                       They were wrong. \
                       I am right. \
                       Not only this, but also that! \
                       Guess what. \
                       They were wrong. \
                       I am right. \
                       Guess what."]));
                ]
              );
            ]
          )
        )

    let test () = output_xml stdout xml0
  end
