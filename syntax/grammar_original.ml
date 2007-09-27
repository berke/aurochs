(* Grammar *)

module B = Boolean;;
open Peg;;

(** BNF-style notation for PEGs *)

(* alpha ::= 'A'..'Z'|'a'..'z'
   num ::= '0'..'9'
   ident ::= (alpha|'_')(alpha|'_'|num)*
   grammar ::= prod+
   prod ::= ident "::=" expr ";"
   expr ::=
     "epsilon"
   | "tokenize" "(" expr ")"
   | ident ":" expr
   | expr expr
   | ident
   | "<"ident">" expr "</"ident">"
   | expr "|" expr
   | expr "&" expr
   | expr "!" expr
   | expr "?"
   | expr "*"
   | expr "+"
   | "(" expr ")" *)

let any = C B.True;;
let chars = function
  | [c] -> C(B.Atom(One c))
  | cl -> C(B.Atom(Many cl))
;;
let token op = S[N"space"; op; N"space"];;
let k_char name = token (S[A"'"; Ascribe(name, Or[(N"char_escape");S[Not (N"char_escape");any]]); A"'"]);;
let k_string name =
 token (S[A"\"";
          Ascribe(name,
                  Star(
                    S[Not(C(B.Atom(One '"')));
                      Or[
                         N"string_escape";
                         (*Not(string_escape);*)
                         C(B.Not(B.Atom(One '"')))]]));
          A"\""]);;

let peg = [
"left_comment", A"(*";
"right_comment", A"*)";
"comment", S[(N"left_comment"); Star(Or[N"comment"; S[Not (N"right_comment"); (any)]]); (N"right_comment")];
"space", Star(Or[(N"comment"); chars [' ';'\t';'\n';'\r']]);
"alpha", C(B.Or[B.Atom(Range('a','z')); B.Atom(Range('A','Z'))]);
"decimal", C(B.Atom(Range('0','9')));
"bracketed_escape", S[chars['\\']; chars ['n';'r';'t';'b';'\\';'-';'[';']']];
"char_escape", S[chars['\\']; chars ['n';'r';'t';'b';'\'';'\\']];
"string_escape", S[chars['\\']; chars ['n';'r';'t';'b';'"';'\\']];
"core_ident", S[Or[(N"alpha");chars['_']]; Star(Or[(N"alpha");(N"decimal");chars['_']])];
"ident", token (Ascribe("name", (N"core_ident")));
"op_sigma", token (A"sigma");
"op_epsilon", token (A"epsilon");
"op_eof", token (A"EOF");
"op_or", token (A"|");
"op_and", token (A"&");
"op_not", token (A"~");
"op_star", token (A"*");
"op_plus", token (A"+");
"op_option", token (A"?");
"op_produces", token (A"::=");
"op_colon", token (A":");
"op_semicolon", token (A";");
"op_l_bracket", S[(N"space"); (A"["); Not(A"^")];
"op_l_n_bracket", S[(N"space"); (A"["); A"^"];
"op_r_bracket", S[A"]"; (N"space")];
"op_l_paren", token (A"(");
"op_r_paren", token (A")");
"op_l_brace", token (A"{");
"op_r_brace", token (A"}");
"opening_tag", token (S[A"<"; Ascribe("open", (N"core_ident")); A">"]);
"closing_tag", token (S[A"</"; Ascribe("close", (N"core_ident")); A">"]);
"expr",
 Or[
     Build("Or",    [(N"sequencable"); (N"op_or"); (N"expr")]);
     Build("And",   [(N"suffixable_expr"); (N"op_and"); (N"expr")]);
     N"sequencable" ];
"sequencable",
 Or[
   Build("S",     [(N"suffixable_expr"); (N"sequencable")]);
   N"suffixable_expr"
 ];
"suffixable_expr",
 Or[
   Build("Option",[(N"simple_expr"); (N"op_option")]);
   Build("Plus",  [(N"simple_expr"); (N"op_plus")]);
   Build("Star",  [(N"simple_expr"); (N"op_star")]);
   Build("Not",   [(N"op_not"); (N"suffixable_expr")]);
   N"simple_expr";
 ];
"simple_expr",
 Or[ S[(N"op_l_paren"); (N"expr"); (N"op_r_paren")];
     Build("Build", [(N"opening_tag"); (N"expr"); (N"closing_tag")]);
     Build("Tokenize",[(N"op_l_brace"); (N"expr"); (N"op_r_brace")]);
     Build("Epsilon", [(N"op_epsilon")]);
     Build("EOF",     [(N"op_eof")]);
     Build("Sigma",   [(N"op_sigma")]);
     Build("Ascribe", [(N"ident"); (N"op_colon"); N"suffixable_expr"]);
     Build("String",  [k_string "value"]);
     Build("Char",    [k_char "value"]);
     Build("N",       [(N"ident")]);
     N"characters"
   ];
"bracketed_char_spec", 
 Or[
     Build("Range",[Ascribe("low", (N"char_spec"));
                    chars['-'];
                    Ascribe("high", (N"char_spec"))]);
     Build("Char", [Ascribe("value", (N"char_spec"))])
];
"char_spec",
 Or[N"bracketed_escape";
    C(B.Not(B.Atom(Many['[';'\\';'-';']';'\n';'\t';'\r';'\b'])))];
"characters",
 Or[
   Build("Chars",
     [(N"op_l_bracket");
      Star (N"bracketed_char_spec");
      (N"op_r_bracket")]);
   Build("NotChars",
     [(N"op_l_n_bracket");
      Star (N"bracketed_char_spec");
      (N"op_r_bracket")])];
"production",
 Build("Production",
       [(N"ident");
        (N"op_produces");
        (N"expr");
        (N"op_semicolon")]);
"grammar", Build("Grammar", [Plus((N"production"))]);
"start", (N"grammar");
];;
