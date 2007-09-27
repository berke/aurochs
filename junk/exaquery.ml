(* Exaquery *)

open Peg;;

let space = Star(Or[A" ";A"\t";A"\n";A"\r"]);;
let decimal = Class['0','9'];;
let alpha =
  Or[ Class['a','z'; 'A','Z'];
      A"_" ]
;;
let alphanum =
  Or[ alpha;
      decimal ]
;;
let regexp_chars = Or[alphanum; space];;
let token ?exclude op =
  S[space; op;
      begin
        match exclude with
        | None -> Epsilon
        | Some x -> Not x
      end;
    space]
;;
let lex_regexp = Build("Rex",
                   [A "/";
                    Tokenize(Star regexp_chars);
                    A "/";
                    Tokenize(Star(Or([A "i"; A "w"; A "b"])))])
and lex_quoted = S[A "\"";
                   Tokenize(Star regexp_chars);
                   A "\""]
and lex_word = Or
  [
   Build("ExactWord",     [A"+"; Tokenize(Plus alphanum)]);
   Build("Synonym",       [A"~"; Tokenize(Plus alphanum)]);
   Build("SharpPrefixed", [A"#"; Tokenize(Plus alphanum)]);
   Build("SharpSuffixed", [Tokenize(Plus alphanum); A"#"]);
   Build("StarPrefixed",  [A"*"; Tokenize(Plus alphanum)]);
   Build("StarSuffixed",  [Tokenize(Plus alphanum); A"*"]);
   Build("Word",          [Tokenize(Plus alphanum)])
   ]
;;

let token_a x = token (A x);;
let token_alpha_a x = token ~exclude:alphanum (A x);;

let ident  = S[alpha; Star alphanum];;

let lex_ident  = Tokenize ident;;

let any = Class['\000','\255'];;

let lex_string = S[A"\""; Tokenize(Star(S[Not(Chars['"']);any])); A"\""];;

let
rec lex_atom   = S[Not lex_oper; token (Or[lex_regexp; lex_quoted; lex_word; lex_string])]
and lex_and    = Or(List.map token_alpha_a ["AND";"and";"And"])
and lex_or     = Or(List.map token_alpha_a ["OR";"or";"Or"])
and lex_not    = Or(List.map token_alpha_a ["NOT";"not";"Not"])
and lex_near2  = Or(List.map token_alpha_a ["NEAR";"near";"Near"])
and lex_next   = Or(List.map token_alpha_a ["NEXT";"next";"Next"])
and lex_integer = S[Opt(A"-");Plus decimal]
and lex_float  = S[lex_integer;Opt(S[A".";Plus decimal]);Opt(S[A"e";Opt(Or[A"-";A"+"])]);Plus decimal]
and lex_near   = Or[S[lex_near2; A"/"; Ascribe("depth", lex_integer)];
                    lex_near2]
and lex_oper   =
  Or[
      lex_and;
      lex_or;
      lex_not;
      lex_near;
      lex_next;
      lex_comparison
]
and lex_date_or_value = Or[Build("Date", [Tokenize lex_date]); lex_value]
and lex_value =
  Or[Build("Float", [Tokenize lex_float]);
     Build("Int", [Tokenize lex_integer])]
and lex_date =
  Or[ S[decimal;decimal;decimal;decimal;A"-";decimal;decimal;A"-";decimal;decimal];
      S[decimal;decimal;A"/";decimal;Opt decimal;A"/";Opt(S[decimal;decimal]);decimal;decimal;];
      S[decimal;decimal;A".";decimal;decimal;A".";Opt(S[decimal;decimal]);decimal;decimal;]]
and lex_comparison =
  S[space;
    Ascribe("operator", Or(List.map (fun x -> A x) ["<"; "<="; "=="; ">="; ">"; "!="]));
    space]
and lex_field  = S[Ascribe("name", lex_ident); A":"]
and lex_group = Build("Group", [token (A"("); N lex_d; token (A")")])
and lex_simple2 = Or[N lex_atom; N lex_group]
and lex_simple = Or[Build("FieldNode", [lex_field; lex_simple2]);
                    N lex_simple2;
                    ]
and lex_d      = Or[Build("Or",       [N lex_c; N lex_or; N lex_d]);
                    Build("Near",     [N lex_c; N lex_near; N lex_d]);
                    Build("Next",     [N lex_c; N lex_next; N lex_d]);
                    Build("Compare",  [N lex_ident; N lex_comparison; N lex_date_or_value]);
                    N lex_c;
                    ];
and lex_c1      = Or[lex_group;
                     Build("And",  [N lex_simple; N lex_and; N lex_c]);
                     Build("Not",  [N lex_not; N lex_simple]);
                     Build("Seq",  [N lex_simple; N lex_c]);
                     N lex_simple
                    ]
and lex_c       = Or[N lex_c1;
                     Build("Unrecognized", [Tokenize(Plus(S[Not lex_c1; any]))])]
and lex_start  = Build("Expr", [N lex_d])
;;
