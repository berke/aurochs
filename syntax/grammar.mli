(* Grammar *)

type node_name = 
| N_Root
| N_Appending
| N_Prepending
| N_Prefixing
| N_Suffixing
| N_Outfixing
| N_Surrounding
| N_Or
| N_S
| N_And
| N_Tight
| N_Option
| N_TightPlus
| N_TightStar
| N_Plus
| N_Star
| N_Not
| N_Position
| N_Build
| N_JustTag
| N_Tokenize
| N_Epsilon
| N_EOF
| N_BOF
| N_Sigma
| N_Ascribe
| N_String
| N_Char
| N_N
| N_Range
| N_Chars
| N_NotChars
| N_Production
| N_Modifying
| N_Modifier
| N_Grammar

type attribute_name = 
| A_name
| A_open
| A_close
| A_value
| A_low
| A_high

type tree = (node_name, attribute_name) Peg.poly_tree

type positioned_tree = (node_name, attribute_name) Peg.poly_positioned_tree

val print_node_name : out_channel -> node_name -> unit
val print_attribute_name : out_channel -> attribute_name -> unit
val print_tree : out_channel -> tree -> unit
val program : (node_name, attribute_name) Nog.program
val parse_positioned : string -> positioned_tree
val parse : string -> tree
