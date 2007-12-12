(* Examples *)

let examples = [
  "Arithmetic",
  ("\
int ::= <Int>{ [0-9]+ }</Int>;

space ::= [ \\n\\t]*;

outfixing space do
  start ::= sum EOF;

  sum ::= <Sum> term '+' sum </Sum> | term;

  term ::= <Prod> simple '*' term </Prod> | simple;

  simple ::= int | '(' sum ')';
done;",
  "1+2*3");

  "Words",
  ("\
foo ::= \"foo\" | \"bar\";
word ::= <Foo> kind:foo </Foo> | <Word> { [a-zA-Z]+ } </Word> ;
space ::= [\\n\\t\\r ]+;
junk ::= <Junk> { sigma * } </Junk>;
punctuation ::= <Punctuation>kind:[.,;!?]+</Punctuation>;
phrase ::= <Phrase> word ( space word )* space? punctuation? </Phrase>;
start ::= space* (phrase space*)* junk EOF;",
  "A bar is a particular kind of foo.\nThis is true regardless of the phase of the moon.");

  "Integers",
  ("\
hex ::= <Hexadecimal>\"0x\" {[0-9a-fA-F]+} </Hexadecimal>;
dec ::= <Decimal> {[1-9][0-9]*} </Decimal>;
bin ::= <Binary> \"0b\" {[01]+}</Binary>;
oct ::= <Octal> \"0o\" {[0-7]+}</Octal>;
integer ::= bin | oct | hex | dec;
space ::= [\\n\\t\\r ]+;
start ::= space? (integer space?)* EOF;",
  "394 0xdeadbeef 0o337 0b001010101 213891238");
]
