/* Aurochs
 */

package fr.aurochs;

import java.util.*;

abstract class Tree { }

class Attribute {
  String name;
  int start;
  int end;
}

class Node extends Tree {
  String name;
  LinkedList<Attribute> attributes;
  LinkedList<Node> Children;
}

class Token extends Tree {
  int start;
  int end;
}

class ParseError extends Exception {
  int position;
}

class Parser {
  private long program;

  private native long unpack(byte[] nog);

  Parser(byte[] nog) {
    program = unpack(nog);
  }

  native Tree parse(String u) throws ParseError;

  static
  {
    System.loadLibrary("aurochs_java");
  }
}
