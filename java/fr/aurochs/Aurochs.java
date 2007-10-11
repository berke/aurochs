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
  ParseError(int x) {
    position = x;
  }
}

class NOGexception extends Exception { }

class Parser {
  private long program;

  private native long unpack(byte[] nog);

  Parser(byte[] nog) throws NOGexception {
    program = unpack(nog);
    System.out.printf("Program = %d\n", program);
    if(program == 0) {
      throw new NOGexception();
    }
  }

  native Tree parse(byte[] u) throws ParseError;

  static
  {
    System.loadLibrary("aurochsjava");
  }
}
