/* Aurochs
 */

package fr.aurochs;

import java.util.*;
import java.io.*;

abstract class Tree {
  static void putIndent(PrintStream out, int count) {
    for(int i = 0; i < count; i ++) {
      out.printf("  ");
    }
  }
  abstract void print(PrintStream out, int indent, String input);
}

class Attribute {
  String name;
  int start;
  int end;

  Attribute(String name, int start, int end) {
    this.name = name;
    this.start = start;
    this.end = end;
  }
}

class Node extends Tree {
  String name;
  LinkedList<Attribute> attributes;
  LinkedList<Tree> children;

  Node(String name) {
    this.name = name;
    attributes = new LinkedList<Attribute>();
    children = new LinkedList<Tree>();
  }

  void addToken(int begin, int end) {
    children.add(new Token(begin, end));
  }

  void addAttribute(String name, int start, int end) {
    attributes.add(new Attribute(name, start, end));
  }

  void addChildren(Tree t) {
    children.add(t);
  }

  void print(PrintStream out, int indent, String input) {
    putIndent(out, indent);

    out.printf("<%s", name);
    for(Attribute a : attributes) {
      out.printf(" %s=\"%s\"", a.name, input.substring(a.start, a.end));
    }
    out.printf(">\n");

    for(Tree t : children) {
      t.print(out, indent + 1, input);
    }

    putIndent(out, indent);
    out.printf("</%s>\n", name);
  }
}

class Token extends Tree {
  int begin;
  int end;

  Token(int begin, int end) {
    this.begin = begin;
    this.end = end;
  }

  void print(PrintStream out, int indent, String input) {
    putIndent(out, indent);
    out.printf("%s\n", input.substring(begin, end));
  }
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
