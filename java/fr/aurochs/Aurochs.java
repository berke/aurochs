/* Aurochs
 */

package fr.aurochs;

import java.util.*;
import java.io.*;
import java.lang.reflect.*;

abstract class Tree {
  static void putIndent(PrintStream out, int count) {
    for(int i = 0; i < count; i ++) {
      out.printf("  ");
    }
  }
  abstract void print(PrintStream out, int indent, String input);
  abstract Object instantiate(String pkg) throws Exception;
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

  void print(PrintStream out, String input) {
     out.printf("%s=\"", name);
     if(start < end) {
       out.printf("%s\"", input.substring(start, end));
     } else {
       out.printf("%d\"", start);
     }
  }
  
  public void instantiate(Object self, Class c)
  {
    try
    {
      Method m = c.getMethod(name + "FromString");

      m.invoke(self, "FIXME");

      return;
    }
    catch(Exception e)
    {
    }

    try
    {
      Field f = c.getField(name);
      Class f_t = f.getType();


    }
    catch(Exception e)
    {
    }
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
      out.printf(" ");
      a.print(out, input);
    }

    if(children.isEmpty()) {
      out.printf("/>\n");
    } else {
      out.printf(">\n");

      for(Tree t : children) {
        t.print(out, indent + 1, input);
      }

      putIndent(out, indent);
      out.printf("</%s>\n", name);
    }
  }

  public Object instantiate(String pkg) throws Exception
  {
    Class c = Class.forName(name);

    Object node = c.newInstance();

    Field f = c.getField("content");
    Vector a = (Vector) f.get(node);

    for(Tree t : children)
      a.add(t.instantiate(pkg));

    for(Attribute attr : attributes)
      attr.instantiate(node, c);

    return node;
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

  public Object instantiate(String pkg) throws Exception
  {
    Object obj = new Token(0, 0);

    return obj;
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
