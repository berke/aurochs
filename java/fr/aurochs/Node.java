package fr.aurochs;

import java.util.*;
import java.io.*;
import java.lang.*;
import java.lang.reflect.*;

public class Node extends Tree {
  String name;
  LinkedList<Attribute> attributes;
  LinkedList<Tree> children;

  Node(String name) {
    this.name = name;
    attributes = new LinkedList<Attribute>();
    children = new LinkedList<Tree>();
  }

  void addToken(int begin, int end, byte[] exp) {
    children.add(new Token(begin, end, exp));
  }

  void addAttribute(String name, int start, int end, byte[] value) {
    attributes.add(new Attribute(name, start, end, value));
  }

  void addConstantAttribute(String name, int length, byte[] value) {
    attributes.add(new Attribute(name, length, value));
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

  @SuppressWarnings("unchecked") // Comment this for Java 1.5 or earlier
  public Object instantiate(String pkg) throws Exception
  {
    Class c = Class.forName(pkg + "." + name);

    Object node = c.newInstance();

    Field f = c.getField("contents");
    LinkedList l = (LinkedList) f.get(node);

    for(Tree t : children)
      l.add(t.instantiate(pkg));

    for(Attribute attr : attributes)
      attr.instantiate(node, c);

    return node;
  }
}
