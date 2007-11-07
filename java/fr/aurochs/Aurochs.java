/* Aurochs
 */

package fr.aurochs;

import java.util.*;
import java.io.*;
import java.lang.*;
import java.lang.reflect.*;

abstract class Tree {
  static void putIndent(PrintStream out, int count) {
    for(int i = 0; i < count; i ++) {
      out.printf("  ");
    }
  }
  abstract void print(PrintStream out, int indent, String input);
  abstract Object instantiate() throws Exception;
}

class AttributeInstantiateError extends Exception { }

class Attribute {
  String name;
  String value;
  int start;
  int end;

  Attribute(String name, int start, int end, byte[] exp) {
    this.name = name;
    this.start = start;
    this.end = end;
    this.value = new String(exp, start, end - start);
  }

  void print(PrintStream out, String input) {
     out.printf("%s=\"", name);
     if(start < end) {
       out.printf("%s\"", input.substring(start, end));
     } else {
       out.printf("%d\"", start);
     }
  }
  
  public void instantiate(Object self, Class c) throws Exception
  {
    try
    {
      Method m = c.getMethod(name + "FromString");

      m.invoke(self, value);

      return;
    }
    catch(Exception e)
    {
      // No *FromString method, going on...
    }

    try
    {
      Field f = c.getField(name);
      Class cF = f.getType();

      // Try to convert to primitive types

      if (cF == cByte)
	f.setByte(self, Byte.parseByte(value));
      else if(cF == cDouble)
	f.setDouble(self, Double.parseDouble(value));
      else if(cF == cFloat)
	f.setFloat(self, Float.parseFloat(value));
      else if (cF == cInteger)
	f.setInt(self, Integer.parseInt(value));
      else if (cF == cLong)
	f.setLong(self, Long.parseLong(value));
      else if (cF == cShort)
	f.setShort(self, Short.parseShort(value));
      else if (cF == cString)
	f.set(self, value);
      else
	// We have neither a method *FromString nor a primitive type, i give up :-(
	throw new AttributeInstantiateError();
    }
    catch(AttributeInstantiateError e)
    {
      throw e;
    }
    catch(Exception e)
    {
    }
  }

  static
  {
    cByte = new Byte("0").getClass();
    cDouble = new Double(0.).getClass();
    cFloat = new Float(0.).getClass();
    cInteger = new Integer(0).getClass();
    cLong = new Long(0).getClass();
    cShort = new Short((short) 0).getClass();
    cString = new String("").getClass();
  }

  static Class cByte;
  static Class cDouble;
  static Class cFloat;
  static Class cInteger;
  static Class cLong;
  static Class cShort;
  static Class cString;
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

  void addToken(int begin, int end, byte[] exp) {
    children.add(new Token(begin, end, exp));
  }

  void addAttribute(String name, int start, int end, byte[] exp) {
    attributes.add(new Attribute(name, start, end, exp));
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

  public Object instantiate() throws Exception
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
  String value;
  int begin;
  int end;


  Token(int begin, int end, byte[] exp) {
    this.begin = begin;
    this.end = end;
    this.value = new String(exp, begin, end - begin);
  }

  void print(PrintStream out, int indent, String input) {
    putIndent(out, indent);
    out.printf("%s\n", input.substring(begin, end));
  }

  public Object instantiate() throws Exception
  {
    Object obj = new Object();

    // ?

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
