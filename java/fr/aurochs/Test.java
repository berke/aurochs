// Example for Aurochs

package fr.aurochs;

import java.lang.*;
import java.util.*;
import java.io.*;

abstract class calcNode
{
  public LinkedList<calcNode> contents;

  calcNode() {
    contents = new LinkedList<calcNode>();
  }

  abstract public int calculate();
}

class calcRoot extends calcNode
{
  public int calculate()
  {
    return contents.get(0).calculate();
  }
}

class number extends calcNode {
  public int value;

  public int calculate() {
    return value;
  }
}

class add extends calcNode
{
  public int calculate()
  {
    int res = 0;

    for(calcNode n : contents)
      res += n.calculate();

    return res;
  }
}

class mul extends calcNode
{
  public int calculate()
  {
    int res = 1;

    for(calcNode n : contents)
      res *= n.calculate();

    return res;
  }
}

class Test {
  private static byte[] loadFile(String fn) throws IOException {
    File f = new File(fn);
    int size = (int) f.length();
    byte[] data = new byte[size];
    FileInputStream fis = new FileInputStream(f);
    int i;
    i = 0;
    while(i < size) {
      int n;
      n = fis.read(data, i, size - i);
      System.out.printf("  Read %d\n", n);
      if(n < 0) throw new IOException();
      i += n;
    }
    return data;
  }
  public static void main(String[] args) {
    String nog_fn = args[0];

    try {
      System.out.printf("Loading NOG from %s\n", nog_fn);
      byte[] nog = loadFile(nog_fn);
      System.out.printf("Creating parser\n");
      Parser p = new Parser(nog);
      for(int i = 1; i < args.length; i ++) {
        String input_fn = args[i];
        System.out.printf("Loading file %s\n", input_fn);
        byte[] input = loadFile(input_fn);
        try {
          System.out.printf("Parsing input\n");
          Tree t = p.parse(input);
          System.out.printf("Parsed input:\n");
          t.print(System.out, 0, new String(input));
          System.out.printf("Instantiating object:\n");
	  calcRoot r = (calcRoot) t.instantiate("fr.aurochs");
          System.out.printf("Computed value is %d\n", r.calculate());
        } catch(ParseError pe) {
          System.out.printf("Parse error at position %d\n", pe.position);
        }
      }
    } catch(IOException io) {
      System.out.printf("IO exception: %s\n", io);
    } catch(Exception x) {
      System.out.printf("Exception: %s\n", x);
      x.printStackTrace();
    }
  }
}
