package fr.aurochs;

import java.util.*;
import java.io.*;
import java.lang.*;
import java.lang.reflect.*;

public class Token extends Tree {
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
