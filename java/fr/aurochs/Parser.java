package fr.aurochs;

import java.util.*;
import java.io.*;
import java.lang.*;
import java.lang.reflect.*;

class ParseError extends Exception {
  public static final long serialVersionUID = 43;
  int position;
  ParseError(int x) {
    position = x;
  }
}

class NOGexception extends Exception
{
  public static final long serialVersionUID = 44;
}


public class Parser {
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
