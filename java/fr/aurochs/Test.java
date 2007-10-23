package fr.aurochs;

import java.io.*;

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
        } catch(ParseError pe) {
          System.out.printf("Parse error at position %d\n", pe.position);
        }
      }
    } catch(IOException io) {
      System.out.printf("IO exception: %s\n", io);
    } catch(Exception x) {
      System.out.printf("Exception: %s\n", x);
    }
  }
}
