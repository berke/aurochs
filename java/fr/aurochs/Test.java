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
    String input_fn = args[1];

    try {
      System.out.printf("Loading NOG from %s\n", nog_fn);
      byte[] nog = loadFile(nog_fn);
      System.out.printf("Creating parser\n");
      Parser p = new Parser(nog);
      System.out.printf("Loading input\n");
      byte[] input = loadFile(input_fn);
      System.out.printf("Parsing input\n");
      Tree t = p.parse(input);
      System.out.printf("Parsed input:\n");
      t.print(System.out, 0, new String(input));
    } catch(IOException io) {
      System.out.printf("IO exception: %s\n", io);
    } catch(ParseError p) {
      System.out.printf("Parse error at position %d\n", p.position);
    } catch(Exception x) {
      System.out.printf("Exception: %s\n", x);
    }
  }
}
