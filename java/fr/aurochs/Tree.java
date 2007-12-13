package fr.aurochs;

import java.util.*;
import java.io.*;
import java.lang.*;
import java.lang.reflect.*;

abstract public class Tree {
  static void putIndent(PrintStream out, int count) {
    for(int i = 0; i < count; i ++) {
      out.printf("  ");
    }
  }
  abstract void print(PrintStream out, int indent, String input);
  abstract Object instantiate(String pkg) throws Exception;
}
