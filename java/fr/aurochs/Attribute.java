package fr.aurochs;

import java.util.*;
import java.io.*;
import java.lang.*;
import java.lang.reflect.*;

class AttributeInstantiateError extends Exception
{
  public static final long serialVersionUID = 42;
}

public class Attribute {
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
  
  public void instantiate(Object self, Class<?> c) throws Exception
  {
    try
    {
      Method m = c.getMethod(name + "FromString", String.class);

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
