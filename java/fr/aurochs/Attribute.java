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

  Attribute(String name, int length, byte[] value)
  {
    this.name = name;
    this.start = -2;
    this.end = -1;
    this.value = new String(value, 0, length);
  }

  Attribute(String name, int start, int end, byte[] exp) {
    this.name = name;
    this.start = start;
    this.end = end;
    this.value = new String(exp, start, end - start);
  }

  void print(PrintStream out, String input) {
     out.printf("%s=\"", name);
     if(start < end) {
       out.printf("%s\"", value);
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
      try
      {
        Field f = c.getField(name);
        Class cF = f.getType();

        if (cF == Byte.TYPE)
          f.setByte(self, Byte.parseByte(value));
        else if(cF == Double.TYPE)
          f.setDouble(self, Double.parseDouble(value));
        else if(cF == Float.TYPE)
          f.setFloat(self, Float.parseFloat(value));
        else if (cF == Integer.TYPE)
          f.setInt(self, Integer.parseInt(value));
        else if (cF == Long.TYPE)
          f.setLong(self, Long.parseLong(value));
        else if (cF == Short.TYPE)
          f.setShort(self, Short.parseShort(value));
        else if (cF == String.class)
          f.set(self, value);
        else
          // We have neither a method *FromString nor a primitive type, i give up :-(
          throw new AttributeInstantiateError();
      }
      catch(AttributeInstantiateError aie)
      {
        throw aie;
      }
    }
  }
}
