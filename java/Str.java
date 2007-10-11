public class Str
{
	private native void fuzzy(java.nio.CharBuffer s);

	public static void main(String[] args)
	{
		Str s = new Str();
		
		System.out.println(" ---> Testing Java native CharBuffer");
		java.nio.CharBuffer cb = java.nio.ByteBuffer.allocateDirect(100 * 2).asCharBuffer();

		cb.put("Test !");
		cb.position(0);

		System.out.println("  class name : " + cb.getClass().getName());
		System.out.println("  is direct  : " + cb.isDirect());
		System.out.println("  capacity   : " + cb.capacity());
		System.out.println("  length     : " + cb.length());
		System.out.println("  content    : " + cb.toString());
		s.fuzzy(cb);
		System.out.println("  After call  : " + cb.toString());
	}
	
	static
	{
		System.loadLibrary("Str");
	}
}
