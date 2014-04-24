package fm.serializer;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * Some non-public helpers from java.lang.{Integer,Long}
 */
public final class StringUtils {
  /**
   * All possible chars for representing a number as a String
   */
  final static char[] digits = {
      '0' , '1' , '2' , '3' , '4' , '5' ,
      '6' , '7' , '8' , '9' , 'a' , 'b' ,
      'c' , 'd' , 'e' , 'f' , 'g' , 'h' ,
      'i' , 'j' , 'k' , 'l' , 'm' , 'n' ,
      'o' , 'p' , 'q' , 'r' , 's' , 't' ,
      'u' , 'v' , 'w' , 'x' , 'y' , 'z'
  };
  
  final static char [] DigitTens = {
    '0', '0', '0', '0', '0', '0', '0', '0', '0', '0',
    '1', '1', '1', '1', '1', '1', '1', '1', '1', '1',
    '2', '2', '2', '2', '2', '2', '2', '2', '2', '2',
    '3', '3', '3', '3', '3', '3', '3', '3', '3', '3',
    '4', '4', '4', '4', '4', '4', '4', '4', '4', '4',
    '5', '5', '5', '5', '5', '5', '5', '5', '5', '5',
    '6', '6', '6', '6', '6', '6', '6', '6', '6', '6',
    '7', '7', '7', '7', '7', '7', '7', '7', '7', '7',
    '8', '8', '8', '8', '8', '8', '8', '8', '8', '8',
    '9', '9', '9', '9', '9', '9', '9', '9', '9', '9',
    } ;

  final static char [] DigitOnes = {
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    } ;

  final static byte[] INT_MIN_VALUE_BYTES = Integer.toString(Integer.MIN_VALUE).getBytes(UTF_8);
  final static byte[] LONG_MIN_VALUE_BYTES = Long.toString(Long.MIN_VALUE).getBytes(UTF_8);
  
  /**
   * Write the characters that make up this integer into the buffer
   * 
   * Note: index is the ending location in the buffer.  Bytes will be written backwards into the buffer.
   */
  final public static void writeIntChars(int value, int index, byte[] buf) {
    if (value == Integer.MIN_VALUE) {
      System.arraycopy(INT_MIN_VALUE_BYTES, 0, buf, index - INT_MIN_VALUE_BYTES.length, INT_MIN_VALUE_BYTES.length);
    } else {
      getIntChars(value, index, buf);
    }
  }
  
  /**
   * Write the characters that make up this long into the buffer.
   * 
   * Note: index is the ending location in the buffer.  Bytes will be written backwards into the buffer.
   */
  final public static void writeLongChars(long value, int index, byte[] buf) {
    if (value == Long.MIN_VALUE) {
      System.arraycopy(LONG_MIN_VALUE_BYTES, 0, buf, index - LONG_MIN_VALUE_BYTES.length, LONG_MIN_VALUE_BYTES.length);
    } else {
      getLongChars(value, index, buf);
    }
  }
  
  /**
   * Places characters representing the integer i into the
   * character array buf. The characters are placed into
   * the buffer backwards starting with the least significant
   * digit at the specified index (exclusive), and working
   * backwards from there.
   *
   * Will fail if i == Integer.MIN_VALUE
   */
  final static void getIntChars(int i, int index, byte[] buf) {
    int q, r;
    int charPos = index;
    char sign = 0;

    if (i < 0) {
        sign = '-';
        i = -i;
    }

    // Generate two digits per iteration
    while (i >= 65536) {
        q = i / 100;
    // really: r = i - (q * 100);
        r = i - ((q << 6) + (q << 5) + (q << 2));
        i = q;
        buf [--charPos] = (byte)DigitOnes[r];
        buf [--charPos] = (byte)DigitTens[r];
    }

    // Fall thru to fast mode for smaller numbers
    // assert(i <= 65536, i);
    for (;;) {
        q = (i * 52429) >>> (16+3);
        r = i - ((q << 3) + (q << 1));  // r = i-(q*10) ...
        buf [--charPos] = (byte)digits[r];
        i = q;
        if (i == 0) break;
    }
    if (sign != 0) {
        buf [--charPos] = (byte)sign;
    }
  }
  
  final static int [] sizeTable = { 9, 99, 999, 9999, 99999, 999999, 9999999,
                                    99999999, 999999999, Integer.MAX_VALUE };
  
  /**
   * Modified to work with negative numbers
   */
  final public static int stringSize(int x) {
      if (x == Integer.MIN_VALUE) return 11;
      int sign = 0;
      if (x < 0) {
        x = -x;
        sign = 1;
      }
    
      for (int i=0; ; i++)
          if (x <= sizeTable[i])
              return i+1+sign;
  }
  
  /**
   * Places characters representing the integer i into the
   * character array buf. The characters are placed into
   * the buffer backwards starting with the least significant
   * digit at the specified index (exclusive), and working
   * backwards from there.
   *
   * Will fail if i == Long.MIN_VALUE
   */
  static void getLongChars(long i, int index, byte[] buf) {
      long q;
      int r;
      int charPos = index;
      char sign = 0;

      if (i < 0) {
          sign = '-';
          i = -i;
      }

      // Get 2 digits/iteration using longs until quotient fits into an int
      while (i > Integer.MAX_VALUE) {
          q = i / 100;
          // really: r = i - (q * 100);
          r = (int)(i - ((q << 6) + (q << 5) + (q << 2)));
          i = q;
          buf[--charPos] = (byte)DigitOnes[r];
          buf[--charPos] = (byte)DigitTens[r];
      }

      // Get 2 digits/iteration using ints
      int q2;
      int i2 = (int)i;
      while (i2 >= 65536) {
          q2 = i2 / 100;
          // really: r = i2 - (q * 100);
          r = i2 - ((q2 << 6) + (q2 << 5) + (q2 << 2));
          i2 = q2;
          buf[--charPos] = (byte)DigitOnes[r];
          buf[--charPos] = (byte)DigitTens[r];
      }

      // Fall thru to fast mode for smaller numbers
      // assert(i2 <= 65536, i2);
      for (;;) {
          q2 = (i2 * 52429) >>> (16+3);
          r = i2 - ((q2 << 3) + (q2 << 1));  // r = i2-(q2*10) ...
          buf[--charPos] = (byte)digits[r];
          i2 = q2;
          if (i2 == 0) break;
      }
      if (sign != 0) {
          buf[--charPos] = (byte)sign;
      }
  }

  /**
   * Modified to work with negative numbers
   */
  public static int stringSize(long x) {
      if (x == Long.MIN_VALUE) return 20;
      
      int sign = 0;
      
      if (x < 0) {
        x = -x;
        sign = 1;
      }
    
      long p = 10;
      for (int i=1; i<19; i++) {
          if (x < p)
              return i+sign;
          p = 10*p;
      }
      return 19+sign;
  }
}
