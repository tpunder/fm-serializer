package fm.serializer.fastutil;

/*     
 * Copyright (C) 2005-2013 Sebastiano Vigna 
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License. 
 */
public class ByteArrays {
  private ByteArrays() {}
  
  /** A static, final, empty array. */
  public final static byte[] EMPTY_ARRAY = {};
  
  /** Trims the given array to the given length.
  *
  * @param array an array.
  * @param length the new maximum length for the array.
  * @return <code>array</code>, if it contains <code>length</code>
  * entries or less; otherwise, an array with
  * <code>length</code> entries whose entries are the same as
  * the first <code>length</code> entries of <code>array</code>.
  * 
  */
  public static byte[] trim( final byte[] array, final int length ) {
   if ( length >= array.length ) return array;
   final byte t[] =
    length == 0 ? EMPTY_ARRAY : new byte[ length ];
   System.arraycopy( array, 0, t, 0, length );
   return t;
  }
  
  /** Ensures that a range given by an offset and a length fits an array.
  *
  * <P>This method may be used whenever an array range check is needed.
  *
  * @param a an array.
  * @param offset a start index.
  * @param length a length (the number of elements in the range).
  * @throws IllegalArgumentException if <code>length</code> is negative.
  * @throws ArrayIndexOutOfBoundsException if <code>offset</code> is negative or <code>offset</code>+<code>length</code> is greater than the array length.
  */
  public static void ensureOffsetLength( final byte[] a, final int offset, final int length ) {
   Arrays.ensureOffsetLength( a.length, offset, length );
  }
  
  /** Grows the given array to the maximum between the given length and
   * the current length multiplied by two, provided that the given
   * length is larger than the current length.
   *
   * <P>If you want complete control on the array growth, you
   * should probably use <code>ensureCapacity()</code> instead.
   *
   * @param array an array.
   * @param length the new minimum length for this array.
   * @return <code>array</code>, if it can contain <code>length</code>
   * entries; otherwise, an array with
   * max(<code>length</code>,<code>array.length</code>/&phi;) entries whose first
   * <code>array.length</code> entries are the same as those of <code>array</code>.
   * */
 public static byte[] grow( final byte[] array, final int length ) {
  if ( length > array.length ) {
   final int newLength = (int)Math.max( Math.min( 2L * array.length, Arrays.MAX_ARRAY_SIZE ), length );
   final byte t[] =
    new byte[ newLength ];
   System.arraycopy( array, 0, t, 0, array.length );
   return t;
  }
  return array;
 }
 
 /** Grows the given array to the maximum between the given length and
   * the current length multiplied by two, provided that the given
   * length is larger than the current length, preserving just a part of the array.
   *
   * <P>If you want complete control on the array growth, you
   * should probably use <code>ensureCapacity()</code> instead.
   *
   * @param array an array.
   * @param length the new minimum length for this array.
   * @param preserve the number of elements of the array that must be preserved in case a new allocation is necessary.
   * @return <code>array</code>, if it can contain <code>length</code>
   * entries; otherwise, an array with
   * max(<code>length</code>,<code>array.length</code>/&phi;) entries whose first
   * <code>preserve</code> entries are the same as those of <code>array</code>.
   * */
 public static byte[] grow( final byte[] array, final int length, final int preserve ) {
  if ( length > array.length ) {
   final int newLength = (int)Math.max( Math.min( 2L * array.length, Arrays.MAX_ARRAY_SIZE ), length );
   final byte t[] =
    new byte[ newLength ];
   System.arraycopy( array, 0, t, 0, preserve );
   return t;
  }
  return array;
 }
}


