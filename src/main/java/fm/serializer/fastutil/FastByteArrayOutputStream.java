// A local copy of it.unimi.dsi.fastutil.io.FastByteArrayOutputStream with minimal dependencies
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

import java.io.IOException;
import java.io.OutputStream;

/** Simple, fast byte-array output stream that exposes the backing array.
 *
 * <P>{@link java.io.ByteArrayOutputStream} is nice, but to get its content you
 * must generate each time a new object. This doesn't happen here.
 *
 * <P>This class will automatically enlarge the backing array, doubling its
 * size whenever new space is needed. The {@link #reset()} method will
 * mark the content as empty, but will not decrease the capacity: use 
 * {@link #trim()} for that purpose.
 *
 * @author Sebastiano Vigna
 */

public class FastByteArrayOutputStream extends OutputStream {

  /** The array backing the output stream. */
  public final static int DEFAULT_INITIAL_CAPACITY = 16;

  /** The array backing the output stream. */
  public byte[] array;

  /** The number of valid bytes in {@link #array}. */
  public int length;

  /** The current writing position. */
  private int position;

  /** Creates a new array output stream with an initial capacity of {@link #DEFAULT_INITIAL_CAPACITY} bytes. */
  public FastByteArrayOutputStream() {
    this( DEFAULT_INITIAL_CAPACITY );
  }

  /** Creates a new array output stream with a given initial capacity.
   *
   * @param initialCapacity the initial length of the backing array.
   */
  public FastByteArrayOutputStream( final int initialCapacity ) {
    array = new byte[ initialCapacity ];
  }

  /** Creates a new array output stream wrapping a given byte array.
   *
   * @param a the byte array to wrap.
   */
  public FastByteArrayOutputStream( final byte[] a ) {
    array = a;
  }

  /** Marks this array output stream as empty. */
  public void reset() {
    length = 0;
    position = 0;
  }

  /** Ensures that the length of the backing array is equal to {@link #length}. */
  public void trim() {
    array = ByteArrays.trim( array, length );
  }
  
  /** Ensures that the length of the backing array is equal to {@link #length}. */
  public void trimTo(int newLength) {
    if (newLength < length) throw new IllegalArgumentException("newLength must be less than the current length");
    array = ByteArrays.trim( array, newLength );
  }
  
  /** Create a NEW COPY of the byte array trimmed to the proper length */
  public byte[] toByteArray() {
    byte[] dst = new byte[length];
    System.arraycopy(array, 0, dst, 0, length);
    return dst;
  }

  public void write( final int b ) {
    if ( position >= array.length ) array = ByteArrays.grow( array, position + 1, length );
    array[ position++ ] = (byte)b;
    if ( length < position ) length = position;
  }

  public void write( final byte[] b, final int off, final int len ) throws IOException {
    ByteArrays.ensureOffsetLength( b, off, len );
    if ( position + len > array.length ) array = ByteArrays.grow( array, position + len, position );
    System.arraycopy( b, off, array, position, len );
    if ( position + len > length ) length = position += len;
  }
}
