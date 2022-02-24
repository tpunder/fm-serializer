/*
 * Copyright 2014 Frugal Mechanic (http://frugalmechanic.com)
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
package fm.serializer.protobuf

import java.nio.charset.StandardCharsets.UTF_8
import fm.serializer.Input

/**
 * Optimized Input implementation for reading from a byte array that
 * is much more JVM/JIT optimization friendly than reading from an
 * InputStream.
 */
final class ProtobufByteArrayInput(buffer: Array[Byte], options: ProtobufOptions) extends ProtobufInput {
  
  /**
   * offset in the array
   */
  private[this] var offset: Int = 0
  
  /**
   * The limit for the number of bytes that can be read
   */
  private[this] var limit: Int = buffer.length
  
  // For CollectionInput
  final def hasAnotherElement: Boolean = offset < limit

  // Optimized to avoid the extra array copy
  final def readRawString(): String = readRawString(limit - offset)

  // Optimized to avoid the extra array copy
  private def readRawString(size: Int): String = {
    val res: String = new String(buffer, offset, size, UTF_8)
    offset += size
    if (options.internStrings) res.intern() else res
  }
    
  // Optimized to avoid the extra array copy
  final def readRawByteArray(): Array[Byte] = readRawBytes(limit - offset)
  
  // Optimized to avoid the extra array copy
  final def readNestedString(): String = {
    if (lastTagIsNullValue) return null
    
    val size: Int = readRawVarint32()
    if (-1 == size) null else if (0 == size) "" else readRawString(size)
  }
  
  def readLengthDelimited[T](f: Input => T): T = {
    // The value is null if the tag is for a null field
    if (lastTagIsNullValue) return null.asInstanceOf[T]

    checkLastTagTypeWas(WireFormat.WIRETYPE_LENGTH_DELIMITED)
    
    val length: Int = readRawVarint32()
    
    // It's also null if the length is -1
    if (-1 == length) return null.asInstanceOf[T]
    
    // Save the old limit
    val oldLimit: Int = limit
    
    limit = offset + length
    
    val res: T = f(this)
    
    // Restore old limit
    limit = oldLimit
    
    res
  }
  
  /**
   * Read a raw byte with checking for the end of the stream
   */  
  protected def readRawByte(): Byte = {
    val b: Byte = buffer(offset)
    offset += 1
    b
  }
  
  final protected def readRawBytes(size: Int): Array[Byte] = {
    if (0 == size) return Array[Byte]()
    if (0 == offset && size == limit) return buffer
    
    val dest: Array[Byte] = new Array[Byte](size)
    System.arraycopy(buffer, offset, dest, 0, size)
    offset += size
    dest
  }
  
  /**
   * Reads and discards {@code size} bytes.
   *
   * @throws InvalidProtocolBufferException The end of the stream or the current
   *                                        limit was reached.
   */
  final def skipRawBytes(size: Int): Unit = {
    if (size < 0) throw InvalidProtocolBufferException.negativeSize()
    offset += size
    if (offset > limit) throw InvalidProtocolBufferException.truncatedMessage()
  }
  
  /**
   * Attempt to read a field tag, returning zero if we have reached EOF.
   * Protocol message parsers use this to read tags, since a protocol message
   * may legally end wherever a tag occurs, and zero is not a valid tag number.
   */
  protected def readTag(): Int = {
    if (offset >= limit) lastTag = 0 // End of Stream
    else {
      lastTag = readRawVarint32()
      //println(s"readTag() => ${lastTag}   fieldNumber: ${WireFormat.getTagFieldNumber(lastTag)}  wireType: ${WireFormat.getTagWireType(lastTag)}")
      if (WireFormat.getTagWireType(lastTag) == WireFormat.WIRETYPE_END_GROUP) {
        // Signal the end of the object
        lastTag = 0
      } else if (WireFormat.getTagFieldNumber(lastTag) == 0) {
        // If we actually read zero (or any tag number corresponding to field
        // number zero), that's not a valid tag.
        throw InvalidProtocolBufferException.invalidTag()
      }
    }
    
    lastTag
  }
}