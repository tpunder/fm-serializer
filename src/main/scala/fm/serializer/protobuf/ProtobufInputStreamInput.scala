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

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.charset.StandardCharsets.UTF_8

import fm.serializer.Input

// TODO
// TODO: Check if this patch needs to be ported: https://code.google.com/p/protobuf/source/diff?spec=svn349&r=349&format=side&path=/trunk/java/src/main/java/com/google/protobuf/CodedInputStream.java
// TODO
final class ProtobufInputStreamInput(is: InputStream, options: ProtobufOptions) extends ProtobufInput {
  if (!is.markSupported()) throw new IllegalArgumentException("mark() must be supported on the InputStream")
  
  /**
   * The current number of bytes read so far (for the current scope)
   */
  private[this] var currentBytesRead: Int = 0
  
  /**
   * The current limit for the number of bytes that can be read
   */
  private[this] var currentBytesLimit: Int = Int.MaxValue
  
  def hasAnotherElement: Boolean = {    
    if (currentBytesLimit < Int.MaxValue) {
      // We are currently reading something with a known length
      currentBytesRead < currentBytesLimit
    } else {
      // We don't know the length so let's peek at the first byte to see if there is anything there
      is.mark(1)
      val res: Boolean = -1 != is.read()
      is.reset()
      res
    }
  }
  
  final def readRawString(): String = {
    val res: String = new String(readRawByteArray(), UTF_8)
    if (options.internStrings) res.intern() else res
  }
  
  final def readRawByteArray(): Array[Byte] = {
    val bos: ByteArrayOutputStream = new ByteArrayOutputStream
    val buf: Array[Byte] = new Array(256)
    var read: Int = is.read(buf)
    while (read != -1) {
      bos.write(buf, 0, read)
      read = is.read(buf)
    }
    bos.toByteArray
  }
  
  final def readNestedString(): String = {
    if (lastTagIsNullValue) return null
    
    val size: Int = readRawVarint32()
    if (-1 == size) null else if (0 == size) "" else {
      val bytes: Array[Byte] = readRawBytes(size)
      val res: String = new String(bytes, UTF_8)
      if (options.internStrings) res.intern() else res
    }
  }
 
  def readLengthDelimited[T](f: Input => T): T = {
    // The value is null if the tag is for a null field
    if (lastTagIsNullValue) return null.asInstanceOf[T]

    checkLastTagTypeWas(WireFormat.WIRETYPE_LENGTH_DELIMITED)
    
    val length: Int = readRawVarint32()
    
    // It's also null if the length is -1
    if (-1 == length) return null.asInstanceOf[T]
    
    // Save the old counts so we can restore them later
    val oldBytesLimit = currentBytesLimit
    
    currentBytesLimit = currentBytesRead + length
    
    val res: T = f(this)
    
    // Restore old counts
    currentBytesLimit = oldBytesLimit
    
    res
  }
  
  /**
   * Increments the current number of bytes read and checks to 
   * make sure we haven't exceeded the limit.
   */
  private def incrementBytesRead(count: Int) {
    currentBytesRead += count
    if (currentBytesRead > currentBytesLimit) throw InvalidProtocolBufferException.sizeLimitExceeded()
  }
  
  /**
   * Read a raw byte as an Int (which can be checked for a -1 value)
   */
  private def readRawByteInt(): Int = {
    if (currentBytesRead >= currentBytesLimit) -1 else {
      val b = is.read()
      incrementBytesRead(1)
      b
    }
  }
  
  /**
   * Read a raw byte with checking for the end of the stream
   */
  protected def readRawByte(): Byte = {
    val b = readRawByteInt()
    if (-1 == b) throw InvalidProtocolBufferException.truncatedMessage()
    b.toByte
  }
  
  protected def readRawBytes(size: Int): Array[Byte] = {
    if (0 == size) return Array[Byte]()
    
    val bytes = new Array[Byte](size)
    val bytesRead = is.read(bytes)
    if (bytesRead != size) throw InvalidProtocolBufferException.truncatedMessage()
    incrementBytesRead(size)
    bytes
  }
  
  /**
   * Reads and discards {@code size} bytes.
   *
   * @throws InvalidProtocolBufferException The end of the stream or the current
   *                                        limit was reached.
   */
  def skipRawBytes(size: Int) {
    if (size < 0) throw InvalidProtocolBufferException.negativeSize()
    is.skip(size)
    incrementBytesRead(size)
  }
  
  /**
   * Attempt to read a field tag, returning zero if we have reached EOF.
   * Protocol message parsers use this to read tags, since a protocol message
   * may legally end wherever a tag occurs, and zero is not a valid tag number.
   */
  protected def readTag(): Int = {
    val firstByte: Int = readRawByteInt()
    
    if (-1 == firstByte) lastTag = 0 // End of Stream
    else {
      lastTag = readRawVarint32(firstByte.toByte)
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