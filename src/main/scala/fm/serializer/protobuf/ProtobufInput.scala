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

import fm.serializer.{CollectionInput, FieldInput, Input}
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import scala.annotation.{switch, tailrec}

abstract class ProtobufInput extends Input {
 
  def allowStringMap: Boolean = false

  private def readJavaBigDecimalFields(in: FieldInput): JavaBigDecimal = {
    var unscaledVal: JavaBigInteger = null
    var scale: Int = -1

    var fieldNumber: Int = in.readFieldNumber(Map.empty)

    while (fieldNumber != 0) {
      fieldNumber match {
        case 1 => unscaledVal = in.readNestedBigInteger()
        case 2 => scale = in.readNestedInt()
        case _ => in.skipUnknownField()
      }

      fieldNumber = in.readFieldNumber(Map.empty)
    }

    new JavaBigDecimal(unscaledVal, scale)
  }

  //
  // FIELD Input Implementation
  //
  
  /**
   * Read the next field number
   */
  final def readFieldNumber(nameToNumMap: Map[String, Int]): Int = {
    val tag: Int = readTag()
    WireFormat.getTagFieldNumber(tag)
  }
  
  // Since allowStringMap is false this doesn't need to be implemented
  def readFieldName(): String = ???
  
  // Skip an unknown field
  final def skipUnknownField() {
    //println(s"skipUnknown for tag: $lastTag   fieldNumber: ${WireFormat.getTagFieldNumber(lastTag)}")
    skipField(lastTag) 
  }
  
  /**
   * Was the last tag read for a null field?  This is only applicable for reading fields but is safe to use in the readNestedXXX
   * call because it will only ever be true if we are actually reading a null value field.
   */
  final protected def lastTagIsNullValue: Boolean = WireFormat.getTagWireType(lastTag) == WireFormat.WIRETYPE_NULL
  
  def nextValueIsNull: Boolean = lastTagIsNullValue
  
  //
  // RAW Input Implementation (with some abstract methods)
  //
   
  // Basic Types
  final def readRawBool(): Boolean = readRawVarint32() != 0
  final def readRawFloat(): Float = java.lang.Float.intBitsToFloat(readRawFixedInt())
  final def readRawDouble(): Double = java.lang.Double.longBitsToDouble(readRawFixedLong())

  final def readRawBigInteger(): JavaBigInteger = {
    if (nextValueIsNull) null else new JavaBigInteger(readRawByteArray())
  }

  final def readRawBigDecimal(): JavaBigDecimal = readRawObject{ readJavaBigDecimalFields }

  def readRawString(): String
    
  // Bytes
  def readRawByteArray(): Array[Byte]
  
  // Ints  
  final def readRawInt(): Int = readRawVarint32()
  final def readRawUnsignedInt(): Int = readRawVarint32()
  final def readRawSignedInt(): Int = decodeZigZag32(readRawVarint32())
  
  final def readRawFixedInt(): Int = {
    readRawLittleEndian32()
    
//    if (getTagWireType(lastTag) == WireFormat.WIRETYPE_FIXED32_LE) readRawLittleEndian32()
//    else readRawBigEndian32()
  }
  
  // Longs
  final def readRawLong(): Long = readRawVarint64()
  final def readRawUnsignedLong(): Long = readRawVarint64()
  final def readRawSignedLong(): Long = decodeZigZag64(readRawVarint64())
  
  final def readRawFixedLong(): Long = {
    readRawLittleEndian64()
    
//    if (getTagWireType(lastTag) == WireFormat.WIRETYPE_FIXED64_LE) readRawLittleEndian64()
//    else readRawBigEndian64()
  }
    
  // Objects
  def readRawObject[T](f: FieldInput => T): T = {
    f(this)
  }
  
  // Collections
  def readRawCollection[T](f: CollectionInput => T): T = f(this)
  
  //
  // NESTED Input Implementation
  //
  // Basic Types
  final def readNestedBool(): Boolean = readRawVarint32() != 0
  final def readNestedFloat(): Float = java.lang.Float.intBitsToFloat(readRawFixedInt())
  final def readNestedDouble(): Double = java.lang.Double.longBitsToDouble(readRawFixedLong())

  final def readNestedBigInteger(): JavaBigInteger = if (nextValueIsNull) null else new JavaBigInteger(readNestedByteArray())
  final def readNestedBigDecimal(): JavaBigDecimal = readNestedObject{ readJavaBigDecimalFields }

  def readNestedString(): String
  
  // Bytes
  final def readNestedByteArray(): Array[Byte] = {
    if (lastTagIsNullValue) return null
    
    val size: Int = readRawVarint32()
    readRawBytes(size)
  }
  
  // Ints  
  final def readNestedInt(): Int = readRawVarint32()
  final def readNestedUnsignedInt(): Int = readRawVarint32()
  final def readNestedSignedInt(): Int = decodeZigZag32(readRawVarint32())
  
  final def readNestedFixedInt(): Int = readRawFixedInt()
  
  // Longs
  final def readNestedLong(): Long = readRawVarint64()
  final def readNestedUnsignedLong(): Long = readRawVarint64()
  final def readNestedSignedLong(): Long = decodeZigZag64(readRawVarint64())
  
  final def readNestedFixedLong(): Long = readRawFixedLong()
  
  // Objects
  def readNestedObject[T](f: FieldInput => T): T = {
    if (lastTagIsNullValue) return null.asInstanceOf[T]
    
    // We use grouped messages for Nested and Field objects
    f(this)
    
    //if (WireFormat.getTagWireType(lastTag) == WireFormat.WIRETYPE_LENGTH_DELIMITED) return readLengthDelimited(f)
    //f(this)
  }
  
  // Collections
  def readNestedCollection[T](f: CollectionInput => T): T = {
    readLengthDelimited(f)
  }
  
  //
  // Shared Protobuf Implementation
  //
  /**
   * The last tag that was read by readTag()
   */
  final protected var lastTag: Int = 0
  
  //
  // Abstract Methods that must be implemented
  //
  protected def readTag(): Int
  protected def readRawByte(): Byte
  protected def skipRawBytes(size: Int)
  protected def readRawBytes(size: Int): Array[Byte]
  protected def readLengthDelimited[T](f: Input => T): T

  
  /**
   * Verifies that the last call to readTag() returned the given tag value.
   * This is used to verify that a nested group ended with the correct
   * end tag.
   *
   * @throws InvalidProtocolBufferException {@code value} does not match the
   *                                        last tag.
   */
  final protected def checkLastTagWas(value: Int): Unit = {
    if (lastTag != value) throw InvalidProtocolBufferException.invalidEndTag()
  }
  
  /**
   * Reads and discards an entire message.  This will read either until EOF
   * or until an endgroup tag, whichever comes first.
   */
  final def skipMessage(): Unit = {
    while (true) {
      val tag: Int = readTag()
      if (tag == 0 || !skipField(tag)) return
    }
  }
  
  /**
   * Reads and discards a single field, given its tag value.
   *
   * @return {@code false} if the tag is an endgroup tag, in which case
   *         nothing is skipped.  Otherwise, returns {@code true}.
   */
  final def skipField(tag: Int): Boolean = {
    val wireType: Int = WireFormat.getTagWireType(tag)
    
    (wireType: @switch) match {
      case WireFormat.WIRETYPE_VARINT =>
        readRawVarint32()
        true
      case WireFormat.WIRETYPE_FIXED64_LE =>
        readRawLittleEndian64()
        true
      case WireFormat.WIRETYPE_LENGTH_DELIMITED =>
        // We use -1 for null so only skip bytes if it's positive
        val len: Int = readRawVarint32()
        if (len > 0) skipRawBytes(len)
        true
      case WireFormat.WIRETYPE_START_GROUP =>
        skipMessage()
        true
      case WireFormat.WIRETYPE_END_GROUP =>
        // I don't think this should happen
        ???
//      case WireFormat.WIRETYPE_FIXED64_BE =>
//        readRawBigEndian64()
//        true
//      case WireFormat.WIRETYPE_FIXED32_BE =>
//        readRawBigEndian32()
//        true
      case WireFormat.WIRETYPE_FIXED32_LE =>
        readRawLittleEndian32()
        true
      case WireFormat.WIRETYPE_NULL =>
        true
      case _ =>
        throw InvalidProtocolBufferException.invalidWireType()
    }
  }
  
  /**
   * Decode a ZigZag-encoded 32-bit value.  ZigZag encodes signed integers
   * into values that can be efficiently encoded with varint.  (Otherwise,
   * negative values must be sign-extended to 64 bits to be varint encoded,
   * thus always taking 10 bytes on the wire.)
   *
   * @param n An unsigned 32-bit integer, stored in a signed int because
   *          Java has no explicit unsigned support.
   * @return A signed 32-bit integer.
   */
  protected final def decodeZigZag32(n: Int): Int = (n >>> 1) ^ -(n & 1)
  
  /**
   * Decode a ZigZag-encoded 64-bit value.  ZigZag encodes signed integers
   * into values that can be efficiently encoded with varint.  (Otherwise,
   * negative values must be sign-extended to 64 bits to be varint encoded,
   * thus always taking 10 bytes on the wire.)
   *
   * @param n An unsigned 64-bit integer, stored in a signed int because
   *          Java has no explicit unsigned support.
   * @return A signed 64-bit integer.
   */
  protected def decodeZigZag64(n: Long): Long = (n >>> 1) ^ -(n & 1)
  
  /**
   * Read a raw Varint from the stream.  If larger than 32 bits, discard the
   * upper bits.
   */
  final def readRawVarint32(): Int = readRawVarint32Impl(0, 0, readRawByte())
    
  /**
   * Read a raw Varint from the stream given the first byte.  
   * If larger than 32 bits, discard the upper bits.
   */
  final protected def readRawVarint32(firstByte: Byte): Int = readRawVarint32Impl(0, 0, firstByte)
  
  @tailrec
  private def readRawVarint32Impl(result: Int, bytesRead: Int, nextByte: Byte): Int = {
    val tmp: Byte = nextByte
    
    // If we've already read our 32 bits we are just discarding the rest
    // until we reach our termination condition (a non-negative byte)
    if (bytesRead > 4) {
      if (tmp >= 0) result else readRawVarint32Impl(result, bytesRead + 1, readRawByte())
    } else {
      val shift = bytesRead * 7
      if (tmp >= 0) result | (tmp << shift)
      else readRawVarint32Impl(result | ((tmp & 0x7f) << shift), bytesRead + 1, readRawByte()) 
    }
  }
  
  /** Read a raw Varint from the stream. */
  private def readRawVarint64(): Long = {
    var shift: Int = 0
    var result: Long = 0l
    
    while (shift < 64) {
      val b = readRawByte()
      result |= (b & 0x7f).toLong << shift
      if ((b & 0x80) == 0) return result
      shift += 7
    }
    
    throw InvalidProtocolBufferException.malformedVarint()
  }
  
  /** Read a 32-bit little-endian integer from the stream. */
  protected final def readRawLittleEndian32(): Int = {
    val b1: Byte = readRawByte()
    val b2: Byte = readRawByte()
    val b3: Byte = readRawByte()
    val b4: Byte = readRawByte()
    
    ((b1.toInt & 0xff)      ) |
    ((b2.toInt & 0xff) <<  8) |
    ((b3.toInt & 0xff) << 16) |
    ((b4.toInt & 0xff) << 24)
  }
  
  /** Read a 64-bit little-endian integer from the stream. */
  protected final def readRawLittleEndian64(): Long = {
    val b1: Byte = readRawByte()
    val b2: Byte = readRawByte()
    val b3: Byte = readRawByte()
    val b4: Byte = readRawByte()
    val b5: Byte = readRawByte()
    val b6: Byte = readRawByte()
    val b7: Byte = readRawByte()
    val b8: Byte = readRawByte()
    
    ((b1.toLong & 0xff)      ) |
    ((b2.toLong & 0xff) <<  8) |
    ((b3.toLong & 0xff) << 16) |
    ((b4.toLong & 0xff) << 24) |
    ((b5.toLong & 0xff) << 32) |
    ((b6.toLong & 0xff) << 40) |
    ((b7.toLong & 0xff) << 48) |
    ((b8.toLong & 0xff) << 56)
  }
  
  /** Read a 32-bit big-endian integer from the stream. */
  protected final def readRawBigEndian32(): Int = {
    val b1: Byte = readRawByte()
    val b2: Byte = readRawByte()
    val b3: Byte = readRawByte()
    val b4: Byte = readRawByte()
    
    ((b1.toInt & 0xff) << 24) |
    ((b2.toInt & 0xff) << 16) |
    ((b3.toInt & 0xff) <<  8) |
    ((b4.toInt & 0xff)      )
  }
  
  /** Read a 64-bit big-endian integer from the stream. */
  protected final def readRawBigEndian64(): Long = {
    val b1: Byte = readRawByte()
    val b2: Byte = readRawByte()
    val b3: Byte = readRawByte()
    val b4: Byte = readRawByte()
    val b5: Byte = readRawByte()
    val b6: Byte = readRawByte()
    val b7: Byte = readRawByte()
    val b8: Byte = readRawByte()
    
    ((b1.toLong & 0xff) << 56) |
    ((b2.toLong & 0xff) << 48) |
    ((b3.toLong & 0xff) << 40) |
    ((b4.toLong & 0xff) << 32) |
    ((b5.toLong & 0xff) << 24) |
    ((b6.toLong & 0xff) << 16) |
    ((b7.toLong & 0xff) <<  8) |
    ((b8.toLong & 0xff)      )
  }
}