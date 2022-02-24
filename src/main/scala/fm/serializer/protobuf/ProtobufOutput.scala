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

import fm.serializer.{FieldOutput, NestedOutput, Output}
import fm.serializer.FMByteArrayOutputStream
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import scala.annotation.tailrec


/**
 * Modified Protocol Buffers Output
 * 
 * The main difference between his and the stock Protocol Buffers Implementation
 * is the handling of repeated fields.  In this implementation:
 * 
 *  - We use ONLY packed repeated fields for collections (including non-numeric types)
 *  
 * The Stock Implementation:
 * 
 *  - Optionally uses packed repeated fields for numeric types
 */
final class ProtobufOutput() extends Output {
  def allowStringMap: Boolean = false
  
  private[this] val MaxVarint32Bytes: Int = 5
  private[this] val MaxVarint64Bytes: Int = 10
  private[this] val EncodeMessagesAsGroups: Boolean = true
  
  private[this] val os: FMByteArrayOutputStream = new FMByteArrayOutputStream()
  
  def toByteArray: Array[Byte] = os.toByteArray
  def reset(): Unit = os.reset()
  
  private def writeRawByte(byte: Int): Unit = os.write(byte)  
  private def writeRawBytes(bytes: Array[Byte]): Unit = os.write(bytes)

  private def writeJavaBigDecimalFields(out: FieldOutput, obj: JavaBigDecimal): Unit = {
    out.writeFieldBigInteger(1, "unscaledVal", obj.unscaledValue())
    out.writeFieldInt(2, "scale", obj.scale())
  }

  //
  // RAW Output Implementation
  //
  
  // Basic
  final def writeRawBool(value: Boolean): Unit = writeBoolNoTag(value)
  final def writeRawFloat(value: Float): Unit = writeFloatNoTag(value)
  final def writeRawDouble(value: Double): Unit = writeDoubleNoTag(value)

  final def writeRawBigInteger(value: JavaBigInteger): Unit = writeRawByteArray(value.toByteArray)
  final def writeRawBigDecimal(value: JavaBigDecimal): Unit = writeRawObject(value){ writeJavaBigDecimalFields }

  final def writeRawString(value: String): Unit = {
    if (null == value) throw new IllegalArgumentException("Can't write a null rawString value")
    
    // No Length Prefix
    os.writeUTF8Bytes(value)
  }

  // Bytes
  final def writeRawByteArray(value: Array[Byte]): Unit = {
    // No Length Prefix
    writeRawBytes(value)
  }
  
  // Ints  
  final def writeRawInt(value: Int): Unit = writeInt32NoTag(value)
  final def writeRawUnsignedInt(value: Int): Unit = writeRawVarint32(value)
  final def writeRawSignedInt(value: Int): Unit = writeRawVarint32(encodeZigZag32(value))
  final def writeRawFixedInt(value: Int): Unit = writeRawLittleEndian32(value)
  
  // Longs
  final def writeRawLong(value: Long): Unit = writeRawVarint64(value)
  final def writeRawUnsignedLong(value: Long): Unit = writeRawVarint64(value)
  final def writeRawSignedLong(value: Long): Unit = writeRawVarint64(encodeZigZag64(value))
  final def writeRawFixedLong(value: Long): Unit = writeRawLittleEndian64(value)
  
  // Objects
  final def writeRawObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    if (null == obj) throw new IllegalArgumentException("Cannot write raw null object")
    f(this, obj)
  }
  
  // Colletions
  final def writeRawCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null == col) throw new IllegalArgumentException("Cannot write a raw null collection")
    f(this, col)
  }
  
  //
  // NESTED Output Implementation
  //
  
  // Basic
  final def writeNestedBool(value: Boolean): Unit = writeBoolNoTag(value)
  final def writeNestedFloat(value: Float): Unit = writeFloatNoTag(value)
  final def writeNestedDouble(value: Double): Unit = writeDoubleNoTag(value)

  final def writeNestedBigInteger(value: JavaBigInteger): Unit = writeNestedByteArray(value.toByteArray)
  final def writeNestedBigDecimal(value: JavaBigDecimal): Unit = writeNestedObject(value){ writeJavaBigDecimalFields }

  final def writeNestedString(value: String): Unit = {
    if (null == value) return writeLengthDelimitedNull()
    writeLengthPrefixedString(value)
  }

  // Bytes
  final def writeNestedByteArray(value: Array[Byte]): Unit = writeLengthPrefixedBytes(value)
  
  // Ints  
  final def writeNestedInt(value: Int): Unit = writeInt32NoTag(value)
  final def writeNestedUnsignedInt(value: Int): Unit = writeRawVarint32(value)
  final def writeNestedSignedInt(value: Int): Unit = writeRawVarint32(encodeZigZag32(value))
  final def writeNestedFixedInt(value: Int): Unit = writeRawLittleEndian32(value)
  
  // Longs
  final def writeNestedLong(value: Long): Unit = writeRawVarint64(value)
  final def writeNestedUnsignedLong(value: Long): Unit = writeRawVarint64(value)
  final def writeNestedSignedLong(value: Long): Unit = writeRawVarint64(encodeZigZag64(value))
  final def writeNestedFixedLong(value: Long): Unit = writeRawLittleEndian64(value)
  
  // Objects
  final def writeNestedObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    if (null == obj) throw new IllegalArgumentException("Cannot write a null nested object")
    //if (null == obj) return writeLengthDelimitedNull()
    //else if (EncodeMessagesAsGroups) writeNestedGroupEncodedMessage(obj)(f)
    //else writeLengthDelimited(obj)(f)
    
    f(this, obj)
    writeTag(0, WireFormat.WIRETYPE_END_GROUP)
  }
  
//  private def writeNestedGroupEncodedMessage[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
//    f(this, obj)
//    writeTag(0, WireFormat.WIRETYPE_END_GROUP)
//  }
  
  // Collections
  final def writeNestedCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null == col) writeLengthDelimitedNull() else writeLengthDelimited(col)(f)
  }
  
  //
  // FIELD Output Implementation
  //

  final def writeFieldBool(number: Int, name: String, value: Boolean): Unit = {
    val tag: Int = makeTag(number, WireFormat.WIRETYPE_VARINT)    

    os.ensureAvailable(computeRawVarint32Size(tag) + 1)
    
    val array: Array[Byte] = os.array
    var offset: Int = os.offset
    
    offset = writeRawVarint32(tag, array, offset)
    array(offset) = if (value) 1.toByte else 0.toByte
    
    os.offset = offset + 1   
  }
  
  final def writeFieldFloat(number: Int, name: String, value: Float): Unit = {
    val tag: Int = makeTag(number, WireFormat.WIRETYPE_FIXED32_LE)
    os.ensureAvailable(computeRawVarint32Size(tag) + 4)
    
    val array: Array[Byte] = os.array
    var offset: Int = os.offset
    
    offset = writeRawVarint32(tag, array, offset)
    os.offset = writeRawLittleEndian32(java.lang.Float.floatToRawIntBits(value), array, offset)
  }
  
  final def writeFieldDouble(number: Int, name: String, value: Double): Unit = {
    val tag: Int = makeTag(number, WireFormat.WIRETYPE_FIXED64_LE)
    os.ensureAvailable(computeRawVarint32Size(tag) + 8)
    
    val array: Array[Byte] = os.array
    var offset: Int = os.offset
    
    offset = writeRawVarint32(tag, array, offset)
    os.offset = writeRawLittleEndian64(java.lang.Double.doubleToRawLongBits(value), array, offset)
  }

  final def writeFieldBigInteger(number: Int, name: String, value: JavaBigInteger): Unit = {
    if (null == value) return writeFieldNull(number, name)

    writeFieldByteArray(number, name, value.toByteArray)
  }

  final def writeFieldBigDecimal(number: Int, name: String, value: JavaBigDecimal): Unit = {
    if (null == value) return writeFieldNull(number, name)

    writeFieldObject(number, name, value){ writeJavaBigDecimalFields }
  }

  final def writeFieldString(number: Int, name: String, value: String): Unit = {
    if (null == value) return writeFieldNull(number, name)

    writeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    writeLengthPrefixedString(value)
  }

  // Bytes
  final def writeFieldByteArray(number: Int, name: String, value: Array[Byte]): Unit = {
    if (null == value) return writeFieldNull(number, name)

    writeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    writeLengthPrefixedBytes(value)
  }
  
  // Ints  
  final def writeFieldInt(number: Int, name: String, value: Int): Unit = {
    val tag: Int = makeTag(number, WireFormat.WIRETYPE_VARINT)
    val tagSize: Int = computeRawVarint32Size(tag)
    val valueSize: Int = computeRawVarint32Size(value)
    
    os.ensureAvailable(tagSize + valueSize)
    
    val array: Array[Byte] = os.array
    var offset: Int = os.offset
    
    offset = writeRawVarint32(tag, array, offset)
    offset = writeRawVarint32(value, array, offset)

    os.offset = offset
  }
  
  final def writeFieldUnsignedInt(number: Int, name: String, value: Int): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeRawVarint32(value)
  }
  
  final def writeFieldSignedInt(number: Int, name: String, value: Int): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeRawVarint32(encodeZigZag32(value))
  }
  
  final def writeFieldFixedInt(number: Int, name: String, value: Int): Unit = {
    writeTag(number, WireFormat.WIRETYPE_FIXED32_LE)
    writeRawLittleEndian32(value)
  }
  
  // Longs
  final def writeFieldLong(number: Int, name: String, value: Long): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeRawVarint64(value)
  }
  
  final def writeFieldUnsignedLong(number: Int, name: String, value: Long): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeRawVarint64(value)
  }
  
  final def writeFieldSignedLong(number: Int, name: String, value: Long): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeRawVarint64(encodeZigZag64(value))
  }
  
  final def writeFieldFixedLong(number: Int, name: String, value: Long): Unit = {
    writeTag(number, WireFormat.WIRETYPE_FIXED64_LE)
    writeRawLittleEndian64(value)
  }
  
  // Object Field
  final def writeFieldObject[T](number: Int, name: String, obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    if (null == obj) return writeFieldNull(number, name)
    
    writeTag(number, WireFormat.WIRETYPE_START_GROUP)
    f(this, obj)
    writeTag(number, WireFormat.WIRETYPE_END_GROUP)
    
    //if (null == obj) writeFieldNull(number, name)
    //else writeFieldGroupEncodedMessage(number, name, obj)(f)
    
//    else if (EncodeMessagesAsGroups) writeFieldGroupEncodedMessage(number, name, obj)(f)
//    else writeFieldLengthDelimitedObject(number, name, obj)(f)
  }
  
//  final def writeFieldLengthDelimitedObject[T](number: Int, name: String, obj: T)(f: (ProtobufOutput, T) => Unit): Unit = {
//    writeLengthDelimitedWithTag(makeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED), obj)(f)
//  }
//  
//  final def writeFieldGroupEncodedMessage[T](number: Int, name: String, obj: T)(f: (ProtobufOutput, T) => Unit): Unit = {
//    writeTag(number, WireFormat.WIRETYPE_START_GROUP)
//    f(this, obj)
//    writeTag(number, WireFormat.WIRETYPE_END_GROUP)
//  }
  
  // Collection Field
  final def writeFieldCollection[T](number: Int, name: String, col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null == col) return writeFieldNull(number, name)
    
    writeLengthDelimitedWithTag(makeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED), col)(f)
  }
  
  // Null Field Value
  @inline final def writeFieldNull(number: Int, name: String): Unit = writeTag(number, WireFormat.WIRETYPE_NULL)
  
  //
  // Protocol Buffers Implementation
  //
  
  private def writeBoolNoTag(value: Boolean): Unit = writeRawByte(if (value) 1 else 0)
  
  private def writeFloatNoTag(value: Float): Unit = writeRawLittleEndian32(java.lang.Float.floatToRawIntBits(value))
  private def writeDoubleNoTag(value: Double): Unit = writeRawLittleEndian64(java.lang.Double.doubleToRawLongBits(value))
  
  private def writeInt32NoTag(value: Int): Unit = {
    if (value >= 0) writeRawVarint32(value)
    else writeRawVarint64(value)  // Must sign-extend.
  }
  
  /**
   * Encode a ZigZag-encoded 32-bit value.  ZigZag encodes signed integers
   * into values that can be efficiently encoded with varint.  (Otherwise,
   * negative values must be sign-extended to 64 bits to be varint encoded,
   * thus always taking 10 bytes on the wire.)
   *
   * @param n A signed 32-bit integer.
   * @return An unsigned 32-bit integer, stored in a signed int because
   *         Java has no explicit unsigned support.
   */
  private def encodeZigZag32(n: Int): Int = (n << 1) ^ (n >> 31)
  
  /**
   * Encode a ZigZag-encoded 64-bit value.  ZigZag encodes signed integers
   * into values that can be efficiently encoded with varint.  (Otherwise,
   * negative values must be sign-extended to 64 bits to be varint encoded,
   * thus always taking 10 bytes on the wire.)
   *
   * @param n A signed 64-bit integer.
   * @return An unsigned 64-bit integer, stored in a signed int because
   *         Java has no explicit unsigned support.
   */
  private def encodeZigZag64(n: Long): Long = (n << 1) ^ (n >> 63)
  
  private def writeLengthPrefixedBytes(value: Array[Byte]): Unit = {
    writeRawVarint32(value.length)
    writeRawBytes(value)
  }
  
  private def writeLengthPrefixedString(value: String): Unit = {
    val maxPrefixSize: Int = computeRawVarint32Size(value.length * 3 /* FMByteArrayOutputStream.MAX_UTF8_CHAR_BYTES */)
    
    // This is the maximum size varint32 size that the prefix will take to represent the length of the string
    os.ensureAvailable(maxPrefixSize)
    
    // Save references to the array/offset we will write the prefix to
    val array: Array[Byte] = os.array
    val offset: Int = os.offset
    
    // Skip over the prefix size
    os.offset = offset + maxPrefixSize
    
    // Write out the string bytes
    val bytesWritten: Int = os.writeUTF8Bytes(value)
    
    // Write the length
    val endingOffset: Int = writeRawVarint32(bytesWritten, array, offset)

    val actualPrefixSize: Int = endingOffset - offset
    
    val diff: Int = maxPrefixSize - actualPrefixSize
    if (diff > 1) throw new AssertionError("Expected diff to be <= 1")
    
    // If we have a gap in the array between the prefixed size and the data
    // then we will extends the varint32 by 1 byte (which should be the max
    // gap size)
    if (diff == 1) {
      // Flip the leading bit of the last byte of the varint to 1 to signal
      // that the varint has another byte
      array(endingOffset - 1) = (array(endingOffset - 1) | 0x80).toByte
      
      // The final byte is just zero
      array(endingOffset) = 0
    }
  }
  
//  private def writeLengthPrefixedStringFast(value: String, prefixSize: Int): Unit = {
//    // Fast Path -- We know the exact size of the length prefix
//    os.ensureAvailable(prefixSize)
//    
//    // Save references to the array/offset we will write the prefix to
//    val array: Array[Byte] = os.array
//    val offset: Int = os.offset
//    
//    // Skip over the prefix size
//    os.offset = offset + prefixSize
//    
//    // Write out the string bytes
//    val bytesWritten: Int = os.writeUTF8Bytes(value)
//    
//    // Write the length prefix
//    writeRawVarint32(bytesWritten, array, offset)
//  }
//  
//  private def writeLengthPrefixedStringSlow(value: String, maxPrefixSize: Int): Unit = {    
//    // Slow path -- Might involve compacting/splicing
//    os.lengthPrefixed(maxPrefixSize) {
//      os.writeUTF8Bytes(value)
//    } { length: Int =>
//      os.offset = writeRawVarint32(length, os.array, os.offset)
//    }
//  }
  
  /**
   * Note: This is a very inefficient way to encode null (it takes 10 bytes!!!)
   * but it should be a very rare case since we have a separate encoding for null
   * fields that will be used the majority of the time.
   * 
   * The only time this should get called is for null values within a collection
   * or nested collection which is hopefully a rare case.
   * 
   * e.g. List("1","2",null,"4") or List(List("1","2"), List(null, null), List("5","6"))
   */
  private def writeLengthDelimitedNull(): Unit = {
    // Since it's negative we have to sign extend which is why we call
    // writeRawVarint64 instead of writeRawVarint32.
    writeRawVarint64(-1)
  }

  private def writeLengthDelimited[T](obj: T)(f: (ProtobufOutput, T) => Unit): Unit = {
    // Note: os.lengthPrefixed is marked as @inline to eliminate these closures
    os.lengthPrefixed(MaxVarint32Bytes){ f(this, obj) }{ writeRawVarint32 }
  }
  
  private def writeLengthDelimitedWithTag[T](tag: Int, obj: T)(f: (ProtobufOutput, T) => Unit): Unit = {
    val tagSize: Int = computeRawVarint32Size(tag)
    
    // Note: os.lengthPrefixed is marked as @inline to eliminate these closures
    os.lengthPrefixed(tagSize + MaxVarint32Bytes){ f(this, obj) }{ (length: Int) =>
      val lengthSize: Int = computeRawVarint32Size(length)
      
      val array: Array[Byte] = os.array
      var offset: Int = os.offset
      
      offset = writeRawVarint32(tag, array, offset)
      offset = writeRawVarint32(length, array, offset)
      os.offset = offset
    }

  }
  
  /** Encode and write a tag. */
  @inline private def writeTag(number: Int, wireType: Int): Unit = {
    //println(s"writeTag() => ${makeTag(number, wireType)}  fieldNumber: ${number}  wireType: ${wireType}")
    writeRawVarint32(makeTag(number, wireType))
  }

  /** Write a little-endian 32-bit integer. */
  private def writeRawLittleEndian32(value: Int): Unit = {
    os.ensureAvailable(4)
    os.offset = writeRawLittleEndian32(value, os.array, os.offset)
  }
  
  /** Write a little-endian 32-bit integer. */
  private def writeRawLittleEndian32(value: Int, array: Array[Byte], offset: Int): Int = {
    array(offset)   = ((value       ) & 0xFF).toByte
    array(offset+1) = ((value >>>  8) & 0xFF).toByte
    array(offset+2) = ((value >>> 16) & 0xFF).toByte
    array(offset+3) = ((value >>> 24) & 0xFF).toByte
    
    offset + 4
  }
  
  /** Write a little-endian 64-bit integer. */
  private def writeRawLittleEndian64(value: Long): Unit = {
    os.ensureAvailable(8)
    os.offset = writeRawLittleEndian64(value, os.array, os.offset)
  }
  
  /** Write a little-endian 64-bit integer. */
  private def writeRawLittleEndian64(value: Long, array: Array[Byte], offset: Int): Int = {
    array(offset)   = (value       ).toByte
    array(offset+1) = (value >>>  8).toByte
    array(offset+2) = (value >>> 16).toByte
    array(offset+3) = (value >>> 24).toByte
    array(offset+4) = (value >>> 32).toByte
    array(offset+5) = (value >>> 40).toByte
    array(offset+6) = (value >>> 48).toByte
    array(offset+7) = (value >>> 56).toByte
    
    offset + 8
  }
  
  /** Write a big-endian 32-bit integer. */
  private def writeRawBigEndian32(value: Int): Unit = {
    os.ensureAvailable(4)
    os.offset = writeRawBigEndian32(value, os.array, os.offset)
  }
  
  /** Write a big-endian 32-bit integer. */
  private def writeRawBigEndian32(value: Int, array: Array[Byte], offset: Int): Int = {
    array(offset)   = ((value >>> 24) & 0xFF).toByte
    array(offset+1) = ((value >>> 16) & 0xFF).toByte
    array(offset+2) = ((value >>>  8) & 0xFF).toByte
    array(offset+3) = ((value       ) & 0xFF).toByte
    
    offset + 4
  }
  
  /** Write a big-endian 64-bit integer. */
  private def writeRawBigEndian64(value: Long): Unit = {
    os.ensureAvailable(8)
    os.offset = writeRawBigEndian64(value, os.array, os.offset)
  }
  
  /** Write a big-endian 64-bit integer. */
  private def writeRawBigEndian64(value: Long, array: Array[Byte], offset: Int): Int = {
    array(offset)   = (value >>> 56).toByte
    array(offset+1) = (value >>> 48).toByte
    array(offset+2) = (value >>> 40).toByte
    array(offset+3) = (value >>> 32).toByte
    array(offset+4) = (value >>> 24).toByte
    array(offset+5) = (value >>> 16).toByte
    array(offset+6) = (value >>>  8).toByte
    array(offset+7) = (value       ).toByte
    
    offset + 8
  }
  
  private def writeRawVarint32(value: Int): Unit = {
    os.ensureAvailable(computeRawVarint32Size(value))
    os.offset = writeRawVarint32(value, os.array, os.offset)
  }
  
  /**
   * Encode and write a varint.  {@code value} is treated as
   * unsigned, so it won't be sign-extended if negative.
   */
  @tailrec
  final def writeRawVarint32(value: Int, arr: Array[Byte], off: Int): Int = {
    if ((value & ~0x7F) == 0) {
      arr(off) = value.toByte
      off + 1
    } else {
      arr(off) = ((value & 0x7F) | 0x80).toByte
      writeRawVarint32(value >>> 7, arr, off + 1)
    }
  }
  
  private def writeRawVarint64(value: Long): Unit = {
    os.ensureAvailable(computeRawVarint64Size(value))
    os.offset = writeRawVarint64(value, os.array, os.offset)
  }
  
  /** Encode and write a varint64. */
  @tailrec
  final def writeRawVarint64(value: Long, arr: Array[Byte], off: Int): Int = {
    if ((value & ~0x7FL) == 0) {
      arr(off) = value.toByte
      off + 1
    } else {
      arr(off) = ((value.toInt & 0x7F) | 0x80).toByte
      writeRawVarint64(value >>> 7, arr, off + 1)
    }
  }

  /**
   * Compute the number of bytes that would be needed to encode a varint.
   * {@code value} is treated as unsigned, so it won't be sign-extended if
   * negative.
   */  
  @inline private def computeRawVarint32Size(value: Int): Int = computeRawVarint32Size0(value, 0xffffffff << 7, 1)
  
  @tailrec
  private def computeRawVarint32Size0(value: Int, mask: Int, size: Int): Int = {
    if ((value & mask) == 0) return size
    computeRawVarint32Size0(value, mask << 7, size + 1)
  }

    
  /** Compute the number of bytes that would be needed to encode a varint64 */
  @inline private def computeRawVarint64Size(value: Long): Int = computeRawVarint64Size0(value, 0xffffffffffffffffL << 7, 1)
  
  @tailrec
  private def computeRawVarint64Size0(value: Long, mask: Long, size: Int): Int = {
    if ((value & mask) == 0) return size
    computeRawVarint64Size0(value, mask << 7, size + 1)
  }
  
  /** Copied From WireType.makeTag */
  @inline final def makeTag(fieldNumber: Int, wireType: Int): Int = (fieldNumber << 3) | wireType
}