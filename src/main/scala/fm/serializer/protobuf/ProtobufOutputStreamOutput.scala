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

import scala.annotation.tailrec
import java.io.OutputStream

import fm.serializer.{FieldOutput, NestedOutput, Output}
import fm.serializer.fastutil.FastByteArrayOutputStream

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
final class ProtobufOutputStreamOutput(private var os: OutputStream) extends Output {
  def allowStringMap: Boolean = false
  
  private def writeRawByte(byte: Int): Unit = os.write(byte)  
  private def writeRawBytes(bytes: Array[Byte]): Unit = os.write(bytes)
  
  //
  // RAW Output Implementation
  //
  
  // Basic
  final def writeRawBool(value: Boolean): Unit = writeBoolNoTag(value)
  final def writeRawFloat(value: Float): Unit = writeFloatNoTag(value)
  final def writeRawDouble(value: Double): Unit = writeDoubleNoTag(value)
  
  final def writeRawString(value: String): Unit = {
    require(null != value, "Can't write a null rawString value")
    
    // No Length Prefix
    val bytes: Array[Byte] = value.getBytes("UTF-8")
    writeRawBytes(bytes)
  }

  // Bytes
  final def writeRawByteArray(value: Array[Byte]): Unit = {
    // No Length Prefix
    writeRawBytes(value)
  }
  
  // Ints  
  final def writeRawInt(value: Int): Unit = writeInt32NoTag(value)
  final def writeRawUnsignedInt(value: Int): Unit = writeUInt32NoTag(value)
  final def writeRawSignedInt(value: Int): Unit = writeSInt32NoTag(value)
  final def writeRawFixedInt(value: Int): Unit = writeFixed32NoTag(value)
  
  // Longs
  final def writeRawLong(value: Long): Unit = writeInt64NoTag(value)
  final def writeRawUnsignedLong(value: Long): Unit = writeUInt64NoTag(value)
  final def writeRawSignedLong(value: Long): Unit = writeSInt64NoTag(value)
  final def writeRawFixedLong(value: Long): Unit = writeFixed64NoTag(value)
  
  // Objects
  final def writeRawObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    require(null != obj, "Cannot write raw null object")
    f(this, obj)
  }
  
  // Colletions
  final def writeRawCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = {
    require(null != col, "Cannot write a raw null collection")
    f(this, col)
  }
  
  //
  // NESTED Output Implementation
  //
  
  // Basic
  final def writeNestedBool(value: Boolean): Unit = writeBoolNoTag(value)
  final def writeNestedFloat(value: Float): Unit = writeFloatNoTag(value)
  final def writeNestedDouble(value: Double): Unit = writeDoubleNoTag(value)
  
  final def writeNestedString(value: String): Unit = {
    if (null == value) writeLengthDelimitedNull() else writeStringNoTag(value)
  }

  // Bytes
  final def writeNestedByteArray(value: Array[Byte]): Unit = writeBytesNoTag(value)
  
  // Ints  
  final def writeNestedInt(value: Int): Unit = writeInt32NoTag(value)
  final def writeNestedUnsignedInt(value: Int): Unit = writeUInt32NoTag(value)
  final def writeNestedSignedInt(value: Int): Unit = writeSInt32NoTag(value)
  final def writeNestedFixedInt(value: Int): Unit = writeFixed32NoTag(value)
  
  // Longs
  final def writeNestedLong(value: Long): Unit = writeInt64NoTag(value)
  final def writeNestedUnsignedLong(value: Long): Unit = writeUInt64NoTag(value)
  final def writeNestedSignedLong(value: Long): Unit = writeSInt64NoTag(value)
  final def writeNestedFixedLong(value: Long): Unit = writeFixed64NoTag(value)
  
  // Objects
  final def writeNestedObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    require(null != obj)
    f(this, obj)
    writeTag(0, WireFormat.WIRETYPE_END_GROUP)
    
    //if (null == obj) writeLengthDelimitedNull() else writeMessageNoTag(obj)(f)
  }
  
  // Colletions
  final def writeNestedCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null == col) writeLengthDelimitedNull() else writeLengthDelimited(col)(f)
  }

  //
  // FIELD Output Implementation
  //

  final def writeFieldBool(number: Int, name: String, value: Boolean): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeBoolNoTag(value)
  }
  
  final def writeFieldFloat(number: Int, name: String, value: Float): Unit = {
    writeTag(number, WireFormat.WIRETYPE_FIXED32_LE)
    writeFloatNoTag(value)
  }
  
  final def writeFieldDouble(number: Int, name: String, value: Double): Unit = {
    writeTag(number, WireFormat.WIRETYPE_FIXED64_LE)
    writeDoubleNoTag(value)
  }
  
  final def writeFieldString(number: Int, name: String, value: String): Unit = {
    if (null == value) {
      writeFieldNull(number, name) 
    } else {
      writeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED)
      writeStringNoTag(value)
    }
  }
  
  // Bytes
  final def writeFieldByteArray(number: Int, name: String, value: Array[Byte]): Unit = {
    if (null == value) {
      writeFieldNull(number, name)
    } else {
      writeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED)
      writeBytesNoTag(value)
    }
  }
  
  // Ints  
  final def writeFieldInt(number: Int, name: String, value: Int): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeInt32NoTag(value)
  }
  
  final def writeFieldUnsignedInt(number: Int, name: String, value: Int): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeUInt32NoTag(value)
  }
  
  final def writeFieldSignedInt(number: Int, name: String, value: Int): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeSInt32NoTag(value)
  }
  
  final def writeFieldFixedInt(number: Int, name: String, value: Int): Unit = {
    writeTag(number, WireFormat.WIRETYPE_FIXED32_LE)
    writeFixed32NoTag(value)
  }
  
  // Longs
  final def writeFieldLong(number: Int, name: String, value: Long): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeInt64NoTag(value)
  }
  
  final def writeFieldUnsignedLong(number: Int, name: String, value: Long): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeUInt64NoTag(value)
  }
  
  final def writeFieldSignedLong(number: Int, name: String, value: Long): Unit = {
    writeTag(number, WireFormat.WIRETYPE_VARINT)
    writeSInt64NoTag(value)
  }
  
  final def writeFieldFixedLong(number: Int, name: String, value: Long): Unit = {
    writeTag(number, WireFormat.WIRETYPE_FIXED64_LE)
    writeFixed64NoTag(value)
  }
  
  // Object Field
  final def writeFieldObject[T](number: Int, name: String, obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    if (null == obj) writeFieldNull(number, name) else {
      // writeFieldLengthDelimitedObject(number, name, obj)(f)
      writeTag(number, WireFormat.WIRETYPE_START_GROUP)
      f(this, obj)
      writeTag(number, WireFormat.WIRETYPE_END_GROUP)
    }
  }
  
  final def writeFieldLengthDelimitedObject[T](number: Int, name: String, obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    writeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED)
    writeMessageNoTag(obj)(f)
  }
  
  // Collection Field
  final def writeFieldCollection[T](number: Int, name: String, col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null == col) writeFieldNull(number, name) else {
      writeTag(number, WireFormat.WIRETYPE_LENGTH_DELIMITED)
      writeLengthDelimited(col)(f)
    }
  }
  
  // Null Field Value
  def writeFieldNull(number: Int, name: String): Unit = {
    writeTag(number, WireFormat.WIRETYPE_NULL)
  }
  
  //
  // Protocol Buffers Implementation
  //
  
  final def writeBoolNoTag(value: Boolean): Unit = writeRawByte(if (value) 1 else 0)
  
  final def writeFloatNoTag(value: Float): Unit = writeRawLittleEndian32(java.lang.Float.floatToRawIntBits(value))
  final def writeDoubleNoTag(value: Double): Unit = writeRawLittleEndian64(java.lang.Double.doubleToRawLongBits(value))
  
  final def writeInt32NoTag(value: Int): Unit = {
    if (value >= 0) writeRawVarint32(value)
    else writeRawVarint64(value)  // Must sign-extend.
  }
  
  final def writeInt64NoTag(value: Long): Unit = writeRawVarint64(value)
  
  final def writeUInt32NoTag(value: Int): Unit = writeRawVarint32(value)
  final def writeUInt64NoTag(value: Long): Unit = writeRawVarint64(value)
  
  final def writeSInt32NoTag(value: Int): Unit = writeRawVarint32(encodeZigZag32(value))
  final def writeSInt64NoTag(value: Long): Unit = writeRawVarint64(encodeZigZag64(value))

  final def writeFixed32NoTag(value: Int): Unit = writeRawLittleEndian32(value)
  final def writeFixed64NoTag(value: Long): Unit = writeRawLittleEndian64(value)
  
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
  final def encodeZigZag32(n: Int): Int = (n << 1) ^ (n >> 31)
  
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
  final def encodeZigZag64(n: Long): Long = (n << 1) ^ (n >> 63)
  
  final def writeBytesNoTag(value: Array[Byte]): Unit = {
    writeRawVarint32(value.length)
    writeRawBytes(value)
  }
  
  final def writeStringNoTag(value: String): Unit = {
    val bytes: Array[Byte] = value.getBytes("UTF-8")
    writeRawVarint32(bytes.length)
    writeRawBytes(bytes)
  }
  
  final def writeMessageNoTag[T](obj: T)(f: (Output, T) => Unit): Unit = writeLengthDelimited(obj)(f)
  
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
  final def writeLengthDelimitedNull(): Unit = {
    // Since it's negative we have to sign extend which is why we call
    // writeRawVarint64 instead of writeRawVarint32.
    writeRawVarint64(-1)
  }
  
  final def writeLengthDelimited[T](obj: T)(f: (Output, T) => Unit): Unit = {
    //println(s"writeMessageNoTag - START")
    
    // Save the old output stream
    val oldOutputStream: OutputStream = os
    
    val byteArrayOS: FastByteArrayOutputStream = new FastByteArrayOutputStream()
    
    os = byteArrayOS
    
    f(this, obj)
    
    // Restore the original output stream
    os = oldOutputStream
    
    // Length of the embedded message
    writeRawVarint32(byteArrayOS.length)
    
    // The contents of the embedded message
    os.write(byteArrayOS.array, 0, byteArrayOS.length)
    
    //println(s"writeMessageNoTag - END - size: ${byteArrayOS.size}, contents: "+byteArrayOS.toByteArray.toSeq)
  }
  
  /** Encode and write a tag. */
  final def writeTag(number: Int, wireType: Int): Unit = {
    //println(s"writeTag() => ${WireFormat.makeTag(number, wireType)}  fieldNumber: ${number}  wireType: ${wireType}")
    writeRawVarint32(WireFormat.makeTag(number, wireType))
  }
  
  /** Write a little-endian 32-bit integer. */
 final  def writeRawLittleEndian32(value: Int): Unit = {
    writeRawByte((value       ) & 0xFF)
    writeRawByte((value >>>  8) & 0xFF)
    writeRawByte((value >>> 16) & 0xFF)
    writeRawByte((value >>> 24) & 0xFF)
  }
  
  /** Write a little-endian 64-bit integer. */
  final def writeRawLittleEndian64(value: Long): Unit = {
    writeRawByte((value       ).toInt & 0xFF)
    writeRawByte((value >>>  8).toInt & 0xFF)
    writeRawByte((value >>> 16).toInt & 0xFF)
    writeRawByte((value >>> 24).toInt & 0xFF)
    writeRawByte((value >>> 32).toInt & 0xFF)
    writeRawByte((value >>> 40).toInt & 0xFF)
    writeRawByte((value >>> 48).toInt & 0xFF)
    writeRawByte((value >>> 56).toInt & 0xFF)
  }
  
  /** Write a big-endian 32-bit integer. */
  final def writeRawBigEndian32(value: Int): Unit = {
    writeRawByte((value >>> 24) & 0xFF)
    writeRawByte((value >>> 16) & 0xFF)
    writeRawByte((value >>>  8) & 0xFF)
    writeRawByte((value       ) & 0xFF)
  }
  
  /** Write a big-endian 64-bit integer. */
  final def writeRawBigEndian64(value: Long): Unit = {
    writeRawByte((value >>> 56).toInt & 0xFF)
    writeRawByte((value >>> 48).toInt & 0xFF)
    writeRawByte((value >>> 40).toInt & 0xFF)
    writeRawByte((value >>> 32).toInt & 0xFF)
    writeRawByte((value >>> 24).toInt & 0xFF)
    writeRawByte((value >>> 16).toInt & 0xFF)
    writeRawByte((value >>>  8).toInt & 0xFF)
    writeRawByte((value       ).toInt & 0xFF)
  }
  
  /**
   * Encode and write a varint.  {@code value} is treated as
   * unsigned, so it won't be sign-extended if negative.
   */
  @tailrec
  final def writeRawVarint32(value: Int): Unit = {
    if ((value & ~0x7F) == 0) writeRawByte(value)
    else {
      writeRawByte((value & 0x7F) | 0x80)
      writeRawVarint32(value >>> 7)
    }
  }
  
  /** Encode and write a varint. */
  @tailrec
  final def writeRawVarint64(value: Long): Unit = {
    if ((value & ~0x7FL) == 0) writeRawByte(value.toInt)
    else {
      writeRawByte((value.toInt & 0x7F) | 0x80)
      writeRawVarint64(value >>> 7)
    }
  }

}