/*
 * Copyright 2019 Frugal Mechanic (http://frugalmechanic.com)
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
package fm.serializer.jackson

import com.fasterxml.jackson.core.JsonGenerator
import fm.serializer.json.JSONSerializerOptions
import fm.serializer.{FieldOutput, NestedOutput, Output}
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}

/**
 * An Output implementation on top of Jackson's JsonGenerator
 */
final class JsonGeneratorOutput(generator: JsonGenerator, options: JSONSerializerOptions) extends Output {
  def this(generator: JsonGenerator) = this(generator, JSONSerializerOptions.default)

  override def allowStringMap: Boolean = true

  private def writeFieldName(name: String): Unit = {
    generator.writeFieldName(name)
  }

  override def writeFieldBool(number: Int, name: String, value: Boolean): Unit = {
    if (options.outputFalse || value) {
      writeFieldName(name)
      writeRawBool(value)
    }
  }

  override def writeFieldFloat(number: Int, name: String, value: Float): Unit = {
    if (options.outputZeros || value != 0f) {
      writeFieldName(name)
      writeRawFloat(value)
    }
  }

  override def writeFieldDouble(number: Int, name: String, value: Double): Unit = {
    if (options.outputZeros || value != 0d) {
      writeFieldName(name)
      writeRawDouble(value)
    }
  }

  override def writeFieldBigInteger(number: Int, name: String, value: JavaBigInteger): Unit = {
    if (null != value || options.outputNulls) {
      writeFieldName(name)
      writeRawBigInteger(value)
    }
  }

  override def writeFieldBigDecimal(number: Int, name: String, value: JavaBigDecimal): Unit = {
    if (null != value || options.outputNulls) {
      writeFieldName(name)
      writeRawBigDecimal(value)
    }
  }

  override def writeFieldString(number: Int, name: String, value: String): Unit = {
    if (null != value || options.outputNulls) {
      writeFieldName(name)
      writeRawString(value)
    }
  }

  override def writeFieldByteArray(number: Int, name: String, value: Array[Byte]): Unit = {
    if (null != value || options.outputNulls) {
      writeFieldName(name)
      writeRawByteArray(value)
    }
  }

  override def writeFieldInt(number: Int, name: String, value: Int): Unit = {
    if (options.outputZeros || value != 0) {
      writeFieldName(name)
      writeRawInt(value)
    }
  }

  override def writeFieldUnsignedInt(number: Int, name: String, value: Int): Unit = writeFieldInt(number, name, value)
  override def writeFieldSignedInt(number: Int, name: String, value: Int): Unit = writeFieldInt(number, name, value)
  override def writeFieldFixedInt(number: Int, name: String, value: Int): Unit = writeFieldInt(number, name, value)

  override def writeFieldLong(number: Int, name: String, value: Long): Unit = {
    if (options.outputZeros || value != 0L) {
      writeFieldName(name)
      writeRawLong(value)
    }
  }

  override def writeFieldUnsignedLong(number: Int, name: String, value: Long): Unit = writeFieldLong(number, name, value)
  override def writeFieldSignedLong(number: Int, name: String, value: Long): Unit = writeFieldLong(number, name, value)
  override def writeFieldFixedLong(number: Int, name: String, value: Long): Unit = writeFieldLong(number, name, value)

  override def writeFieldObject[T](number: Int, name: String, obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    if (null != obj || options.outputNulls) {
      writeFieldName(name)
      writeRawObject(obj)(f)
    }
  }

  override def writeFieldCollection[T](number: Int, name: String, col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null != col || options.outputNulls) {
      writeFieldName(name)
      writeRawCollection(col)(f)
    }
  }

  override def writeFieldNull(number: Int, name: String): Unit = {
    if (options.outputNulls) {
      writeFieldName(name)
      writeNull()
    }
  }

  override def writeRawBool(value: Boolean): Unit = generator.writeBoolean(value)
  override def writeRawFloat(value: Float): Unit = generator.writeNumber(value)
  override def writeRawDouble(value: Double): Unit = generator.writeNumber(value)
  override def writeRawBigInteger(value: JavaBigInteger): Unit = generator.writeNumber(value)
  override def writeRawBigDecimal(value: JavaBigDecimal): Unit = generator.writeNumber(value)
  override def writeRawString(value: String): Unit = generator.writeString(value)

  override def writeRawByteArray(value: Array[Byte]): Unit = {
    if (null == value) writeNull()
    else generator.writeBinary(value)
  }

  override def writeRawInt(value: Int): Unit = generator.writeNumber(value)
  override def writeRawUnsignedInt(value: Int): Unit = writeRawInt(value)
  override def writeRawSignedInt(value: Int): Unit = writeRawInt(value)
  override def writeRawFixedInt(value: Int): Unit = writeRawInt(value)
  override def writeRawLong(value: Long): Unit = generator.writeNumber(value)
  override def writeRawUnsignedLong(value: Long): Unit = writeRawLong(value)
  override def writeRawSignedLong(value: Long): Unit = writeRawLong(value)
  override def writeRawFixedLong(value: Long): Unit = writeRawLong(value)

  /**
   * For writing objects.  Note: that the obj is passed in for null handling
   * by the implementation.  If the object is not null then the function f
   * will be called so the caller can write out the fields
   */
  override def writeRawObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    if (null == obj) {
      writeNull()
    } else {
      generator.writeStartObject()
      f(this, obj)
      generator.writeEndObject()
    }
  }

  /**
   * Write out a RAW collection.  This method will wrap
   * the collection in whatever leading/trailing "stuff"
   * is needed (e.g. length prefixing, leading/trailing
   * chars, etc...).  The method that you pass in should
   * use the Output instance to make repeated calls to
   * a single write
   */
  override def writeRawCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null == col) {
      writeNull()
    } else {
      generator.writeStartArray()
      f(this, col)
      generator.writeEndArray()
    }
  }

  override def writeNestedBool(value: Boolean): Unit = writeRawBool(value)
  override def writeNestedFloat(value: Float): Unit = writeRawFloat(value)
  override def writeNestedDouble(value: Double): Unit = writeRawDouble(value)
  override def writeNestedString(value: String): Unit = writeRawString(value)
  override def writeNestedBigInteger(value: JavaBigInteger): Unit = writeRawBigInteger(value)
  override def writeNestedBigDecimal(value: JavaBigDecimal): Unit = writeRawBigDecimal(value)
  override def writeNestedByteArray(value: Array[Byte]): Unit = writeRawByteArray(value)
  override def writeNestedInt(value: Int): Unit = writeRawInt(value)
  override def writeNestedUnsignedInt(value: Int): Unit = writeRawUnsignedInt(value)
  override def writeNestedSignedInt(value: Int): Unit = writeRawSignedInt(value)
  override def writeNestedFixedInt(value: Int): Unit = writeRawFixedInt(value)
  override def writeNestedLong(value: Long): Unit = writeRawLong(value)
  override def writeNestedUnsignedLong(value: Long): Unit = writeRawUnsignedLong(value)
  override def writeNestedSignedLong(value: Long): Unit = writeRawSignedLong(value)
  override def writeNestedFixedLong(value: Long): Unit = writeRawFixedLong(value)
  override def writeNestedObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = writeRawObject(obj)(f)
  override def writeNestedCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = writeRawCollection(col)(f)

  private def writeNull(): Unit = generator.writeNull()
}
