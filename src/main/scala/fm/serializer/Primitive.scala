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
package fm.serializer

import fm.common.ImmutableArray
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}

object Primitive extends PrimitiveImplicits {
  val unsignedInt:  UnsignedIntPrimitive  = new UnsignedIntPrimitive()
  val signedInt:    SignedIntPrimitive    = new SignedIntPrimitive()
  val fixedInt:     FixedIntPrimitive     = new FixedIntPrimitive()
  
  val unsignedLong: UnsignedLongPrimitive = new UnsignedLongPrimitive()
  val signedLong:   SignedLongPrimitive   = new SignedLongPrimitive()
  val fixedLong:    FixedLongPrimitive    = new FixedLongPrimitive()
  
  import java.lang.{Integer => JavaInt, Long => JavaLong}
  
  val javaUnsignedInt:  SimpleSerializer[JavaInt] = unsignedInt.map[JavaInt](null)   { _.intValue } { JavaInt.valueOf }
  val javaSignedInt:    SimpleSerializer[JavaInt] = signedInt.map[JavaInt](null)     { _.intValue } { JavaInt.valueOf }
  val javaFixedInt:     SimpleSerializer[JavaInt] = fixedInt.map[JavaInt](null)      { _.intValue } { JavaInt.valueOf }
  
  val javaUnsignedLong: SimpleSerializer[JavaLong] = unsignedLong.map[JavaLong](null) { _.longValue } { JavaLong.valueOf }
  val javaSignedLong:   SimpleSerializer[JavaLong] = signedLong.map[JavaLong](null)   { _.longValue } { JavaLong.valueOf }
  val javaFixedLong:    SimpleSerializer[JavaLong] = fixedLong.map[JavaLong](null)    { _.longValue } { JavaLong.valueOf }
}

sealed trait Primitive[@specialized T] extends SimpleSerializer[T] {
  final def uninitializedValue: T = defaultValue
}

final class BooleanPrimitive extends Primitive[Boolean] {
  def serializeRaw(output: RawOutput, v: Boolean): Unit = output.writeRawBool(v)
  def serializeNested(output: NestedOutput, v: Boolean): Unit = output.writeNestedBool(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Boolean): Unit = output.writeFieldBool(number, name, v)
  
  def defaultValue: Boolean = false
  def deserializeRaw(input: RawInput): Boolean = input.readRawBool()
  def deserializeNested(input: NestedInput): Boolean = input.readNestedBool()
}

final class FloatPrimitive extends Primitive[Float] {
  def serializeRaw(output: RawOutput, v: Float): Unit = output.writeRawFloat(v)
  def serializeNested(output: NestedOutput, v: Float): Unit = output.writeNestedFloat(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Float): Unit = output.writeFieldFloat(number, name, v)
  
  def defaultValue: Float = 0f
  def deserializeRaw(input: RawInput): Float = input.readRawFloat()
  def deserializeNested(input: NestedInput): Float = input.readNestedFloat()
}

final class DoublePrimitive extends Primitive[Double] {
  def serializeRaw(output: RawOutput, v: Double): Unit = output.writeRawDouble(v)
  def serializeNested(output: NestedOutput, v: Double): Unit = output.writeNestedDouble(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Double): Unit = output.writeFieldDouble(number, name, v)
  
  def defaultValue: Double = 0d
  def deserializeRaw(input: RawInput): Double = input.readRawDouble()
  def deserializeNested(input: NestedInput): Double = input.readNestedDouble()
}

final class StringPrimitive extends Primitive[String] {
  def serializeRaw(output: RawOutput, v: String): Unit = output.writeRawString(v)
  def serializeNested(output: NestedOutput, v: String): Unit = output.writeNestedString(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: String): Unit = output.writeFieldString(number, name, v)
  
  def defaultValue: String = null
  def deserializeRaw(input: RawInput): String = input.readRawString()
  def deserializeNested(input: NestedInput): String = input.readNestedString()
}

// We treat chars as Strings
final class CharPrimitive extends Primitive[Char] {
  def serializeRaw(output: RawOutput, v: Char): Unit = output.writeRawString(v.toString)
  def serializeNested(output: NestedOutput, v: Char): Unit = output.writeNestedString(v.toString)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Char): Unit = output.writeFieldString(number, name, v.toString)
  
  def defaultValue: Char = (0: Char)
  def deserializeRaw(input: RawInput): Char = toChar(input.readRawString())
  def deserializeNested(input: NestedInput): Char = toChar(input.readNestedString())
  
  private def toChar(s: String): Char = {
    if (null == s || s.length() == 0) (0: Char)
    else s.charAt(0)
  }
}

final class ByteArrayPrimitive extends Primitive[Array[Byte]] {
  def serializeRaw(output: RawOutput, v: Array[Byte]): Unit = output.writeRawByteArray(v)
  def serializeNested(output: NestedOutput, v: Array[Byte]): Unit = output.writeNestedByteArray(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Array[Byte]): Unit = output.writeFieldByteArray(number, name, v)
  
  def defaultValue: Array[Byte] = null
  def deserializeRaw(input: RawInput): Array[Byte] = input.readRawByteArray()
  def deserializeNested(input: NestedInput): Array[Byte] = input.readNestedByteArray()
}

final class ImmutableByteArrayPrimitive extends Primitive[ImmutableArray[Byte]] {
  def serializeRaw(output: RawOutput, v: ImmutableArray[Byte]): Unit = if (null != v) output.writeRawByteArray(v.toArray)
  def serializeNested(output: NestedOutput, v: ImmutableArray[Byte]): Unit = if (null != v) output.writeNestedByteArray(v.toArray)
  def serializeField(output: FieldOutput, number: Int, name: String, v: ImmutableArray[Byte]): Unit = if (null != v) output.writeFieldByteArray(number, name, v.toArray) else output.writeFieldNull(number, name)

  def defaultValue: ImmutableArray[Byte] = null
  def deserializeRaw(input: RawInput): ImmutableArray[Byte] = if (input.nextValueIsNull) null else ImmutableArray.wrap(input.readRawByteArray())
  def deserializeNested(input: NestedInput): ImmutableArray[Byte] = if (input.nextValueIsNull) null else ImmutableArray.wrap(input.readNestedByteArray())
}

final class IntPrimitive extends Primitive[Int] {
  def serializeRaw(output: RawOutput, v: Int): Unit = output.writeRawInt(v)
  def serializeNested(output: NestedOutput, v: Int): Unit = output.writeNestedInt(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Int): Unit = output.writeFieldInt(number, name, v)
  
  def defaultValue: Int = 0
  def deserializeRaw(input: RawInput): Int = input.readRawInt()
  def deserializeNested(input: NestedInput): Int = input.readNestedInt()
}

final class UnsignedIntPrimitive extends Primitive[Int] {
  def serializeRaw(output: RawOutput, v: Int): Unit = output.writeRawUnsignedInt(v)
  def serializeNested(output: NestedOutput, v: Int): Unit = output.writeNestedUnsignedInt(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Int): Unit = output.writeFieldUnsignedInt(number, name, v)
  
  def defaultValue: Int = 0
  def deserializeRaw(input: RawInput): Int = input.readRawUnsignedInt()
  def deserializeNested(input: NestedInput): Int = input.readNestedUnsignedInt()
}

final class SignedIntPrimitive extends Primitive[Int] {
  def serializeRaw(output: RawOutput, v: Int): Unit = output.writeRawSignedInt(v)
  def serializeNested(output: NestedOutput, v: Int): Unit = output.writeNestedSignedInt(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Int): Unit = output.writeFieldSignedInt(number, name, v)
  
  def defaultValue: Int = 0
  def deserializeRaw(input: RawInput): Int = input.readRawSignedInt()
  def deserializeNested(input: NestedInput): Int = input.readNestedSignedInt()
}

final class FixedIntPrimitive extends Primitive[Int] {
  def serializeRaw(output: RawOutput, v: Int): Unit = output.writeRawFixedInt(v)
  def serializeNested(output: NestedOutput, v: Int): Unit = output.writeNestedFixedInt(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Int): Unit = output.writeFieldFixedInt(number, name, v)
  
  def defaultValue: Int = 0
  def deserializeRaw(input: RawInput): Int = input.readRawFixedInt()
  def deserializeNested(input: NestedInput): Int = input.readNestedFixedInt()
}

final class LongPrimitive extends Primitive[Long] {
  def serializeRaw(output: RawOutput, v: Long): Unit = output.writeRawLong(v)
  def serializeNested(output: NestedOutput, v: Long): Unit = output.writeNestedLong(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Long): Unit = output.writeFieldLong(number, name, v)
  
  def defaultValue: Long = 0L
  def deserializeRaw(input: RawInput): Long = input.readRawLong()
  def deserializeNested(input: NestedInput): Long = input.readNestedLong()
}

final class UnsignedLongPrimitive extends Primitive[Long] {
  def serializeRaw(output: RawOutput, v: Long): Unit = output.writeRawUnsignedLong(v)
  def serializeNested(output: NestedOutput, v: Long): Unit = output.writeNestedUnsignedLong(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Long): Unit = output.writeFieldUnsignedLong(number, name, v)
  
  def defaultValue: Long = 0L
  def deserializeRaw(input: RawInput): Long = input.readRawUnsignedLong()
  def deserializeNested(input: NestedInput): Long = input.readNestedUnsignedLong()
}

final class SignedLongPrimitive extends Primitive[Long] {
  def serializeRaw(output: RawOutput, v: Long): Unit = output.writeRawSignedLong(v)
  def serializeNested(output: NestedOutput, v: Long): Unit = output.writeNestedSignedLong(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Long): Unit = output.writeFieldSignedLong(number, name, v)
  
  def defaultValue: Long = 0L
  def deserializeRaw(input: RawInput): Long = input.readRawSignedLong()
  def deserializeNested(input: NestedInput): Long = input.readNestedSignedLong()
}

final class FixedLongPrimitive extends Primitive[Long] {
  def serializeRaw(output: RawOutput, v: Long): Unit = output.writeRawFixedLong(v)
  def serializeNested(output: NestedOutput, v: Long): Unit = output.writeNestedFixedLong(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: Long): Unit = output.writeFieldFixedLong(number, name, v)
  
  def defaultValue: Long = 0L
  def deserializeRaw(input: RawInput): Long = input.readRawFixedLong()
  def deserializeNested(input: NestedInput): Long = input.readNestedFixedLong()
}

final class JavaBigIntegerPrimitive extends Primitive[JavaBigInteger] {
  def serializeRaw(output: RawOutput, v: JavaBigInteger): Unit = output.writeRawBigInteger(v)
  def serializeNested(output: NestedOutput, v: JavaBigInteger): Unit = output.writeNestedBigInteger(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: JavaBigInteger): Unit = output.writeFieldBigInteger(number, name, v)

  def defaultValue: JavaBigInteger = null
  def deserializeRaw(input: RawInput): JavaBigInteger = input.readRawBigInteger()
  def deserializeNested(input: NestedInput): JavaBigInteger = input.readNestedBigInteger()
}

final class JavaBigDecimalPrimitive extends Primitive[JavaBigDecimal] {
  def serializeRaw(output: RawOutput, v: JavaBigDecimal): Unit = output.writeRawBigDecimal(v)
  def serializeNested(output: NestedOutput, v: JavaBigDecimal): Unit = output.writeNestedBigDecimal(v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: JavaBigDecimal): Unit = output.writeFieldBigDecimal(number, name, v)

  def defaultValue: JavaBigDecimal = null
  def deserializeRaw(input: RawInput): JavaBigDecimal = input.readRawBigDecimal()
  def deserializeNested(input: NestedInput): JavaBigDecimal = input.readNestedBigDecimal()
}

final class ScalaBigIntPrimitive extends Primitive[BigInt] {
  private def toJavaBigInteger(v: BigInt): JavaBigInteger = if (v != null) v.bigInteger else null
  private def toScalaBigInteger(v: JavaBigInteger): BigInt = if (v != null) BigInt(v) else null

  def serializeRaw(output: RawOutput, v: BigInt): Unit = output.writeRawBigInteger(toJavaBigInteger(v))
  def serializeNested(output: NestedOutput, v: BigInt): Unit = output.writeNestedBigInteger(toJavaBigInteger(v))
  def serializeField(output: FieldOutput, number: Int, name: String, v: BigInt): Unit = output.writeFieldBigInteger(number, name, toJavaBigInteger(v))

  def defaultValue: BigInt = null
  def deserializeRaw(input: RawInput): BigInt = toScalaBigInteger(input.readRawBigInteger())
  def deserializeNested(input: NestedInput): BigInt = toScalaBigInteger(input.readNestedBigInteger())
}

final class ScalaBigDecimalPrimitive extends Primitive[BigDecimal] {
  private def toJavaBigDecimal(v: BigDecimal): JavaBigDecimal = if (v != null) v.bigDecimal else null
  private def toScalaBigDecimal(v: JavaBigDecimal): BigDecimal = if (v != null) BigDecimal(v) else null

  def serializeRaw(output: RawOutput, v: BigDecimal): Unit = output.writeRawBigDecimal(toJavaBigDecimal(v))
  def serializeNested(output: NestedOutput, v: BigDecimal): Unit = output.writeNestedBigDecimal(toJavaBigDecimal(v))
  def serializeField(output: FieldOutput, number: Int, name: String, v: BigDecimal): Unit = output.writeFieldBigDecimal(number, name, toJavaBigDecimal(v))

  def defaultValue: BigDecimal = null
  def deserializeRaw(input: RawInput): BigDecimal = toScalaBigDecimal(input.readRawBigDecimal())
  def deserializeNested(input: NestedInput): BigDecimal = toScalaBigDecimal(input.readNestedBigDecimal())
}
