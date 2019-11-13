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

import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}

/**
 * NESTED Output
 * 
 * See the documentation for Output
 */
trait NestedOutput {
  def allowStringMap: Boolean
  
  // Basic Types
  def writeNestedBool(value: Boolean): Unit
  def writeNestedFloat(value: Float): Unit
  def writeNestedDouble(value: Double): Unit
  def writeNestedString(value: String): Unit
  def writeNestedBigInteger(value: JavaBigInteger): Unit
  def writeNestedBigDecimal(value: JavaBigDecimal): Unit
  
  // Bytes
  def writeNestedByteArray(value: Array[Byte]): Unit
  
  // Ints  
  def writeNestedInt(value: Int): Unit
  def writeNestedUnsignedInt(value: Int): Unit
  def writeNestedSignedInt(value: Int): Unit
  def writeNestedFixedInt(value: Int): Unit
  
  // Longs
  def writeNestedLong(value: Long): Unit
  def writeNestedUnsignedLong(value: Long): Unit
  def writeNestedSignedLong(value: Long): Unit
  def writeNestedFixedLong(value: Long): Unit
  
  def writeNestedObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit
  def writeNestedCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit
}