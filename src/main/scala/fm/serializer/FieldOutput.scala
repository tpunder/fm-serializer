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

/**
 * FIELD Output
 * 
 * See the documentation for Output
 */
trait FieldOutput {
  def allowStringMap: Boolean
  
  // Basic Types
  def writeFieldBool(number: Int, name: String, value: Boolean): Unit
  def writeFieldFloat(number: Int, name: String, value: Float): Unit
  def writeFieldDouble(number: Int, name: String, value: Double): Unit
  def writeFieldString(number: Int, name: String, value: String): Unit
  
  // Bytes
  def writeFieldByteArray(number: Int, name: String, value: Array[Byte]): Unit
  
  // Ints  
  def writeFieldInt(number: Int, name: String, value: Int): Unit
  def writeFieldUnsignedInt(number: Int, name: String, value: Int): Unit
  def writeFieldSignedInt(number: Int, name: String, value: Int): Unit
  def writeFieldFixedInt(number: Int, name: String, value: Int): Unit
  
  // Longs
  def writeFieldLong(number: Int, name: String, value: Long): Unit
  def writeFieldUnsignedLong(number: Int, name: String, value: Long): Unit
  def writeFieldSignedLong(number: Int, name: String, value: Long): Unit
  def writeFieldFixedLong(number: Int, name: String, value: Long): Unit
  
  // Objects
  def writeFieldObject[T](number: Int, name: String, obj: T)(f: (FieldOutput, T) => Unit): Unit
  
  // Collections
  def writeFieldCollection[T](number: Int, name: String, col: T)(f: (NestedOutput, T) => Unit): Unit
  
  // Null
  def writeFieldNull(number: Int, name: String): Unit
}