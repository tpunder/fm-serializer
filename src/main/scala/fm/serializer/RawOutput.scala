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
 * RAW Output
 * 
 * See the documentation for Output
 */
trait RawOutput {
  // Basic Types
  def writeRawBool(value: Boolean): Unit
  def writeRawFloat(value: Float): Unit
  def writeRawDouble(value: Double): Unit
  def writeRawString(value: String): Unit
  def writeRawByteArray(value: Array[Byte]): Unit
  
  // Ints
  def writeRawInt(value: Int): Unit
  def writeRawUnsignedInt(value: Int): Unit
  def writeRawSignedInt(value: Int): Unit
  def writeRawFixedInt(value: Int): Unit
  
  // Longs
  def writeRawLong(value: Long): Unit
  def writeRawUnsignedLong(value: Long): Unit
  def writeRawSignedLong(value: Long): Unit
  def writeRawFixedLong(value: Long): Unit
  
  /**
   * For writing objects.  Note: that the obj is passed in for null handling
   * by the implementation.  If the object is not null then the function f
   * will be called so the caller can write out the fields
   */
  def writeRawObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit
  
  /**
   * Write out a RAW collection.  This method will wrap 
   * the collection in whatever leading/trailing "stuff"
   * is needed (e.g. length prefixing, leading/trailing 
   * chars, etc...).  The method that you pass in should
   * use the Output instance to make repeated calls to
   * a single write
   */
  def writeRawCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit
}