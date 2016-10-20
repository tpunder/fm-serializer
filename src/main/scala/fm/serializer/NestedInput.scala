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
 * NESTED Input
 * 
 * See documentation for Input/Output traits
 */
trait NestedInput {
  def allowStringMap: Boolean
  
  // Basic Types
  def readNestedBool(): Boolean
  def readNestedFloat(): Float
  def readNestedDouble(): Double
  def readNestedString(): String
  
  // Bytes
  def readNestedByteArray(): Array[Byte]
  
  // Ints  
  def readNestedInt(): Int
  def readNestedUnsignedInt(): Int
  def readNestedSignedInt(): Int
  def readNestedFixedInt(): Int
  
  // Longs
  def readNestedLong(): Long
  def readNestedUnsignedLong(): Long
  def readNestedSignedLong(): Long
  def readNestedFixedLong(): Long
    
  // Objects
  def readNestedObject[T](f: FieldInput => T): T
  
  // Collections
  def readNestedCollection[T](f: CollectionInput => T): T
  
  /**
   * Returns true if the next value is known to be null otherwise false if the value is not null or is unknown.
   * This means that even if the next value ends up being null this can return false.
   *
   * Note: If the next value is null then this method should consume that input
   */
  def nextValueIsNull: Boolean
}