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
 * FIELD Input
 * 
 * This the extra methods for reading FIELD input along with the NestedInput methods
 */
trait FieldInput extends NestedInput {
  def allowStringMap: Boolean
  
  /**
   * This is for reading fields of an object.
   * 
   * Return the field number for the next readable field.
   * Returns 0 if we've reached the end of the object/message
   */
  def readFieldNumber(nameToNumMap: Map[String, Int]): Int
  
  /**
   * If dynamic string maps are supported then this should be implemented
   * otherwise this can just throw an exception.
   * 
   * null should be returns on the end of an object/message
   */
  def readFieldName(): String
  
  /**
   * Skip an unknown field value.
   * 
   * If after calling readFieldNumber(...) we don't know how
   * to handle the resulting field number then this method
   * can be called to skip the value of the field after which
   * we can call readFieldNumber(...) again.
   */
  def skipUnknownField(): Unit
}