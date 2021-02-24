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
  def readFieldNumber(nameToNumMap: FieldNameToNumberLookup): Int
  
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

  /**
   * This is for reporting that fields for an object were not read and whether or not they had a user-defined default value.
   * @param number The field number
   * @param name The field name.  Note: This can be null.
   * @param hasUserDefinedDefaultValue Whether or not there was a user defined default value (e.g. val foo: Int = 123)
   * @param deserializer The deserializer for the field.  Note: This can be null.
   * @tparam T
   */
  def reportUnsetField[T](number: Int, name: String, hasUserDefinedDefaultValue: Boolean, deserializer: Deserializer[T]): Unit = {
    // This is really for the ValidatingInput so by default we do nothing
  }

  /**
   * The last field name that was read (if any)
   */
  def lastFieldName(): String

  /**
   * The last field number that was read (if any)
   */
  def lastFieldNumber(): Int
}