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

object ObjectDeserializer extends ObjectDeserializerObj {
  // Moved to ObjectDeserializerObj to support Scala 2 and 3
  //def apply[T](): ObjectDeserializer[T] = macro Macros.makeObjectDeserializer[T]
  //def apply[T](field: Field, fields: Field*): ObjectDeserializer[T] = macro Macros.makeObjectDeserializerFromFields[T]
}

/**
 * A combined Object Serializer/Deserializer that Serializes/Deserializes Objects from/to the same type
 */
trait ObjectDeserializer[T] extends Deserializer[T] { 
//  final def defaultValue: T = null.asInstanceOf[T]
//  final def deserializeRaw(input: RawInput): T = input.readRawObject{ read }
//  final def deserializeNested(input: NestedInput): T = input.readNestedObject{ read }

//  def read(input: FieldInput): T
}