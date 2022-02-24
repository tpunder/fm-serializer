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

object SimpleObjectSerializer extends SimpleObjectSerializerObj {
  // Macro is in SimpleObjectSerializerObj to support both Scala 2 and 3
  //def make[T](): SimpleObjectSerializer[T] = macro Macros.makeSimpleObjectSerializer[T]
}

final case class SimpleObjectSerializer[T]()(implicit ser: ObjectSerializer[T], deser: ObjectDeserializer[T]) extends ObjectSerializer[T] with ObjectDeserializer[T] with SimpleSerializer[T] {
  final def defaultValue: T = deser.defaultValue
  final def deserializeRaw(input: RawInput): T = deser.deserializeRaw(input)
  final def deserializeNested(input: NestedInput): T = deser.deserializeNested(input)
  
  final def serializeRaw(output: RawOutput, v: T): Unit = ser.serializeRaw(output, v)
  final def serializeNested(output: NestedOutput, v: T): Unit = ser.serializeNested(output, v)
  final def serializeField(output: FieldOutput, number: Int, name: String, v: T): Unit = ser.serializeField(output, number, name, v)
}