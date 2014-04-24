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

object ObjectSerializer {
  def apply[T](): ObjectSerializer[T] = macro Macros.makeObjectSerializer[T]
  def apply[T](field: Field, fields: Field*): ObjectSerializer[T] = macro Macros.makeObjectSerializerFromFields[T]
  
  def forInterface[IFACE,CONCRETE](): ObjectSerializer[IFACE] = macro Macros.makeObjectSerializerForInterface[IFACE,CONCRETE]
}

/**
 * A combined Object Serializer/Deserializer that Serializes/Deserializes Objects from/to the same type
 */
trait ObjectSerializer[T] extends Serializer[T] {
//  final def serializeRaw(output: RawOutput, v: T): Unit = output.writeRawObject(v){ write(_, v) }
//  final def serializeNested(output: NestedOutput, v: T): Unit = output.writeNestedObject(v){ write(_, v) }
//  final def serializeField(output: FieldOutput, number: Int, name: String, v: T): Unit = output.writeFieldObject(number, name, v){ write(_, v) }
  
//  def write(output: FieldOutput, v: T): Unit
  
//  import fm.serializer.protobuf.ProtobufOutput
//  
//  def serializeRaw(output: ProtobufOutput, v: T): Unit
//  def serializeNested(output: ProtobufOutput, v: T): Unit
//  def serializeField(output: ProtobufOutput, number: Int, name: String, v: T): Unit
}