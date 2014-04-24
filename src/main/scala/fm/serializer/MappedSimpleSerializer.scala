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

trait Mapper[@specialized A, B <: AnyRef] {
  def defaultValue: B
  def serialize(obj: B): A
  def deserialize(value: A): B
}

final class MappedSimpleSerializer[@specialized A, B <: AnyRef](orig: SimpleSerializer[A], mapper: Mapper[A,B]) extends SimpleSerializer[B] {
  final def serializeRaw(output: RawOutput, v: B): Unit = if (null != v) orig.serializeRaw(output, mapper.serialize(v))
  final def serializeNested(output: NestedOutput, v: B): Unit = if (null != v) orig.serializeNested(output, mapper.serialize(v))
  final def serializeField(output: FieldOutput, number: Int, name: String, v: B): Unit = if (null != v) orig.serializeField(output, number, name, mapper.serialize(v))
  
  final def deserializeRaw(input: RawInput): B = mapper.deserialize(orig.deserializeRaw(input))
  final def deserializeNested(input: NestedInput): B = mapper.deserialize(orig.deserializeNested(input))
  
  final def defaultValue: B = mapper.defaultValue
}