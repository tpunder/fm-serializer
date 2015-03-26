/*
 * Copyright 2015 Frugal Mechanic (http://frugalmechanic.com)
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

final class SerializerProxy[T] extends Serializer[T] {
  @volatile var self: Serializer[T] = null
  
  def serializeRaw(output: RawOutput, v: T): Unit = self.serializeRaw(output, v)
  def serializeNested(output: NestedOutput, v: T): Unit = self.serializeNested(output, v)
  def serializeField(output: FieldOutput, number: Int, name: String, v: T): Unit = self.serializeField(output, number, name, v)  
}