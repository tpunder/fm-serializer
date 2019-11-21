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
package fm.serializer.protobuf

import fm.serializer.{Deserializer, Serializer}

/**
 * This tests ProtobufByteArrayInput
 */
final class TestProtobufArray extends fm.serializer.TestSerializer[Array[Byte]]  {
  def serialize[T](v: T)(implicit ser: Serializer[T]): Array[Byte] = Protobuf.toBytes[T](v)
  def deserialize[T](bytes: Array[Byte])(implicit deser: Deserializer[T]): T = Protobuf.fromBytes[T](bytes)
  def makeInput(bytes: Array[Byte]): ProtobufByteArrayInput = new ProtobufByteArrayInput(bytes, ProtobufOptions.default)
}