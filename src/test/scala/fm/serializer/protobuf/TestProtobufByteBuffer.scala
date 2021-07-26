/*
 * Copyright 2021 Tim Underwood
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
import java.nio.ByteBuffer

/**
 * This tests ProtobufByteArrayInput
 */
final class TestProtobufByteBuffer extends fm.serializer.TestSerializer[Array[Byte]]  {
  def serialize[T](v: T)(implicit ser: Serializer[T]): Array[Byte] = {
    Protobuf.toBytes[T](v)
  }

  def deserialize[T](bytes: Array[Byte])(implicit deser: Deserializer[T]): T = {
    Protobuf.fromByteBuffer[T](ByteBuffer.wrap(bytes))
  }

  def makeInput(bytes: Array[Byte]): ProtobufByteBufferInput = {
    new ProtobufByteBufferInput(ByteBuffer.wrap(bytes), ProtobufOptions.default)
  }
}
