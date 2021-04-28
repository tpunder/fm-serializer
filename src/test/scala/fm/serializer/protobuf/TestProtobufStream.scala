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

import java.io.{BufferedInputStream, ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import fm.serializer.{Deserializer, Serializer}

final class TestProtobufStream extends TestProtobufStreamBase(0)
final class TestProtobufStream1 extends TestProtobufStreamBase(1)
final class TestProtobufStream2 extends TestProtobufStreamBase(2)
final class TestProtobufStream3 extends TestProtobufStreamBase(3)
final class TestProtobufStream4 extends TestProtobufStreamBase(4)
final class TestProtobufStream5 extends TestProtobufStreamBase(5)
final class TestProtobufStream6 extends TestProtobufStreamBase(6)
final class TestProtobufStream7 extends TestProtobufStreamBase(7)
final class TestProtobufStream8 extends TestProtobufStreamBase(8)
final class TestProtobufStream9 extends TestProtobufStreamBase(9)
//final class TestProtobufStream10 extends TestProtobufStreamBase(10)
//final class TestProtobufStream11 extends TestProtobufStreamBase(11)
//final class TestProtobufStream12 extends TestProtobufStreamBase(12)
//final class TestProtobufStream13 extends TestProtobufStreamBase(13)
//final class TestProtobufStream14 extends TestProtobufStreamBase(14)
//final class TestProtobufStream15 extends TestProtobufStreamBase(15)
//final class TestProtobufStream16 extends TestProtobufStreamBase(16)
//final class TestProtobufStream17 extends TestProtobufStreamBase(17)
//final class TestProtobufStream18 extends TestProtobufStreamBase(18)
//final class TestProtobufStream19 extends TestProtobufStreamBase(19)
//final class TestProtobufStream20 extends TestProtobufStreamBase(20)
//final class TestProtobufStream16 extends TestProtobufStreamBase(16)
//final class TestProtobufStream32 extends TestProtobufStreamBase(32)
//final class TestProtobufStream64 extends TestProtobufStreamBase(64)
//final class TestProtobufStream128 extends TestProtobufStreamBase(128)
//final class TestProtobufStream1024 extends TestProtobufStreamBase(1024)
//final class TestProtobufStream4096 extends TestProtobufStreamBase(4096)
final class TestProtobufStream8192 extends TestProtobufStreamBase(8192) // This is the default BufferedInputStream size

/**
 * This tests ProtobufInputStreamInput
 */
sealed abstract class TestProtobufStreamBase(bufferSize: Int) extends fm.serializer.TestSerializer[Array[Byte]]  {
  def serialize[T](v: T)(implicit ser: Serializer[T]): Array[Byte] = {
    val os = new ByteArrayOutputStream()
    Protobuf.toOutputStream[T](os, v)
    os.toByteArray()
  }
  
  def deserialize[T](bytes: Array[Byte])(implicit deser: Deserializer[T]): T = {
    val is = new ByteArrayInputStream(bytes)
    Protobuf.fromInputStream[T](is)
  }

  def makeInput(bytes: Array[Byte]): ProtobufInputStreamInput = {
    val is: InputStream = if (bufferSize > 0) new BufferedInputStream(new ByteArrayInputStream(bytes), bufferSize) else new ByteArrayInputStream(bytes)
    new ProtobufInputStreamInput(is, ProtobufOptions.default)
  }
}
