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

import org.scalatest.{FunSuite, Matchers}
import fm.serializer.{Deserializer, Primitive, Serializer}

final class TestProtobufRaw extends FunSuite with Matchers  {
  private def check[T](value: T, expectedBytes: Array[Byte])(implicit ser: Serializer[T], deser: Deserializer[T]): Unit = {
    val bytes = Protobuf.toBytes(value)
    bytes should equal (expectedBytes)
    Protobuf.fromBytes(bytes) should equal (value)
  }
  
  private def checkReverse[T](value: T, expectedBytes: Array[Byte])(implicit ser: Serializer[T], deser: Deserializer[T]): Unit = {
    check(value, expectedBytes.reverse)
  }
  
  test("writeRawBool") {
    check(false, Array(0))
    check(true, Array(1))
  }
  
  test("writeJavaInteger") {
    check(Integer.valueOf(123456), Array(-64, -60, 7))
    
    // TODO: Figure out how to make this work
    //check(null.asInstanceOf[Integer], Array())
  }
  
  test("writeRawInt") {
    check(Int.MinValue, Array(-128, -128, -128, -128, -8, -1, -1, -1, -1, 1))
    check(-128, Array(-128, -1, -1, -1, -1, -1, -1, -1, -1, 1))
    check(-127, Array(-127, -1, -1, -1, -1, -1, -1, -1, -1, 1))
    check(-1, Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, 1))
    check(0, Array(0))
    check(1, Array(1))
    check(127, Array(127))
    check(128, Array(-128, 1))
    check(300, Array(-84, 2))
    check(123456, Array(-64, -60, 7))
    check(1234567890, Array(-46, -123, -40, -52, 4))
    check(Int.MaxValue, Array(-1, -1, -1, -1, 7))
  }
  
  test("writeRawLong") {
    check(Long.MinValue, Array(-128, -128, -128, -128, -128, -128, -128, -128, -128, 1))
    check(-128L, Array(-128, -1, -1, -1, -1, -1, -1, -1, -1, 1))
    check(-127L, Array(-127, -1, -1, -1, -1, -1, -1, -1, -1, 1))
    check(-1L, Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, 1))
    check(0L, Array(0))
    check(1L, Array(1))
    check(127L, Array(127))
    check(128L, Array(-128, 1))
    check(300L, Array(-84, 2))
    check(123456L, Array(-64, -60, 7))
    check(1234567890L, Array(-46, -123, -40, -52, 4))
    check(Long.MaxValue, Array(-1, -1, -1, -1, -1, -1, -1, -1, 127))
  }
  
  test("writeRawFixedInt - LITTLE ENDIAN") {
    implicit val serializer = Primitive.fixedInt
    
    checkReverse(Int.MinValue, Array(-128, 0, 0, 0))
    checkReverse(-1610612736, Array(-96, 0, 0, 0))
    checkReverse(-255, Array(-1, -1, -1, 1))
    checkReverse(-128, Array(-1, -1, -1, -128))
    checkReverse(-127, Array(-1, -1, -1, -127))
    checkReverse(-1, Array(-1, -1, -1, -1))
    checkReverse(0, Array(0, 0, 0, 0))
    checkReverse(1, Array(0, 0, 0, 1))
    checkReverse(127, Array(0, 0, 0, 127))
    checkReverse(128, Array(0, 0, 0, -128))
    checkReverse(255, Array(0, 0, 0, -1))
    checkReverse(65280, Array(0, 0, -1, 0))
    checkReverse(65535, Array(0, 0, -1, -1))
    checkReverse(10485760, Array(0, -96, 0, 0))
    checkReverse(Int.MaxValue, Array(127, -1, -1, -1))
  }
  
  test("writeRawFixedLong - LITTLE ENDIAN") {
    implicit val serializer = Primitive.fixedLong
    
    checkReverse(Long.MinValue, Array(-128, 0, 0, 0, 0, 0, 0, 0))
    checkReverse(-255L, Array(-1, -1, -1, -1, -1, -1, -1, 1))
    checkReverse(-128L, Array(-1, -1, -1, -1, -1, -1, -1, -128))
    checkReverse(-127L, Array(-1, -1, -1, -1, -1, -1, -1, -127))
    checkReverse(-1L, Array(-1, -1, -1, -1, -1, -1, -1, -1))
    checkReverse(0L, Array(0, 0, 0, 0, 0, 0, 0, 0))
    checkReverse(1L, Array(0, 0, 0, 0, 0, 0, 0, 1))
    checkReverse(127L, Array(0, 0, 0, 0, 0, 0, 0, 127))
    checkReverse(128L, Array(0, 0, 0, 0, 0, 0, 0, -128))
    checkReverse(255L, Array(0, 0, 0, 0, 0, 0, 0, -1))
    checkReverse(65280L, Array(0, 0, 0, 0, 0, 0, -1, 0))
    checkReverse(65535L, Array(0, 0, 0, 0, 0, 0, -1, -1))
    checkReverse(10485760L, Array(0, 0, 0, 0, 0, -96, 0, 0))
    checkReverse(2684354560L, Array(0, 0, 0, 0, -96, 0, 0, 0))
    checkReverse(Long.MaxValue, Array(127, -1, -1, -1, -1, -1, -1, -1))
  }
  
//  test("writeRawFixedInt - BIG ENDIAN") {
//    implicit val serializer = Primitive.fixedInt
//    
//    check(Int.MinValue, Array(-128, 0, 0, 0))
//    check(-1610612736, Array(-96, 0, 0, 0))
//    check(-255, Array(-1, -1, -1, 1))
//    check(-128, Array(-1, -1, -1, -128))
//    check(-127, Array(-1, -1, -1, -127))
//    check(-1, Array(-1, -1, -1, -1))
//    check(0, Array(0, 0, 0, 0))
//    check(1, Array(0, 0, 0, 1))
//    check(127, Array(0, 0, 0, 127))
//    check(128, Array(0, 0, 0, -128))
//    check(255, Array(0, 0, 0, -1))
//    check(65280, Array(0, 0, -1, 0))
//    check(65535, Array(0, 0, -1, -1))
//    check(10485760, Array(0, -96, 0, 0))
//    check(Int.MaxValue, Array(127, -1, -1, -1))
//  }
//  
//  test("writeRawFixedLong - BIG ENDIAN") {
//    implicit val serializer = Primitive.fixedLong
//    
//    check(Long.MinValue, Array(-128, 0, 0, 0, 0, 0, 0, 0))
//    check(-255L, Array(-1, -1, -1, -1, -1, -1, -1, 1))
//    check(-128L, Array(-1, -1, -1, -1, -1, -1, -1, -128))
//    check(-127L, Array(-1, -1, -1, -1, -1, -1, -1, -127))
//    check(-1L, Array(-1, -1, -1, -1, -1, -1, -1, -1))
//    check(0L, Array(0, 0, 0, 0, 0, 0, 0, 0))
//    check(1L, Array(0, 0, 0, 0, 0, 0, 0, 1))
//    check(127L, Array(0, 0, 0, 0, 0, 0, 0, 127))
//    check(128L, Array(0, 0, 0, 0, 0, 0, 0, -128))
//    check(255L, Array(0, 0, 0, 0, 0, 0, 0, -1))
//    check(65280L, Array(0, 0, 0, 0, 0, 0, -1, 0))
//    check(65535L, Array(0, 0, 0, 0, 0, 0, -1, -1))
//    check(10485760L, Array(0, 0, 0, 0, 0, -96, 0, 0))
//    check(2684354560L, Array(0, 0, 0, 0, -96, 0, 0, 0))
//    check(Long.MaxValue, Array(127, -1, -1, -1, -1, -1, -1, -1))
//  }
  
  test("writeRawByteArray") {
    check("".getBytes("UTF-8"), Array())
    check("Hello World!".getBytes("UTF-8"), "Hello World!".getBytes("UTF-8"))
  }
  
  test("writeRawString") {
    check("", Array())
    check("Hello World!", "Hello World!".getBytes("UTF-8"))
  }
  
  test("writeRawCollection Int") {
    check(List(1,2,3,4,5,6), Array(1, 2, 3, 4, 5, 6))
    check(List(-1,-2,-3,-4,-5,-6), Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, 1, -2, -1, -1, -1, -1, -1, -1, -1, -1, 1, -3, -1, -1, -1, -1, -1, -1, -1, -1, 1, -4, -1, -1, -1, -1, -1, -1, -1, -1, 1, -5, -1, -1, -1, -1, -1, -1, -1, -1, 1, -6, -1, -1, -1, -1, -1, -1, -1, -1, 1))
  }
  
  test("writeRawCollection Signed Int") {
    implicit val serializer = Primitive.signedInt
    
    check(List(1,2,3,4,5,6), Array(2, 4, 6, 8, 10, 12))
    check(List(-1,-2,-3,-4,-5,-6), Array(1, 3, 5, 7, 9, 11))
  }
}