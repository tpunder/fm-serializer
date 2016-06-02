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
package fm.serializer.json

import org.scalatest.{FunSuite, Matchers}
import fm.serializer.{Deserializer, Field, Primitive, Serializer}

final class TestDefaultJSON extends TestJSON {
  def serialize[T](v: T)(implicit ser: Serializer[T]): String = JSON.toJSON[T](v)
  
  test("String Map - Treated as object") {
    val map: Map[String,Int] = Map("foo" -> 123, "bar" -> 321)
    val json: String = serialize(map)
    
    json should equal ("""{"foo":123,"bar":321}""")
    
    deserialize[Map[String,Int]](json) should equal (map)
  }
  
  test("Int Map - Treated as array of tuples") {
    val map: Map[Int,String] = Map(123 -> "foo", 312 -> "bar")
    val json: String = serialize(map)
    
    json should equal ("""[{"_1":123,"_2":"foo"},{"_1":312,"_2":"bar"}]""")
    
    deserialize[Map[Int,String]](json) should equal (map)
  }
  
  case class Unquoted(name: String, int: Int, long: Long)
  
  test("Unquoted Field Names") {
    deserialize[Unquoted]("""{name:null,int:123,long:123123123123123}""") should equal(Unquoted(null, 123, 123123123123123L))
    deserialize[Unquoted]("""{name:nullnot,int:123,long:123123123123123}""") should equal(Unquoted("nullnot", 123, 123123123123123L))
    deserialize[Unquoted]("""{name:foo,int:123,long:123123123123123}""") should equal(Unquoted("foo", 123, 123123123123123L))
    deserialize[Unquoted]("""{name:foo,int:"123",long:"123123123123123"}""") should equal(Unquoted("foo", 123, 123123123123123L))
  }
  
  case class AlternateName(@Field("type") tpe: String, @Field foo: Int)
  
  test("Alternate Field Name") {
    val instance: AlternateName = AlternateName("the_type_field", 123)
    val json: String = """{"type":"the_type_field","foo":123}"""
    
    serialize(instance) should equal (json)
    deserialize[AlternateName](json) should equal (instance)
  }
}

final class TestMinimalJSON extends TestJSON {
  def serialize[T](v: T)(implicit ser: Serializer[T]): String = JSON.toMinimalJSON[T](v)
  override def ignoreNullRetainTest: Boolean = true
}

final class TestPrettyJSON extends TestJSON {
  def serialize[T](v: T)(implicit ser: Serializer[T]): String = JSON.toPrettyJSON[T](v)
}

abstract class TestJSON extends fm.serializer.TestSerializer[String]  {
  def serialize[T](v: T)(implicit ser: Serializer[T]): String
  final def deserialize[T](json: String)(implicit deser: Deserializer[T]): T = JSON.fromJSON[T](json)
}