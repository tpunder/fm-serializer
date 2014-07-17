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
import fm.serializer.{Deserializer, Primitive, Serializer}

final class TestJSON extends fm.serializer.TestSerializer[String]  {
  def serialize[T](v: T)(implicit ser: Serializer[T]): String = JSON.toJSON[T](v)
  def deserialize[T](json: String)(implicit deser: Deserializer[T]): T = JSON.fromJSON[T](json)
  
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
}