/*
 * Copyright 2017 Frugal Mechanic (http://frugalmechanic.com)
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

import org.scalatest.{FunSuite, Matchers}

final class TestOptionDeserializer extends FunSuite with Matchers {
  test("BooleanOptionDeserializer") {
    BooleanOptionDeserializer.makeValue(false) should be theSameInstanceAs (BooleanOptionDeserializer.SomeFalse)
    BooleanOptionDeserializer.makeValue(true) should be theSameInstanceAs (BooleanOptionDeserializer.SomeTrue)
  }

  test("IntOptionDeserializer") {
    val low: Int = IntOptionDeserializer.low
    val high: Int = IntOptionDeserializer.high

    Vector(low - 1, high + 1).foreach { i: Int =>
      IntOptionDeserializer.makeValue(i) should not be theSameInstanceAs (IntOptionDeserializer.makeValue(i))
      IntOptionDeserializer.makeValue(i) should equal (Some(i))
    }

    (low to high).foreach { i: Int =>
      IntOptionDeserializer.makeValue(i) should be theSameInstanceAs (IntOptionDeserializer.makeValue(i))
      IntOptionDeserializer.makeValue(i) should equal (Some(i))
    }
  }
}
