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

import fm.common.ASCIIUtil
import fm.common.Implicits._
import org.scalatest.{FunSuite, Matchers}

final class TestOptionDeserializer extends FunSuite with Matchers {
  // These are the JVM defaults but will change if -XX:AutoBoxCacheMax is set
  private val low: Int = -128
  private val high: Int = 127

  test("BooleanOptionDeserializer") {
    BooleanOptionDeserializer.makeValue(false) should be theSameInstanceAs (Some.cached(false))
    BooleanOptionDeserializer.makeValue(true) should be theSameInstanceAs (Some.cached(true))
  }

  test("CharOptionDeserializer") {
    // These are the JVM defaults but will change if -XX:AutoBoxCacheMax is set
    Vector(Char.MaxValue, (ASCIIUtil.MaxASCIIChar+1).toChar).foreach { i: Char =>
      CharOptionDeserializer.makeValue(i) should not be theSameInstanceAs (CharOptionDeserializer.makeValue(i))
      CharOptionDeserializer.makeValue(i) should equal (Some(i))
    }

    (Char.MinValue to ASCIIUtil.MaxASCIIChar).foreach { i: Char =>
      CharOptionDeserializer.makeValue(i) should be theSameInstanceAs (CharOptionDeserializer.makeValue(i))
      CharOptionDeserializer.makeValue(i) should equal (Some(i))
    }
  }

  test("IntOptionDeserializer") {
    // These are the JVM defaults but will change if -XX:AutoBoxCacheMax is set
    Vector(low - 1, high + 1).foreach { i: Int =>
      IntOptionDeserializer.makeValue(i) should not be theSameInstanceAs (IntOptionDeserializer.makeValue(i))
      IntOptionDeserializer.makeValue(i) should equal (Some(i))
    }

    (low to high).foreach { i: Int =>
      IntOptionDeserializer.makeValue(i) should be theSameInstanceAs (IntOptionDeserializer.makeValue(i))
      IntOptionDeserializer.makeValue(i) should equal (Some(i))
    }
  }

  test("LongOptionDeserializer") {
    Vector(low - 1, high + 1).foreach { i: Int =>
      LongOptionDeserializer.makeValue(i) should not be theSameInstanceAs (LongOptionDeserializer.makeValue(i))
      LongOptionDeserializer.makeValue(i) should equal (Some(i))
    }

    (low to high).foreach { i: Int =>
      LongOptionDeserializer.makeValue(i) should be theSameInstanceAs (LongOptionDeserializer.makeValue(i))
      LongOptionDeserializer.makeValue(i) should equal (Some(i))
    }
  }
}
