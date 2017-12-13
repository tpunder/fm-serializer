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

import fm.common.Implicits._

/**
 * For deserializing Option types.  Note: this does NOT allow Some(null)
 */
final case class OptionDeserializer[A](implicit deser: Deserializer[A]) extends Deserializer[Option[A]] {
  def defaultValue: Option[A] = None
  
  final def deserializeRaw(input: RawInput): Option[A] = Option(deser.deserializeRaw(input))
  
  final def deserializeNested(input: NestedInput): Option[A] = {
    if (input.nextValueIsNull) None else Option(deser.deserializeNested(input))
  }
}

/**
 * A Specialzed Option[Boolean] deserializer that uses static true/false values
 * to prevent allocating memory.
 */
object BooleanOptionDeserializer extends Deserializer[Option[Boolean]] {
  private[serializer] val SomeFalse: Option[Boolean] = Some(false)
  private[serializer] val SomeTrue: Option[Boolean] = Some(true)
  private val deser: Deserializer[Boolean] = PrimitiveImplicits.boolean

  def defaultValue: Option[Boolean] = None

  final def deserializeRaw(input: RawInput): Option[Boolean] = makeValue(deser.deserializeRaw(input))

  final def deserializeNested(input: NestedInput): Option[Boolean] = {
    if (input.nextValueIsNull) None else makeValue(deser.deserializeNested(input))
  }

  private[serializer] def makeValue(b: Boolean): Option[Boolean] = if (b) SomeTrue else SomeFalse
}

object IntOptionDeserializer extends Deserializer[Option[Int]] {
  private[serializer] val low: Int = -127
  private[serializer] val high: Int = System.getProperty("java.lang.Integer.IntegerCache.high").toIntOption.getOrElse(128)

  /** The index in our cache of the value we are looking for or -1 if we are out of range */
  private def idxOf(i: Int): Int = if (i >= low && i <= high) i - low else -1

  private val cache: Array[Option[Int]] = {
    val arr: Array[Option[Int]] = new Array(high - low + 1)
    var i: Int = low

    while (i <= high) {
      val idx: Int = idxOf(i)
      arr(idx) = Some(i)
      i += 1
    }

    arr
  }

  private val deser: Deserializer[Int] = PrimitiveImplicits.int

  def defaultValue: Option[Int] = None

  final def deserializeRaw(input: RawInput): Option[Int] = makeValue(deser.deserializeRaw(input))

  final def deserializeNested(input: NestedInput): Option[Int] = {
    if (input.nextValueIsNull) None else makeValue(deser.deserializeNested(input))
  }

  private[serializer] def makeValue(i: Int): Option[Int] = {
    val idx: Int = idxOf(i)
    if (-1 === idx) Some(i) else cache(idx)
  }
}