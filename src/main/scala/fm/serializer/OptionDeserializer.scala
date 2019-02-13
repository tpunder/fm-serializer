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
 * A Specialzed Option[Boolean] deserializer that uses the fm-common OptionCache via the Some.cached implicit
 */
object BooleanOptionDeserializer extends Deserializer[Option[Boolean]] {
  private val deser: Deserializer[Boolean] = PrimitiveImplicits.boolean
  def defaultValue: Option[Boolean] = None
  final def deserializeRaw(input: RawInput): Option[Boolean] = makeValue(deser.deserializeRaw(input))

  final def deserializeNested(input: NestedInput): Option[Boolean] = {
    if (input.nextValueIsNull) None else makeValue(deser.deserializeNested(input))
  }

  private[serializer] def makeValue(b: Boolean): Option[Boolean] = Some.cached(b)
}

/**
 * A Specialzed Option[Char] deserializer that uses the fm-common OptionCache via the Some.cached implicit
 */
object CharOptionDeserializer extends Deserializer[Option[Char]] {
  private val deser: Deserializer[Char] = PrimitiveImplicits.char
  def defaultValue: Option[Char] = None
  final def deserializeRaw(input: RawInput): Option[Char] = makeValue(deser.deserializeRaw(input))

  final def deserializeNested(input: NestedInput): Option[Char] = {
    if (input.nextValueIsNull) None else makeValue(deser.deserializeNested(input))
  }

  private[serializer] def makeValue(i: Char): Option[Char] = Some.cached(i)
}

/**
 * A Specialzed Option[Int] deserializer that uses the fm-common OptionCache via the Some.cached implicit
 */
object IntOptionDeserializer extends Deserializer[Option[Int]] {
  private val deser: Deserializer[Int] = PrimitiveImplicits.int
  def defaultValue: Option[Int] = None
  final def deserializeRaw(input: RawInput): Option[Int] = makeValue(deser.deserializeRaw(input))

  final def deserializeNested(input: NestedInput): Option[Int] = {
    if (input.nextValueIsNull) None else makeValue(deser.deserializeNested(input))
  }

  private[serializer] def makeValue(i: Int): Option[Int] = Some.cached(i)
}

/**
 * A Specialzed Option[Long] deserializer that uses the fm-common OptionCache via the Some.cached implicit
 */
object LongOptionDeserializer extends Deserializer[Option[Long]] {
  private val deser: Deserializer[Long] = PrimitiveImplicits.long
  def defaultValue: Option[Long] = None
  final def deserializeRaw(input: RawInput): Option[Long] = makeValue(deser.deserializeRaw(input))

  final def deserializeNested(input: NestedInput): Option[Long] = {
    if (input.nextValueIsNull) None else makeValue(deser.deserializeNested(input))
  }

  private[serializer] def makeValue(i: Long): Option[Long] = Some.cached(i)
}