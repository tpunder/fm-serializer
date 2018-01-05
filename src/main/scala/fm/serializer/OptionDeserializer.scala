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

/**
 * Note: This caches Option[Int] instances (much like java.lang.Integer caches Integer instances).  By default the cache
 *       attempts to use the same range that the java.lang.Integer cache uses but the method used to determine the range
 *       is somewhat dependant on how OpenJDK handles caching.  You can override the default upper limit using the
 *       "fm.serializer.IntOptionDeserializer.Cache.high" system property if the default behavior of trying to detect
 *       the java.lang.Integer cache range doesn't work for you.
 */
object IntOptionDeserializer extends Deserializer[Option[Int]] {
  private[serializer] val low: Int = -127

  // Note: Ideally we would use the java.lang.Integer.IntegerCache.high property to get this value but the JVM removes
  //       that property from public access.  So we use brute force to determine the value instead
  //
  // Original code that does not work:
  //    System.getProperty("java.lang.Integer.IntegerCache.high").toIntOption.getOrElse(127)
  private[serializer] val high: Int = {
    System.getProperty("fm.serializer.IntOptionDeserializer.Cache.high").toIntOption.getOrElse{ determineIntegerCacheHighValue() }
  }

  /** The min value we will cache (for debugging what this value is being set to) */
  def CacheLowValue: Int = low

  /** The max value we will cache (for debugging what this value is being set to) */
  def CacheHighValue: Int = high

  // Use brute force to determine the value of the private system property "java.lang.Integer.IntegerCache.high"
  private def determineIntegerCacheHighValue(): Int = {
    // The java.lang.Integer.IntegerCache is hardcoded to not be lower than this value
    val IntegerCacheMinValue: Int = 127

    // The maximum value we are willing to go up to.  Since this implementation is somewhat
    // dependant of the behavior of OpenJDK let's set an upper limit to avoid out of memory
    // errors (or other unexpected behaviors) if the JVM we are running in does something else
    // for the caching of Integers.  For Example:  If a JVM used a WeakHashMap for integer caching
    // then `Integer.valueOf(i) eq Integer.valueOf(i)` would always be true.
    val IntegerCacheMaxValue: Int = 1000000

    var i: Int = IntegerCacheMinValue

    while (i < IntegerCacheMaxValue) {
      if (Integer.valueOf(i) ne Integer.valueOf(i)) return i - 1
      i += 1
    }

    IntegerCacheMaxValue
  }

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