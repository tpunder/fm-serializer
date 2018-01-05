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

import fm.common.{ImmutableArray, ImmutableArrayBuilder}
import scala.reflect.ClassTag

/**
 * A specialized implementation for deserializing ImmutableArrays.
 */
final class ImmutableArrayDeserializer[@specialized Elem : ClassTag, Col >: ImmutableArray[Elem]](implicit elemDeser: Deserializer[Elem]) extends CollectionDeserializerBase[Col] {
  // Our default CanBuildFromDeserializer creates a new Builder and then calls result().  This is optimized
  // to just return ImmutableArray.empty without creating a new Builder.
  // TODO: figure out how to generalize this (perhaps using something like IndexedSeqFactory)
  def defaultValue: Col = ImmutableArray.empty[Elem]

  protected def readCollection(input: CollectionInput): Col = {
    if (!input.hasAnotherElement) return ImmutableArray.empty

    // TODO: Add pooling of the ImmutableArray Builders?
    val builder: ImmutableArrayBuilder[Elem] = ImmutableArray.newBuilder[Elem]

    while (input.hasAnotherElement) {

      builder += elemDeser.deserializeNested(input)
    }

    builder.result()
  }
}

final class ImmutableIntArrayDeserializer extends CollectionDeserializerBase[ImmutableArray[Int]] {
  def defaultValue: ImmutableArray[Int] = ImmutableArray.empty[Int]

  protected def readCollection(input: CollectionInput): ImmutableArray[Int] = {
    if (!input.hasAnotherElement) return ImmutableArray.empty[Int]
    val builder: ImmutableArrayBuilder[Int] = new ImmutableArrayBuilder[Int](0)
    while (input.hasAnotherElement) builder += Primitive.int.deserializeNested(input)
    builder.result()
  }
}

final class ImmutableLongArrayDeserializer extends CollectionDeserializerBase[ImmutableArray[Long]] {
  def defaultValue: ImmutableArray[Long] = ImmutableArray.empty[Long]

  protected def readCollection(input: CollectionInput): ImmutableArray[Long] = {
    if (!input.hasAnotherElement) return ImmutableArray.empty[Long]
    val builder: ImmutableArrayBuilder[Long] = new ImmutableArrayBuilder[Long](0)
    while (input.hasAnotherElement) builder += Primitive.int.deserializeNested(input)
    builder.result()
  }
}