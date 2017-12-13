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
final class ImmutableArrayDeserializer[Elem : ClassTag, Col >: ImmutableArray[Elem]](implicit elemDeser: Deserializer[Elem]) extends CollectionDeserializerBase[Col] {
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