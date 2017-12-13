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

import scala.collection.mutable

/**
 * A specialized implementation for deserializing Vectors.
 */
final class VectorDeserializer[Elem, Col >: Vector[Elem]](implicit elemDeser: Deserializer[Elem]) extends Deserializer[Col] {
  // Our default CanBuildFromDeserializer creates a new Builder and then calls result().  This is optimized
  // to just return Vector.empty without creating a new Builder.
  // TODO: figure out how to generalize this (perhaps using something like IndexedSeqFactory)
  def defaultValue: Vector[Elem] = Vector.empty

  def deserializeRaw(input: RawInput): Vector[Elem] = input.readRawCollection{ readCollection }
  def deserializeNested(input: NestedInput): Vector[Elem] = input.readNestedCollection{ readCollection }

  private def readCollection(input: CollectionInput): Vector[Elem] = {
    // TODO: Add pooling of the Vector Builders?
    var builder: mutable.Builder[Elem, Vector[Elem]] = null

    while (input.hasAnotherElement) {
      if (null == builder) builder = Vector.newBuilder[Elem]
      builder += elemDeser.deserializeNested(input)
    }

    if (null == builder) Vector.empty else builder.result()
  }
}