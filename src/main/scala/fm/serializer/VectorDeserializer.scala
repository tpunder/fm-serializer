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
final class VectorDeserializer[Elem, Col >: Vector[Elem]](implicit elemDeser: Deserializer[Elem]) extends CollectionDeserializerBase[Col] {
  // Our default CanBuildFromDeserializer creates a new Builder and then calls result().  This is optimized
  // to just return Vector.empty without creating a new Builder.
  // TODO: figure out how to generalize this (perhaps using something like IndexedSeqFactory)
  def defaultValue: Col = Vector.empty

  protected def readCollection(input: CollectionInput): Col = {
    if (!input.hasAnotherElement) return Vector.empty

    // TODO: Add pooling of the Vector Builders?
    val builder: mutable.Builder[Elem, Vector[Elem]] = Vector.newBuilder[Elem]

    while (input.hasAnotherElement) {
      builder += elemDeser.deserializeNested(input)
    }

    builder.result()
  }
}