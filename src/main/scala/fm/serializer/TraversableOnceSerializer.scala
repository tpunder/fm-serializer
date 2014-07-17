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

/**
 * A Serializer for a TraversableOnce
 */
final class TraversableOnceSerializer[@specialized T, Col <: TraversableOnce[T]](implicit elemSerializer: Serializer[T]) extends Serializer[Col] {
  final def serializeRaw(output: RawOutput, col: Col): Unit = output.writeRawCollection(col){ serializeElems }
  final def serializeNested(output: NestedOutput, col: Col): Unit = output.writeNestedCollection(col){ serializeElems }
  final def serializeField(output: FieldOutput, number: Int, name: String, col: Col): Unit = output.writeFieldCollection(number, name, col){ serializeElems }
  
  private[this] val serializeElems: Function2[NestedOutput, Col, Unit] = new Function2[NestedOutput, Col, Unit] {
    def apply(out: NestedOutput, col: Col): Unit = {
      col match {
        case indexed: scala.collection.IndexedSeq[T] => serializeIndexedSeq(out, indexed)
        case iterable: Iterable[T] => serializeIterable(out, iterable)
        case _ => serializeTraversableOnce(out, col)
      }
    }
    
    private def serializeIndexedSeq(out: NestedOutput, col: scala.collection.IndexedSeq[T]): Unit = {
      var i: Int = 0
      val len: Int = col.length
      while (i < len) {
        elemSerializer.serializeNested(out, col(i))
        i += 1
      }
    }
    
    private def serializeIterable(out: NestedOutput, col: Iterable[T]): Unit = {
      val it: Iterator[T] = col.iterator
      while (it.hasNext) elemSerializer.serializeNested(out, it.next)
    }
    
    private def serializeTraversableOnce(out: NestedOutput, col: Col): Unit = {
      col.foreach{ elem: T => elemSerializer.serializeNested(out, elem) }
    }
  }
}