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

import java.lang.{Iterable => JavaIterable}
import java.util.{Iterator => JavaIterator, List => JavaList, RandomAccess => JavaRandomAccess}

/**
 * A Serializer/FieldSerializer for a Java Iterable
 */
final class JavaIterableSerializer[T, Col <: JavaIterable[T]](implicit elemSerializer: Serializer[T]) extends Serializer[Col] with FieldSerializer[Col] {
  final def serializeRaw(output: RawOutput, col: Col): Unit = output.writeRawCollection(col){ serializeElems }
  final def serializeNested(output: NestedOutput, col: Col): Unit = output.writeNestedCollection(col){ serializeElems }
  final def serializeField(output: FieldOutput, number: Int, name: String, col: Col): Unit = output.writeFieldCollection(number, name, col){ serializeElems }
  
  private[this] val serializeElems: Function2[NestedOutput, Col, Unit] = new Function2[NestedOutput, Col, Unit] {
    def apply(out: NestedOutput, col: Col): Unit = {
      col match {
        case list: JavaList[T] with JavaRandomAccess => serializeRandomAccess(out, list)
        case _ => serializeIterable(out, col)
      }
    }
    
    private def serializeRandomAccess(out: NestedOutput, col: JavaList[T]): Unit = {
      var i: Int = 0
      val size: Int = col.size()
      while (i < size) {
        elemSerializer.serializeNested(out, col.get(i))
        i += 1
      }
    }
    
    private def serializeIterable(out: NestedOutput, col: JavaIterable[T]): Unit = {
      val it: JavaIterator[T] = col.iterator()
      while (it.hasNext) {
        elemSerializer.serializeNested(out, it.next)
      }
    }
  }
  
//  private[this] def serializeElems(out: NestedOutput, col: Col): Unit = {
//    val it: JavaIterator[T] = col.iterator()
//    while (it.hasNext) {
//      elemSerializer.serializeNested(out, it.next)
//    }
//  }
}