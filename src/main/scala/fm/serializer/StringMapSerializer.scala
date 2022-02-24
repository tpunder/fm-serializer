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
 * A Serializer for a Map[String,V] (or rather TraversableOnce[(String,V)]) that allows us to output
 * a JSON Object for a Map[String,V] instead of an Array[(String,V)].  If the underlying Output doesn't
 * support this style (e.g. Protobuf) then the TraversableOnceSerializer is used instead.
 */
final class StringMapSerializer[@specialized V, Col <: TraversableOnce[(String,V)]](implicit elemSerializer: Serializer[(String,V)], valueSerializer: Serializer[V]) extends Serializer[Col] {
  // If the underlying output doesn't support a StringMap type of output (e.g. Protobuf) then we will use this TraversableOnceSerializer instead
  private[this] val travOnceSerializer: TraversableOnceSerializer[(String,V), Col] = new TraversableOnceSerializer()
  
  final def serializeRaw(output: RawOutput, col: Col): Unit = {
    if (output.allowStringMap) output.writeRawObject(col){ serializeElems } else travOnceSerializer.serializeRaw(output, col)
  }
  
  final def serializeNested(output: NestedOutput, col: Col): Unit = {
    if (output.allowStringMap) output.writeNestedObject(col){ serializeElems } else travOnceSerializer.serializeNested(output, col)
  }
  
  final def serializeField(output: FieldOutput, number: Int, name: String, col: Col): Unit = {
    if (output.allowStringMap) output.writeFieldObject(number, name, col){ serializeElems } else travOnceSerializer.serializeField(output, number, name, col)
  }
  
  private[this] val serializeElems: Function2[FieldOutput, Col, Unit] = new Function2[FieldOutput, Col, Unit] {
    def apply(out: FieldOutput, col: Col): Unit = {
      col match {
        case indexed: scala.collection.IndexedSeq[(String,V)] => serializeIndexedSeq(out, indexed)
        case iterable: Iterable[(String,V)] => serializeIterable(out, iterable)
        case _ => serializeTraversableOnce(out, col)
      }
    }
    
    private def serializeIndexedSeq(out: FieldOutput, col: scala.collection.IndexedSeq[(String,V)]): Unit = {
      var i: Int = 0
      val len: Int = col.length
      while (i < len) {
        val (name, value) = col(i)
        valueSerializer.serializeField(out, -1, name, value)
        i += 1
      }
    }
    
    private def serializeIterable(out: FieldOutput, col: Iterable[(String,V)]): Unit = {
      val it: Iterator[(String,V)] = col.iterator
      while (it.hasNext) {
        val (name, value) = it.next()
        valueSerializer.serializeField(out, -1, name, value)
      }
    }
    
    private def serializeTraversableOnce(out: FieldOutput, col: Col): Unit = {
      col.foreach{ case (name, value) => valueSerializer.serializeField(out, -1, name, value) }
    }
  }
}