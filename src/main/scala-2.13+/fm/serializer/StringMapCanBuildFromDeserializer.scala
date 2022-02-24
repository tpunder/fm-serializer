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

import scala.collection.Factory
import scala.collection.mutable.Builder

object StringMapCanBuildFromDeserializer {
  def forVector[V](implicit elemDeser: Deserializer[(String, V)], valueDeser: Deserializer[V]): StringMapCanBuildFromDeserializer[V, Vector[(String, V)]] = {
    val cbf: Factory[(String, V), Vector[(String, V)]] = implicitly[Factory[(String, V), Vector[(String, V)]]]
    new StringMapCanBuildFromDeserializer[V, Vector[(String, V)]]()(cbf, elemDeser, valueDeser)
  }
}

final class StringMapCanBuildFromDeserializer[V, Col](implicit cbf: Factory[(String,V),Col], elemDeser: Deserializer[(String,V)], valueDeser: Deserializer[V]) extends Deserializer[Col] {
  // If the underlying input doesn't support a StringMap type of input (e.g. Protobuf) then we will use this CanBuildFromDeserializer instead
  private[this] val cbfDeserializer: CanBuildFromDeserializer[(String,V), Col] = new CanBuildFromDeserializer()
  
  // TODO: make this a macro to use a Col.empty method (if one exists)
  def defaultValue: Col = cbf.newBuilder.result()
  
  def deserializeRaw(input: RawInput): Col = if (input.allowStringMap) input.readRawObject{ readCollection } else cbfDeserializer.deserializeRaw(input)
  def deserializeNested(input: NestedInput): Col = if (input.allowStringMap) input.readNestedObject{ readCollection } else cbfDeserializer.deserializeNested(input)
  
  private def readCollection(input: FieldInput): Col = {
    val builder: Builder[(String,V),Col] = cbf.newBuilder
    
    var name: String = input.readFieldName()
    
    while (name != null) {
      builder += ((name, valueDeser.deserializeNested(input)))
      name = input.readFieldName()
    }
    
    builder.result()
  }
  
}