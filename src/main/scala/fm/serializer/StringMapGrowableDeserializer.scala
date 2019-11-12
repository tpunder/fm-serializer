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

import scala.collection.generic.Growable

final class StringMapGrowableDeserializer[V,Col <: Growable[(String,V)]](newInstance: => Col)(implicit elemDeser: Deserializer[(String,V)], valueDeser: Deserializer[V]) extends Deserializer[Col] {
  // If the underlying input doesn't support a StringMap type of input (e.g. Protobuf) then we will use this GrowableDeserializer instead
  private[this] val growableDeserializer: GrowableDeserializer[(String,V), Col] = new GrowableDeserializer(newInstance)
  
  def defaultValue: Col = newInstance
  
  def deserializeRaw(input: RawInput): Col = if (input.allowStringMap) input.readRawObject{ readCollection } else growableDeserializer.deserializeRaw(input)
  def deserializeNested(input: NestedInput): Col = if (input.allowStringMap) input.readNestedObject{ readCollection } else growableDeserializer.deserializeNested(input)
  
  private def readCollection(input: FieldInput): Col = {
    val col: Col = newInstance
    
    var name: String = input.readFieldName()
    
    while (null != name) {
      col += ((name, valueDeser.deserializeNested(input)))
      
      name = input.readFieldName()
    }
    
    col
  }
}