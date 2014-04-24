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

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

object CanBuildFromDeserializer {
  def forVector[Elem,Col](implicit elemDeser: Deserializer[Elem]): CanBuildFromDeserializer[Elem,Col] = {
    val cbf: CanBuildFrom[_,Elem,Col] = Vector.canBuildFrom[Elem].asInstanceOf[CanBuildFrom[_,Elem,Col]]
    new CanBuildFromDeserializer[Elem, Col]()(cbf, elemDeser)
  }
}

final class CanBuildFromDeserializer[Elem,Col](implicit cbf: CanBuildFrom[_,Elem,Col], elemDeser: Deserializer[Elem]) extends Deserializer[Col] {  
  // TODO: make this a macro to use a Col.empty method (if one exists)
  def defaultValue: Col = cbf().result
  
  def deserializeRaw(input: RawInput): Col = input.readRawCollection{ readCollection }
  def deserializeNested(input: NestedInput): Col = input.readNestedCollection{ readCollection }
  
  private def readCollection(input: CollectionInput): Col = {
    val builder: Builder[Elem,Col] = cbf()
    
    while (input.hasAnotherElement) {
      builder += elemDeser.deserializeNested(input)
    }
    
    builder.result
  }
  
}