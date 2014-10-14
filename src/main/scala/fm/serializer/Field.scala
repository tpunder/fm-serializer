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

import scala.annotation.StaticAnnotation
import scala.annotation.meta.{getter, param, setter}

@getter
@setter
@param
final class Field(number: Int, name: String, getter: String, setter: String, constructorIdx: Int, serializer: Serializer[_], deserializer: Deserializer[_]) extends StaticAnnotation {
  
  // Without Serializer Arg:
  def this(number: Int, name: String, getter: String, constructorIdx: Int) = this(number, name, getter, null, constructorIdx, null, null) 
  def this(number: Int, name: String, getter: String, setter: String) = this(number, name, getter, setter, -1, null, null)  
  def this(number: Int, name: String) = this(number, name, null, null, -1, null, null)
  def this(number: Int) = this(number, null, null, null, -1, null, null)
  
  // With Serializer Arg:
  def this(number: Int, name: String, getter: String, constructorIdx: Int, serializer: SimpleSerializer[_]) = this(number, name, getter, null, constructorIdx, serializer, serializer) 
  def this(number: Int, name: String, getter: String, setter: String, serializer: SimpleSerializer[_]) = this(number, name, getter, setter, -1, serializer, serializer)  
  def this(number: Int, name: String, serializer: SimpleSerializer[_]) = this(number, name, null, null, -1, serializer, serializer)
  def this(number: Int, serializer: SimpleSerializer[_]) = this(number, null, null, null, -1, serializer, serializer)
  def this(serializer: SimpleSerializer[_]) = this(-1, null, null, null, -1, serializer, serializer)
  
  throw new AssertionError("This class shouldn't be instantiated anywhere")
}
