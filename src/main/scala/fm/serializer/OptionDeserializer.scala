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
 * For deserializing Option types.  Note: this does NOT allow Some(null)
 */
final case class OptionDeserializer[A](implicit deser: Deserializer[A]) extends Deserializer[Option[A]] {
  def defaultValue: Option[A] = None
  
  final def deserializeRaw(input: RawInput): Option[A] = Option(deser.deserializeRaw(input))
  
  final def deserializeNested(input: NestedInput): Option[A] = {
    if (input.nextValueIsNull) None else Option(deser.deserializeNested(input))
  }
}