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

abstract class CollectionDeserializerBase[Col] extends Deserializer[Col] {

  final def deserializeRaw(input: RawInput): Col = input.readRawCollection(readCollectionFunction)

  final def deserializeNested(input: NestedInput): Col = input.readNestedCollection(readCollectionFunction)

  // A single static Function1 instance that avoid constantly re-creating Lambdas for the
  // deserializeRaw/deserializeNested methods.
  //
  // Note: YourKit memory allocation profiling was showing a high number of "fm.serializer.VectorDeserializer$$Lambda$495"
  //       instances being created which is why this was added.  However it is unclear if this actually makes a
  //       difference.
  private object readCollectionFunction extends Function1[CollectionInput, Col] {
    def apply(input: CollectionInput): Col = readCollection(input)
  }

  protected def readCollection(input: CollectionInput): Col
}
