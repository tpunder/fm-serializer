/*
 * Copyright 2016 Frugal Mechanic (http://frugalmechanic.com)
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
package fm.serializer.bson

import fm.serializer.{Deserializer, Serializer, TestSerializer}
import org.bson.BsonDocument

final class TestBsonDocument extends TestSerializer[BsonDocument] {
  override def supportsRawCollections: Boolean = false
  def serialize[T](v: T)(implicit ser: Serializer[T]): BsonDocument = BSON.toBsonDocument(v)
  def deserialize[T](doc: BsonDocument)(implicit deser: Deserializer[T]): T = BSON.fromBsonDocument[T](doc)
}

final class TestBsonBytes extends TestSerializer[Array[Byte]] {
  override def supportsRawCollections: Boolean = false
  def serialize[T](v: T)(implicit ser: Serializer[T]): Array[Byte] = BSON.toBsonBytes(v)
  def deserialize[T](bson: Array[Byte])(implicit deser: Deserializer[T]): T = BSON.fromBsonBytes[T](bson)
}
