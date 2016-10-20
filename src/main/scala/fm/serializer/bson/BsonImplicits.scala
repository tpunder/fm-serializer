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

import fm.common.{ImmutableDate, UUID}
import fm.serializer._
import java.util.Date
import org.bson.types.{MaxKey, MinKey, ObjectId}

trait BsonImplicits {
  implicit val uuidSerializer: SimpleSerializer[UUID] = new UUIDSerializer()
  implicit val objectIdSerializer: SimpleSerializer[ObjectId] = new ObjectIdSerializer()
  implicit val dateSerializer: SimpleSerializer[Date] = new DateSerializer()
  implicit val immutableDateSerializer: SimpleSerializer[ImmutableDate] = new ImmutableDateSerializer()
  implicit val maxKeySerializer: SimpleSerializer[MaxKey] = new MaxKeySerializer()
  implicit val minKeySerializer: SimpleSerializer[MinKey] = new MinKeySerializer()
}



