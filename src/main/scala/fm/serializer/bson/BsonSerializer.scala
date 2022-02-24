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
import org.bson.{BsonBinary, BsonBinarySubType}
import org.bson.types.{MaxKey, MinKey, ObjectId}

final class UUIDSerializer extends BsonSerializer[UUID] {
  protected val normal: SimpleSerializer[UUID] = Primitive.string.map[UUID](defaultValue){ (id: UUID) => id.toHex }{ (hex: String) => UUID(hex) }

  def defaultValue: UUID = null
  def deserializeRaw(input: BSONInput): UUID = if (input.nextValueIsNull) null else fromBsonBinary(input.readRawBsonBinary())
  def deserializeNested(input: BSONInput): UUID = if (input.nextValueIsNull) null else fromBsonBinary(input.readNestedBsonBinary())
  def serializeRaw(output: BSONOutput, v: UUID): Unit = if (null != v) output.writeRawBsonBinary(toBsonBinary(v))
  def serializeNested(output: BSONOutput, v: UUID): Unit = if (null != v) output.writeNestedBsonBinary(toBsonBinary(v))
  def serializeField(output: BSONOutput, number: Int, name: String, v: UUID): Unit = if (null != v) output.writeFieldBsonBinary(number, name, toBsonBinary(v)) else output.writeFieldNull(number, name)

  private def toBsonBinary(v: UUID): BsonBinary = new BsonBinary(BsonBinarySubType.UUID_STANDARD, v.toByteArray())
  private def fromBsonBinary(b: BsonBinary): UUID = UUID(b.getData())
}

final class ObjectIdSerializer extends BsonSerializer[ObjectId] {
  protected val normal: SimpleSerializer[ObjectId] = Primitive.string.map[ObjectId](defaultValue){ (id: ObjectId) => id.toHexString }{ (hex: String) => new ObjectId(hex) }

  def defaultValue: ObjectId = null
  def deserializeRaw(input: BSONInput): ObjectId = if (input.nextValueIsNull) null else input.readRawObjectId()
  def deserializeNested(input: BSONInput): ObjectId = if (input.nextValueIsNull) null else input.readNestedObjectId()
  def serializeRaw(output: BSONOutput, v: ObjectId): Unit = if (null != v) output.writeRawObjectId(v)
  def serializeNested(output: BSONOutput, v: ObjectId): Unit = output.writeNestedObjectId(v)
  def serializeField(output: BSONOutput, number: Int, name: String, v: ObjectId): Unit = if (null != v) output.writeFieldObjectId(number, name, v) else output.writeFieldNull(number, name)
}

final class DateSerializer extends BsonSerializer[Date] {
  protected val normal: SimpleSerializer[Date] = Primitive.long.map[Date](defaultValue){ (date: Date) => date.getTime }{ (time: Long) => new Date(time) }

  def defaultValue: Date = null
  def deserializeRaw(input: BSONInput): Date = if (input.nextValueIsNull) null else new Date(input.readRawDateTime())
  def deserializeNested(input: BSONInput): Date = if (input.nextValueIsNull) null else new Date(input.readNestedDateTime())
  def serializeRaw(output: BSONOutput, v: Date): Unit = if (null != v) output.writeRawDateTime(v.getTime)
  def serializeNested(output: BSONOutput, v: Date): Unit = if (null != v) output.writeNestedDateTime(v.getTime)
  def serializeField(output: BSONOutput, number: Int, name: String, v: Date): Unit = if (null != v) output.writeFieldDateTime(number, name, v.getTime) else output.writeFieldNull(number, name)
}

final class ImmutableDateSerializer extends BsonSerializer[ImmutableDate] {
  protected val normal: SimpleSerializer[ImmutableDate] = Primitive.long.map[ImmutableDate](defaultValue){ (date: ImmutableDate) => date.getTime }{ (time: Long) => new ImmutableDate(time) }

  def defaultValue: ImmutableDate = null.asInstanceOf[ImmutableDate]
  def deserializeRaw(input: BSONInput): ImmutableDate = if (input.nextValueIsNull) null else new ImmutableDate(input.readRawDateTime())
  def deserializeNested(input: BSONInput): ImmutableDate = if (input.nextValueIsNull) null else new ImmutableDate(input.readNestedDateTime())
  def serializeRaw(output: BSONOutput, v: ImmutableDate): Unit = if (null != v) output.writeRawDateTime(v.getTime)
  def serializeNested(output: BSONOutput, v: ImmutableDate): Unit = if (null != v) output.writeNestedDateTime(v.getTime)
  def serializeField(output: BSONOutput, number: Int, name: String, v: ImmutableDate): Unit = if (null != v) output.writeFieldDateTime(number, name, v.getTime) else output.writeFieldNull(number, name)
}

final class MaxKeySerializer extends BsonSerializer[MaxKey] {
  protected val normal: SimpleSerializer[MaxKey] = Primitive.string.map[MaxKey](defaultValue){ (_: MaxKey) => "MaxKey" }{ (_: String) => defaultValue }

  val defaultValue: MaxKey = new MaxKey()
  def deserializeRaw(input: BSONInput): MaxKey = input.readRawMaxKey()
  def deserializeNested(input: BSONInput): MaxKey = input.readNestedMaxKey()
  def serializeRaw(output: BSONOutput, v: MaxKey): Unit = output.writeRawMaxKey()
  def serializeNested(output: BSONOutput, v: MaxKey): Unit = output.writeNestedMaxKey()
  def serializeField(output: BSONOutput, number: Int, name: String, v: MaxKey): Unit = output.writeFieldMaxKey(number, name)
}

final class MinKeySerializer extends BsonSerializer[MinKey] {
  protected val normal: SimpleSerializer[MinKey] = Primitive.string.map[MinKey](defaultValue){ (_: MinKey) => "MinKey" }{ (_: String) => defaultValue }

  val defaultValue: MinKey = new MinKey()
  def deserializeRaw(input: BSONInput): MinKey = input.readRawMinKey()
  def deserializeNested(input: BSONInput): MinKey = input.readNestedMinKey()
  def serializeRaw(output: BSONOutput, v: MinKey): Unit = output.writeRawMinKey()
  def serializeNested(output: BSONOutput, v: MinKey): Unit = output.writeNestedMinKey()
  def serializeField(output: BSONOutput, number: Int, name: String, v: MinKey): Unit = output.writeFieldMinKey(number, name)
}

abstract class BsonSerializer[T <: AnyRef] extends SimpleSerializer[T] {
  protected def normal: SimpleSerializer[T]

  def deserializeRaw(input: BSONInput): T
  def deserializeNested(input: BSONInput): T
  def serializeRaw(output: BSONOutput, v: T): Unit
  def serializeNested(output: BSONOutput, v: T): Unit
  def serializeField(output: BSONOutput, number: Int, name: String, v: T): Unit

  final def deserializeRaw(input: RawInput): T = input match {
    case bsonInput: BSONInput => deserializeRaw(bsonInput)
    case _ => normal.deserializeRaw(input)
  }

  final def deserializeNested(input: NestedInput): T = input match {
    case bsonInput: BSONInput => deserializeNested(bsonInput)
    case _ => normal.deserializeNested(input)
  }

  final def serializeRaw(output: RawOutput, v: T): Unit = output match {
    case bsonOutput: BSONOutput => serializeRaw(bsonOutput, v)
    case _ => normal.serializeRaw(output, v)
  }

  final def serializeNested(output: NestedOutput, v: T): Unit = output match {
    case bsonOutput: BSONOutput => serializeNested(bsonOutput, v)
    case _ => normal.serializeNested(output, v)
  }

  final def serializeField(output: FieldOutput, number: Int, name: String, v: T): Unit = output match {
    case bsonOutput: BSONOutput => serializeField(bsonOutput, number, name, v)
    case _ => normal.serializeField(output, number, name, v)
  }
}