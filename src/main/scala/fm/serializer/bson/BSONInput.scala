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

import fm.serializer.{CollectionInput, FieldInput, Input}
import org.bson.types.{MaxKey, MinKey, ObjectId}
import org.bson.{BsonBinary, BsonReader, BsonType}

object BSONInput {
  private val DefaultMaxKey: MaxKey = new MaxKey()
  private val DefaultMinKey: MinKey = new MinKey()
}

final class BSONInput(reader: BsonReader) extends Input {
  import BSONInput.{DefaultMaxKey, DefaultMinKey}

  def allowStringMap: Boolean = true

  private[this] var currentBsonType: BsonType = null

  private def isBsonType(tpe: BsonType): Boolean = {
    // Read the next type if we don't currently have it set
    if (currentBsonType == null) currentBsonType = reader.readBsonType()
    currentBsonType == tpe
  }

  private def isNotBsonType(tpe: BsonType): Boolean = !isBsonType(tpe)

  private def clearBsonType(): Unit = currentBsonType = null


  //
  // FIELD Input
  //

  // Note: copied from JSONInput
  def readFieldNumber(nameToNumMap: Map[String, Int]): Int = {
    val name: String = readFieldName()

    if (null == name) return 0

    try {
      // Exact name match
      nameToNumMap(name)
    } catch {
      case _: NoSuchElementException =>
        // TODO: possibly require that the map be pre-populated with the lower case versions so we don't have to search through it
        val lowerName: String = name.toLowerCase
        if (nameToNumMap.contains(lowerName)) nameToNumMap(lowerName)
        else nameToNumMap.find{ case (n,i) => n.toLowerCase == lowerName }.map{ _._2 }.getOrElse(-1)
    }
  }

  def readFieldName(): String = {
    if (isBsonType(BsonType.END_OF_DOCUMENT)) {
      null
    } else {
      reader.readName()
    }
  }

  def skipUnknownField(): Unit = {
    reader.skipValue()
    clearBsonType()
  }

  //
  // COLLECTION Input
  //
  def hasAnotherElement: Boolean = isNotBsonType(BsonType.END_OF_DOCUMENT)

  //
  // RAW Input
  //

  // Special Types
  def readRawBsonBinary(): BsonBinary = {
    clearBsonType()
    reader.readBinaryData()
  }

  def readRawObjectId(): ObjectId = {
    clearBsonType()
    reader.readObjectId()
  }

  def readRawDateTime(): Long = {
    clearBsonType()
    reader.readDateTime()
  }

  def readRawMaxKey(): MaxKey = {
    clearBsonType()
    reader.readMaxKey()
    DefaultMaxKey
  }

  def readRawMinKey(): MinKey = {
    clearBsonType()
    reader.readMinKey()
    DefaultMinKey
  }

  // Basic Types
  def readRawBool(): Boolean = {
    clearBsonType()
    reader.readBoolean()
  }

  def readRawFloat(): Float = {
    clearBsonType()
    reader.readDouble().toFloat // BSON doesn't have a Float type
  }

  def readRawDouble(): Double = {
    clearBsonType()
    reader.readDouble()
  }

  def readRawString(): String = {
    if (nextValueIsNull) return null

    clearBsonType()
    reader.readString()
  }

  // Bytes
  def readRawByteArray(): Array[Byte] = {
    clearBsonType()
    reader.readBinaryData().getData
  }

  // Ints -- BSON only has signed ints
  def readRawInt(): Int = {
    clearBsonType()
    reader.readInt32()
  }


  def readRawUnsignedInt(): Int = {
    clearBsonType()
    reader.readInt32()
  }

  def readRawSignedInt(): Int = {
    clearBsonType()
    reader.readInt32()
  }

  def readRawFixedInt(): Int = {
    clearBsonType()
    reader.readInt32()
  }

  // Longs -- BSON only has signed longs
  def readRawLong(): Long = {
    clearBsonType()
    reader.readInt64()
  }

  def readRawUnsignedLong(): Long = {
    clearBsonType()
    reader.readInt64()
  }

  def readRawSignedLong(): Long = {
    clearBsonType()
    reader.readInt64()
  }

  def readRawFixedLong(): Long = {
    clearBsonType()
    reader.readInt64()
  }

  // Objects
  def readRawObject[T](f: FieldInput => T): T = {
    val res: T = if (nextValueIsNull) {
      null.asInstanceOf[T]
    } else {
      reader.readStartDocument()
      clearBsonType()
      val res: T = f(this)
      reader.readEndDocument()
      res
    }

    clearBsonType()
    res
  }

  // Collections
  def readRawCollection[T](f: CollectionInput => T): T = {
    val res: T = if (nextValueIsNull) {
      null.asInstanceOf[T]
    } else {
      reader.readStartArray()
      clearBsonType()
      val res: T = f(this)
      reader.readEndArray()
      res
    }

    clearBsonType()
    res
  }

  //
  // NESTED Input
  //

  // Special Types
  def readNestedBsonBinary(): BsonBinary = readRawBsonBinary()
  def readNestedObjectId(): ObjectId = readRawObjectId()
  def readNestedDateTime(): Long = readRawDateTime()
  def readNestedMaxKey(): MaxKey = readRawMaxKey()
  def readNestedMinKey(): MinKey = readRawMinKey()

  // Basic Types
  def readNestedBool(): Boolean = readRawBool()
  def readNestedFloat(): Float = readRawFloat()
  def readNestedDouble(): Double = readRawDouble()
  def readNestedString(): String = readRawString()

  // Bytes
  def readNestedByteArray(): Array[Byte] = readRawByteArray()

  // Ints
  def readNestedInt(): Int = readRawInt()
  def readNestedUnsignedInt(): Int = readRawUnsignedInt()
  def readNestedSignedInt(): Int = readRawSignedInt()
  def readNestedFixedInt(): Int = readRawFixedInt()

  // Longs
  def readNestedLong(): Long = readRawLong()
  def readNestedUnsignedLong(): Long = readRawUnsignedLong()
  def readNestedSignedLong(): Long = readRawSignedLong()
  def readNestedFixedLong(): Long = readRawFixedLong()

  // Objects
  def readNestedObject[T](f: FieldInput => T): T = readRawObject(f)

  // Collections
  def readNestedCollection[T](f: CollectionInput => T): T = readRawCollection(f)

  /**
   * Returns true if the next value is known to be null otherwise false if the value is not null or is unknown.
   * This means that even if the next value ends up being null this can return false.
   *
   * Note: If the next value is null then this method should consume that input
   */
  def nextValueIsNull: Boolean = {
    if (isNotBsonType(BsonType.NULL)) return false

    clearBsonType()
    reader.readNull()
    true
  }
}
