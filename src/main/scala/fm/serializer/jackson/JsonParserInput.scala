/*
 * Copyright 2019 Frugal Mechanic (http://frugalmechanic.com)
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
package fm.serializer.jackson

import com.fasterxml.jackson.core.{JsonParser, JsonToken}
import fm.json._
import fm.serializer.json.{JSONDeserializerOptions, JSONInput}
import fm.serializer.{CollectionInput, FieldInput, FieldNameToNumberLookup, Input}
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}

/**
 * An Input implementation on top of Jackson's JsonParser
 */
final class JsonParserInput(parser: JsonParser, options: JSONDeserializerOptions) extends Input {
  override def allowStringMap: Boolean = true

  /**
   * Is there another element to read in the collection?
   */
  override def hasAnotherElement: Boolean = !hasTokenOrAdvance(JsonToken.END_ARRAY)

  override def readRawBool(): Boolean = JsonBoolean.parse(parser)
  override def readRawFloat(): Float = JsonFloat.parse(parser)
  override def readRawDouble(): Double = JsonDouble.parse(parser)

  override def readRawBigInteger(): JavaBigInteger = if (nextValueIsNull) null else JsonBigInteger.parse(parser)
  override def readRawBigDecimal(): JavaBigDecimal = if (nextValueIsNull) null else JsonBigDecimal.parse(parser)

  override def readRawString(): String = {
    if (nextValueIsNull) return null

    val res: String = JsonString.parse(parser)
    if (options.internStrings) res.intern() else res
  }

  def readRawByteArray(): Array[Byte] = {
    if (nextValueIsNull) {
      null
    } else {
      // TODO: make a JsonBinary in fm-common?
      val res: Array[Byte] = parser.getBinaryValue()
      parser.nextToken() // Must advance to next token since this isn't like the JsonNode.parse calls
      res
    }
  }

  override def readRawInt(): Int = JsonInt.parse(parser)
  override def readRawUnsignedInt(): Int = readRawInt()
  override def readRawSignedInt(): Int = readRawInt()
  override def readRawFixedInt(): Int = readRawInt()
  override def readRawLong(): Long = JsonLong.parse(parser)
  override def readRawUnsignedLong(): Long = readRawLong()
  override def readRawSignedLong(): Long = readRawLong()
  override def readRawFixedLong(): Long = readRawLong()

  override def readRawObject[T](f: FieldInput => T): T = {
    if (consumeToken(JsonToken.VALUE_NULL)) return null.asInstanceOf[T]

    requireAndConsumeToken(JsonToken.START_OBJECT)
    val res: T = f(this)
    requireAndConsumeToken(JsonToken.END_OBJECT)
    res
  }

  override def readRawCollection[T](f: CollectionInput => T): T = {
    if (consumeToken(JsonToken.VALUE_NULL)) return null.asInstanceOf[T]

    requireAndConsumeToken(JsonToken.START_ARRAY)
    val res: T = f(this)
    requireAndConsumeToken(JsonToken.END_ARRAY)
    res
  }

  /**
   * Returns true if the next value is known to be null otherwise false if the value is not null or is unknown.
   * This means that even if the next value ends up being null this can return false.
   *
   * Note: If the next value is null then this method should consume that input
   */
  override def nextValueIsNull: Boolean = consumeToken(JsonToken.VALUE_NULL)

  private[this] var _lastFieldName: String = ""

  final override def lastFieldName(): String = _lastFieldName
  final override def lastFieldNumber(): Int = 0 // Use for unknown field reporting - there will be no field number

  /**
   * This is for reading fields of an object.
   *
   * Return the field number for the next readable field.
   * Returns 0 if we've reached the end of the object/message
   */
  override def readFieldNumber(nameToNumMap: FieldNameToNumberLookup): Int = {
    val fieldName: String = readFieldName()
    _lastFieldName = fieldName
    JSONInput.readFieldNumber(fieldName, nameToNumMap)
  }

  /**
   * If dynamic string maps are supported then this should be implemented
   * otherwise this can just throw an exception.
   *
   * null should be returned on the end of an object/message
   */
  override def readFieldName(): String = {
//    println(s"readFieldName - START - CurrentToken: ${currentTokenOrAdvance()}")

    val res: String = if (hasTokenOrAdvance(JsonToken.END_OBJECT)) {
      null // End of Object
    } else {
      requireAndConsumeToken(JsonToken.FIELD_NAME)
      parser.currentName()
    }

//    println(s"readFieldName - END - CurrentToken: ${currentTokenOrAdvance()} - FieldName: $res")

    res
  }

  /**
   * Skip an unknown field value.
   *
   * If after calling readFieldNumber(...) we don't know how
   * to handle the resulting field number then this method
   * can be called to skip the value of the field after which
   * we can call readFieldNumber(...) again.
   */
  override def skipUnknownField(): Unit = {
    val token: JsonToken = if (hasTokenOrAdvance(JsonToken.FIELD_NAME)) parser.nextToken() else parser.currentToken()

    if (null == token) return

    token match {
      case JsonToken.START_OBJECT | JsonToken.START_ARRAY =>
        parser.skipChildren()
        parser.nextToken() // Advance past the END_OBJECT or END_ARRAY
      case _ =>
        parser.nextToken()
    }
  }

  override def readNestedBool(): Boolean = readRawBool()
  override def readNestedFloat(): Float = readRawFloat()
  override def readNestedDouble(): Double = readRawDouble()
  override def readNestedBigInteger(): JavaBigInteger = readRawBigInteger()
  override def readNestedBigDecimal(): JavaBigDecimal = readRawBigDecimal()
  override def readNestedString(): String = readRawString()
  override def readNestedByteArray(): Array[Byte] = readRawByteArray()
  override def readNestedInt(): Int = readRawInt()
  override def readNestedUnsignedInt(): Int = readRawUnsignedInt()
  override def readNestedSignedInt(): Int = readRawSignedInt()
  override def readNestedFixedInt(): Int = readRawFixedInt()
  override def readNestedLong(): Long = readRawLong()
  override def readNestedUnsignedLong(): Long = readRawUnsignedLong()
  override def readNestedSignedLong(): Long = readRawSignedLong()
  override def readNestedFixedLong(): Long = readRawFixedLong()
  override def readNestedObject[T](f: FieldInput => T): T = readRawObject(f)
  override def readNestedCollection[T](f: CollectionInput => T): T = readRawCollection(f)

  private def currentTokenOrAdvance(): JsonToken = {
    if (hasTokenOrAdvance()) parser.currentToken else null
  }

  private def hasTokenOrAdvance(token: JsonToken): Boolean = {
    currentTokenOrAdvance() == token
  }

  private def hasTokenOrAdvance(): Boolean = {
    parser.hasCurrentToken || parser.nextToken != null
  }

  private def requireAndConsumeToken(token: JsonToken): Unit = {
    require(consumeToken(token), s"Expected $token but got: ${parser.currentToken}")
  }

  private def consumeToken(token: JsonToken): Boolean = {
    if (hasTokenOrAdvance(token)) {
      parser.nextToken()
      true
    } else {
      false
    }
  }
}
