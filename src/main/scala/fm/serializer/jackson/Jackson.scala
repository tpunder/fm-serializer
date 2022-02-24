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

import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import fm.json.{JsonNode, JsonNodeGenerator, JsonNodeParser, JsonObject}
import fm.serializer.json.{JSONDeserializerOptions, JSONSerializerOptions}
import fm.serializer.validation.{Validation, ValidationOptions, ValidationResult}
import fm.serializer.{Deserializer, Serializer}
import java.io.{Reader, StringWriter}

object Jackson {
  import fm.json.Json.{jsonFactory, jsonPrettyPrinter}

  def toMinimalJsonObject[T](v: T)(implicit serializer: Serializer[T]): JsonObject = {
    toJsonObject(v, JSONSerializerOptions.minimal)
  }

  def toJsonObjectWithoutNulls[T](v: T)(implicit serializer: Serializer[T]): JsonObject = {
    toJsonObject(v, JSONSerializerOptions.defaultWithoutNulls)
  }

  def toJsonObject[T](v: T)(implicit serializer: Serializer[T]): JsonObject = {
    toJsonObject(v, JSONSerializerOptions.default)
  }

  def toJsonObject[T](v: T, options: JSONSerializerOptions)(implicit serializer: Serializer[T]): JsonObject = {
    toJsonNode[T](v, options) match {
      case obj: JsonObject => obj
      case other => throw new IllegalArgumentException("Did not serialize to a JsonObject.  Got: "+other)
    }
  }

  def toMinimalJsonNode[T](v: T)(implicit serializer: Serializer[T]): JsonNode = {
    toJsonNode(v, JSONSerializerOptions.minimal)
  }

  def toJsonNodeWithoutNulls[T](v: T)(implicit serializer: Serializer[T]): JsonNode = {
    toJsonNode(v, JSONSerializerOptions.defaultWithoutNulls)
  }

  def toJsonNode[T](v: T)(implicit serializer: Serializer[T]): JsonNode = {
    toJsonNode(v, JSONSerializerOptions.default)
  }

  def toJsonNode[T](v: T, options: JSONSerializerOptions)(implicit serializer: Serializer[T]): JsonNode = {
    val generator: JsonNodeGenerator = new JsonNodeGenerator
    write(v, new JsonGeneratorOutput(generator, options))
    generator.result()
  }

  def fromJsonNode[T](node: JsonNode)(implicit deserializer: Deserializer[T]): T = {
    fromJsonNode(node, JSONDeserializerOptions.default)
  }

  def fromJsonNode[T](node: JsonNode, options: JSONDeserializerOptions)(implicit deserializer: Deserializer[T]): T = {
    val parser: JsonNodeParser = new JsonNodeParser(node)
    read(new JsonParserInput(parser, options))
  }

  def toMinimalJSON[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, JSONSerializerOptions.minimal)
  }

  def toPrettyJSON[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, JSONSerializerOptions.pretty)
  }

  def toPrettyJSONWithoutNulls[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, JSONSerializerOptions.prettyWithoutNulls)
  }

  def toJSON[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, JSONSerializerOptions.default)
  }

  def toJSONWithoutNulls[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, JSONSerializerOptions.defaultWithoutNulls)
  }

  def toJSON[@specialized T](v: T, options: JSONSerializerOptions)(implicit serializer: Serializer[T]): String = {
    val sw: StringWriter = new StringWriter()
    val generator: JsonGenerator = jsonFactory.createGenerator(sw)
    if (options.prettyFormat) generator.setPrettyPrinter(jsonPrettyPrinter)
    write(v, new JsonGeneratorOutput(generator, options))
    generator.close()
    sw.toString
  }

  def fromJSON[@specialized T](json: String)(implicit deserializer: Deserializer[T]): T = {
    fromJSON(json, JSONDeserializerOptions.default)
  }

  def fromJSON[@specialized T](json: String, options: JSONDeserializerOptions)(implicit deserializer: Deserializer[T]): T = {
    val parser: JsonParser = jsonFactory.createParser(json)
    read(new JsonParserInput(parser, options))
  }

  def fromReader[@specialized T](reader: Reader)(implicit deserializer: Deserializer[T]): T = {
    fromReader(reader, JSONDeserializerOptions.default)
  }

  def fromReader[@specialized T](reader: Reader, options: JSONDeserializerOptions)(implicit deserializer: Deserializer[T]): T = {
    val parser: JsonParser = jsonFactory.createParser(reader)
    read(new JsonParserInput(parser, options))
  }

  def validate[T](node: JsonNode)(implicit deserializer: Deserializer[T]): ValidationResult = {
    validate[T](node, ValidationOptions.default)
  }

  def validate[T](node: JsonNode, options: ValidationOptions)(implicit deserializer: Deserializer[T]): ValidationResult = {
    validate[T](new JsonNodeParser(node), options)
  }

  def validate[T](json: String)(implicit deserializer: Deserializer[T]): ValidationResult = {
    validate[T](json, ValidationOptions.default)
  }

  def validate[T](json: String, options: ValidationOptions)(implicit deserializer: Deserializer[T]): ValidationResult = {
    validate(jsonFactory.createParser(json))
  }

  def validate[T](parser: JsonParser)(implicit deserializer: Deserializer[T]): ValidationResult = {
    validate[T](parser, ValidationOptions.default)
  }

  def validate[T](parser: JsonParser, options: ValidationOptions)(implicit deserializer: Deserializer[T]): ValidationResult = {
    Validation.validate(new JsonParserInput(parser, JSONDeserializerOptions.default), options)
  }

  private def read[@specialized T](input: JsonParserInput)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(input)
  }

  private def write[@specialized T](v: T, out: JsonGeneratorOutput)(implicit serializer: Serializer[T]): Unit = {
    serializer.serializeRaw(out, v)
  }
}