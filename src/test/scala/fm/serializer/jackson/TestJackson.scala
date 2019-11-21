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

import fm.json.{Json, JsonNode}
import fm.serializer.{Deserializer, Serializer}
import fm.serializer.json.{JSON, JSONOptions, TestJSON}

final class TestJackson extends TestJSON {
  import fm.serializer.TestSerializer.Foo

  override protected def allowsUnquotedStringValues: Boolean = false

  def serialize[T](v: T)(implicit ser: Serializer[T]): String = Jackson.toJSON[T](v)
  def deserialize[T](json: String)(implicit deser: Deserializer[T]): T = Jackson.fromJSON[T](json)
  def makeInput(json: String): JsonParserInput = new JsonParserInput(Json.jsonFactory.createParser(json), JSONOptions.default)

  test("JsonNode") {
    val foo: Foo = Foo()

    val jsonNode: JsonNode = Jackson.toJsonNode(foo)

    // Serialized JsonNode should match Jackson.toJSON/toPrettyJSON
    jsonNode.toCompactJson() shouldBe Jackson.toJSON(foo)
    jsonNode.toPrettyJson() shouldBe Jackson.toPrettyJSON(foo)

    // Should be able to deserialize back to a Foo instance
    Jackson.fromJsonNode[Foo](jsonNode) shouldBe foo

    // These do not work due to parsing of floating points into BigDecimal instead of Floats by default.
//    JsonNode.parse(jsonNode.toCompactJson()) shouldBe jsonNode
//    JsonNode.parse(jsonNode.toPrettyJson()) shouldBe jsonNode
  }

  test("fm.serializer.jackson <=> fm.serializer.json") {
    val foo: Foo = Foo()

    // These should match
    Jackson.toJSON(foo) shouldBe JSON.toJSON(foo)

    // The normal toJSON should match the "compact" representation
    Jackson.toJSON(foo) shouldBe Json.toCompactJsonString(Jackson.toPrettyJSON(foo))
    JSON.toJSON(foo) shouldBe Json.toCompactJsonString(JSON.toPrettyJSON(foo))

    // Both fm.serializer.jackson and fm.serializer.json should serialize to the same compact string
    Json.toCompactJsonString(Jackson.toPrettyJSON(foo)) shouldBe Json.toCompactJsonString(JSON.toPrettyJSON(foo))
  }
}
