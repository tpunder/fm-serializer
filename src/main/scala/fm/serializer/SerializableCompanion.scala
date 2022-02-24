package fm.serializer

import fm.json.{JsonNode, JsonObject}
import fm.serializer.bson.BSON
import fm.serializer.jackson.Jackson
import fm.serializer.json.JSON
import fm.serializer.protobuf.Protobuf
import fm.serializer.validation.ValidationResult
import org.bson.{BsonDocument, RawBsonDocument}

/**
 * Usage Pattern:
 * 
 * import fm.serializer.{SerializableCompanion, SerializableInstance, SimpleSerializer}
 * 
 * object Foo extends SerializableCompanion[Foo] {
 *   protected val serializer: SimpleSerializer[Foo] = makeSerializer[Foo]
 * }
 * 
 * final case class Foo(bar: String) extends SerializableInstance[Foo] {
 *   protected def companion: SerializableCompanion[Foo] = Foo
 * }
 */
trait SerializableCompanion[A] extends SerializableCompanionBase {
  // Moved into SerializableCompanionBase to support both Scala 2 and 3
  //protected def makeSerializer[T](): SimpleObjectSerializer[T] = macro Macros.makeSimpleObjectSerializer[T]
  
  protected val serializer: SimpleSerializer[A]
  
  //
  // JSON Methods
  //
  def toJSON(v: A): String = JSON.toJSON(v)(serializer)
  def toJSONWithoutNulls(v: A): String = JSON.toJSONWithoutNulls(v)(serializer)
  def toMinimalJSON(v: A): String = JSON.toMinimalJSON(v)(serializer)
  def toPrettyJSON(v: A): String = JSON.toPrettyJSON(v)(serializer)
  def toPrettyJSONWithoutNulls(v: A): String = JSON.toPrettyJSONWithoutNulls(v)(serializer)
  
  def fromJSON(json: String): A = JSON.fromJSON(json)(serializer)
  def validateJson(json: String): ValidationResult = JSON.validate(json)(serializer)

  //
  // Jackson JsonNode Methods
  //
  def fromJsonNode(node: JsonNode): A = Jackson.fromJsonNode(node)(serializer)
  def toJsonNode(v: A): JsonNode = Jackson.toJsonNode(v)(serializer)
  def toJsonNodeWithoutNulls(v: A): JsonNode = Jackson.toJsonNodeWithoutNulls(v)(serializer)
  def validateJsonNode(node: JsonNode): ValidationResult = Jackson.validate(node)(serializer)

  //
  // Jackson JsonObject Methods
  //
  def fromJsonObject(node: JsonObject): A = Jackson.fromJsonNode(node)(serializer)
  def toJsonObject(v: A): JsonObject = Jackson.toJsonObject(v)(serializer)
  def toJsonObjectWithoutNulls(v: A): JsonObject = Jackson.toJsonObjectWithoutNulls(v)(serializer)
  def validateJsonObject(node: JsonObject): ValidationResult = Jackson.validate(node)(serializer)

  //
  // Protobuf Methods
  //
  def toBytes(v: A): Array[Byte] = Protobuf.toBytes(v)(serializer)  
  def fromBytes(bytes: Array[Byte]): A = Protobuf.fromBytes(bytes)(serializer)

  //
  // BSON Methods
  //
  def toBsonBytes(v: A): Array[Byte] = BSON.toBsonBytes(v)(serializer)
  def fromBsonBytes(bytes: Array[Byte]): A = BSON.fromBsonBytes(bytes)(serializer)

  def toRawBsonDocument(v: A): RawBsonDocument = BSON.toRawBsonDocument(v)(serializer)
  def toBsonDocument(v: A): BsonDocument = BSON.toBsonDocument(v)(serializer)
  def addToBsonDocument(v: A, doc: BsonDocument): Unit = BSON.addToBsonDocument(v, doc)(serializer)

  def fromBsonDocument(doc: BsonDocument): A = BSON.fromBsonDocument(doc)(serializer)
}

/**
 * Usage Pattern:
 * 
 * import fm.serializer.{SerializableCompanion, SerializableInstance, SimpleSerializer}
 * 
 * object Foo extends SerializableCompanion[Foo] {
 *   protected val serializer: SimpleSerializer[Foo] = makeSerializer[Foo]
 * }
 * 
 * final case class Foo(bar: String) extends SerializableInstance[Foo] {
 *   protected def companion: SerializableCompanion[Foo] = Foo
 * }
 */
trait SerializableInstance[A] { self: A =>
  protected def companion: SerializableCompanion[A]
  
  def toJSON: String = companion.toJSON(this)
  def toJSONWithoutNulls: String = companion.toJSONWithoutNulls(this)
  def toMinimalJSON: String = companion.toMinimalJSON(this)
  def toPrettyJSON: String = companion.toPrettyJSON(this)
  def toPrettyJSONWithoutNulls: String = companion.toPrettyJSONWithoutNulls(this)

  def toJsonObject: JsonObject = companion.toJsonObject(this)
  def toJsonObjectWithoutNulls: JsonObject = companion.toJsonObjectWithoutNulls(this)

  def toBytes: Array[Byte] = companion.toBytes(this)

  def toRawBsonDocument: RawBsonDocument = companion.toRawBsonDocument(this)
  def toBsonDocument: BsonDocument = companion.toBsonDocument(this)
  def addToBsonDocument(doc: BsonDocument): Unit = companion.addToBsonDocument(this, doc)
}
