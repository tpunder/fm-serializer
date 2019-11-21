package fm.serializer

import fm.json.JsonNode
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
trait SerializableCompanion[A] {
  protected def makeSerializer[T](): SimpleObjectSerializer[T] = macro Macros.makeSimpleObjectSerializer[T]
  
  protected val serializer: SimpleSerializer[A]
  
  //
  // JSON Methods
  //
  def toJSON(v: A): String = JSON.toJSON(v)(serializer)
  def toMinimalJSON(v: A): String = JSON.toMinimalJSON(v)(serializer)
  def toPrettyJSON(v: A): String = JSON.toPrettyJSON(v)(serializer)
  
  def fromJSON(json: String): A = JSON.fromJSON(json)(serializer)
  def validateJson(json: String): ValidationResult = JSON.validate(json)(serializer)

  //
  // Jackson Methods
  //
  def fromJsonNode(node: JsonNode): A = Jackson.fromJsonNode(node)(serializer)
  def toJsonNode(v: A): JsonNode = Jackson.toJsonNode(v)(serializer)
  def validateJsonNode(node: JsonNode): ValidationResult = Jackson.validate(node)(serializer)

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
  def toMinimalJSON: String = companion.toMinimalJSON(this)
  def toPrettyJSON: String = companion.toPrettyJSON(this)

  def toJsonNode: JsonNode = companion.toJsonNode(this)

  def toBytes: Array[Byte] = companion.toBytes(this)

  def toRawBsonDocument: RawBsonDocument = companion.toRawBsonDocument(this)
  def toBsonDocument: BsonDocument = companion.toBsonDocument(this)
  def addToBsonDocument(doc: BsonDocument): Unit = companion.addToBsonDocument(this, doc)
}
