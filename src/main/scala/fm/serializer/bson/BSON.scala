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

import fm.serializer.{Deserializer, Serializer}
import java.io.{StringWriter, Writer}
import org.bson._
import org.bson.io.BasicOutputBuffer
import org.bson.json.{JsonMode, JsonWriter, JsonWriterSettings}

// Note: None of the methods are specialized because you can't write raw BSON
//       values (they must be part of a document)
object BSON {

  private[this] val defaultShellJsonWriterSettings = new JsonWriterSettings(JsonMode.SHELL)
  private[this] val prettyShellJsonWriterSettings = new JsonWriterSettings(JsonMode.SHELL, "  ", "\n")

  private[this] val defaultStrictJsonWriterSettings = new JsonWriterSettings(JsonMode.STRICT)
  private[this] val prettyStrictJsonWriterSettings = new JsonWriterSettings(JsonMode.STRICT, "  ", "\n")


  def toShellJSON[T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSONString(v, defaultShellJsonWriterSettings)
  }

  def toPrettyShellJSON[T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSONString(v, prettyShellJsonWriterSettings)
  }

  def toJSON[T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSONString(v, defaultStrictJsonWriterSettings)
  }

  def toPrettyJSON[T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSONString(v, prettyStrictJsonWriterSettings)
  }

  private def toJSONString[T](v: T, settings: JsonWriterSettings)(implicit serializer: Serializer[T]): String = {
    val sw: StringWriter = new StringWriter()
    writeJSON(v, sw, settings)
    sw.toString()
  }

  private def writeJSON[T](v: T, writer: Writer, settings: JsonWriterSettings)(implicit serializer: Serializer[T]): Unit = {
    serializer.serializeRaw(new BSONOutput(new JsonWriter(writer, settings)), v)
  }

  def toBsonBytes[T](v: T)(implicit serializer: Serializer[T]): Array[Byte] = {
    val buf: BasicOutputBuffer = new BasicOutputBuffer()
    writeToBsonWriter(v, new BsonBinaryWriter(buf))
    buf.toByteArray
  }

  def toRawBsonDocument[T](v: T)(implicit serializer: Serializer[T]): RawBsonDocument = {
    new RawBsonDocument(toBsonBytes(v))
  }

  def toBsonDocument[T](v: T)(implicit serializer: Serializer[T]): BsonDocument = {
    val doc: BsonDocument = new BsonDocument()
    addToBsonDocument(v, doc)
    doc
  }

  def addToBsonDocument[T](v: T, doc: BsonDocument)(implicit serializer: Serializer[T]): Unit = {
    writeToBsonWriter(v, new BsonDocumentWriter(doc))
  }

  def writeToBsonWriter[T](v: T, writer: BsonWriter)(implicit serializer: Serializer[T]): Unit = {
    val out: BSONOutput = new BSONOutput(writer)
    serializer.serializeRaw(out, v)
  }

  def fromBsonBytes[T](bytes: Array[Byte])(implicit deserializer: Deserializer[T]): T = {
    fromBsonDocument(new RawBsonDocument(bytes))
  }

  def fromBsonDocument[T](doc: BsonDocument)(implicit deserializer: Deserializer[T]): T = {
    fromReader(new BsonDocumentReader(doc))
  }

  def fromReader[T](reader: BsonReader)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new BSONInput(reader))
  }
}
