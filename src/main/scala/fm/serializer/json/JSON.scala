/*
 * Copyright 2014 Frugal Mechanic (http://frugalmechanic.com)
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
package fm.serializer.json

import fm.serializer.validation.{Validation, ValidationOptions, ValidationResult}
import fm.serializer.{Deserializer, Serializer}
import java.io.Reader
import java.nio.charset.StandardCharsets.UTF_8

object JSON {
  private[this] val defaultJSONOutput: ThreadLocal[JSONOutput] = makeThreadLocal(JSONSerializerOptions.default)
  private[this] val defaultWithoutNullsJSONOutput: ThreadLocal[JSONOutput] = makeThreadLocal(JSONSerializerOptions.defaultWithoutNulls)
  private[this] val minimalJSONOutput: ThreadLocal[JSONOutput] = makeThreadLocal(JSONSerializerOptions.minimal)
  private[this] val prettyJSONOutput: ThreadLocal[JSONOutput] = makeThreadLocal(JSONSerializerOptions.pretty)
  private[this] val prettyWithoutNullsJSONOutput: ThreadLocal[JSONOutput] = makeThreadLocal(JSONSerializerOptions.prettyWithoutNulls)

  private def makeThreadLocal(options: JSONSerializerOptions): ThreadLocal[JSONOutput] =  new ThreadLocal[JSONOutput]{
    override protected def initialValue: JSONOutput = new JSONOutput(options)
  }
  
  def toJSON[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, defaultJSONOutput.get)
  }

  def toJSONWithoutNulls[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, defaultWithoutNullsJSONOutput.get)
  }
  
  def toMinimalJSON[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, minimalJSONOutput.get)
  }
  
  def toPrettyJSON[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, prettyJSONOutput.get)
  }

  def toPrettyJSONWithoutNulls[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    toJSON(v, prettyWithoutNullsJSONOutput.get)
  }

  def toJSON[@specialized T](v: T, out: JSONOutput)(implicit serializer: Serializer[T]): String = {
    val bytes: Array[Byte] = toBytes[T](v, out)(serializer)
    new String(bytes, UTF_8)
  }
  
  def toBytes[@specialized T](v: T)(implicit serializer: Serializer[T]): Array[Byte] = {
    toBytes(v, defaultJSONOutput.get)
  }
  
  def toBytes[@specialized T](v: T, out: JSONOutput)(implicit serializer: Serializer[T]): Array[Byte] = {
    serializer.serializeRaw(out, v)
    val ret: Array[Byte] = out.toByteArray
    out.reset()
    ret
  }
  
  def fromJSON[@specialized T](json: String)(implicit deserializer: Deserializer[T]): T = {
    fromJSON(json, JSONDeserializerOptions.default)
  }
  
  def fromJSON[@specialized T](json: String, options: JSONDeserializerOptions)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new JSONCharSequenceInput(json, options))
  }
  
  def fromBytes[@specialized T](bytes: Array[Byte])(implicit deserializer: Deserializer[T]): T = {
    fromBytes(bytes, JSONDeserializerOptions.default)
  }
  
  def fromBytes[@specialized T](bytes: Array[Byte], options: JSONDeserializerOptions)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new JSONByteArrayInput(bytes, options))
  } 
  
  def fromReader[@specialized T](reader: Reader)(implicit deserializer: Deserializer[T]): T = {
    fromReader(reader, JSONDeserializerOptions.default)
  }
  
  def fromReader[@specialized T](reader: Reader, options: JSONDeserializerOptions)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new JSONReaderInput(reader, options))
  }

  def validate[T](json: String)(implicit deserializer: Deserializer[T]): ValidationResult = {
    validate[T](json, ValidationOptions.default)
  }

  def validate[T](json: String, options: ValidationOptions)(implicit deserializer: Deserializer[T]): ValidationResult = {
    Validation.validate(new JSONCharSequenceInput(json, JSONDeserializerOptions.default), options)
  }
}