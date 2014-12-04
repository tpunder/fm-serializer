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

import fm.serializer.{Deserializer, Serializer}
import java.io.{ByteArrayInputStream, InputStreamReader, OutputStreamWriter, Reader, StringReader}
import java.nio.charset.StandardCharsets.UTF_8
import fm.serializer.fastutil.FastByteArrayOutputStream

object JSON {
  private[this] val jsonOutput: ThreadLocal[JSONOutput] = new ThreadLocal[JSONOutput]{
    override protected def initialValue: JSONOutput = new JSONOutput()
  }
  
  def toJSON[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    val bytes: Array[Byte] = toBytes[T](v)(serializer)
    new String(bytes, UTF_8)
  }
  
  def toPrettyJSON[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    val out: JSONOutput =  new JSONOutput(outputNulls = true, prettyFormat = true)
    serializer.serializeRaw(out, v)
    val bytes: Array[Byte] = out.toByteArray
    new String(bytes, UTF_8)
  }

  def toBytes[@specialized T](v: T)(implicit serializer: Serializer[T]): Array[Byte] = {
    val out: JSONOutput = jsonOutput.get
    serializer.serializeRaw(out, v)
    val ret: Array[Byte] = out.toByteArray
    out.reset()
    ret
  }
  
//  def toBytes[@specialized T](v: T)(implicit serializer: Serializer[T]): Array[Byte] = {
//    val os = new FastByteArrayOutputStream
//    val writer = new OutputStreamWriter(os, "UTF-8")
//    toAppendable(writer, v)
//    writer.flush()
//    os.trim()
//    os.array
//  }
  
//  def toAppendable[@specialized T](out: Appendable, v: T)(implicit serializer: Serializer[T]): Unit = {
//    serializer.serializeRaw(new JSONOutput(out), v)
//  }
  
  def fromJSON[@specialized T](json: String)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new JSONCharSequenceInput(json))
  }
  
  def fromBytes[@specialized T](bytes: Array[Byte])(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new JSONByteArrayInput(bytes))
  } 
  
//  def fromBytes[@specialized T](bytes: Array[Byte])(implicit deserializer: Deserializer[T]): T = {
//    fromReader(new InputStreamReader(new ByteArrayInputStream(bytes)))
//  } 
  
  def fromReader[@specialized T](reader: Reader)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new JSONReaderInput(reader))
  }
}