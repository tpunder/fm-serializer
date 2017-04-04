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
package fm.serializer.protobuf

import java.io.{InputStream, OutputStream}
import fm.serializer.{Deserializer, Serializer}
import fm.serializer.base64.Base64

object Protobuf {

//  val pool = new java.util.concurrent.ConcurrentLinkedQueue[FastByteArrayOutputStream]()
//  
//  def take(): FastByteArrayOutputStream = {
//    val os = pool.poll()
//    if (null == os) new FastByteArrayOutputStream(512) else os
//  }
//  
//  def release(os: FastByteArrayOutputStream): Unit = {
//    os.reset()
//    os.trimTo(512)
//    pool.add(os)
//  }
//  
//  def toBytes[@specialized T](v: T)(implicit serializer: Serializer[T]): Array[Byte] = {
//    val os = take()
//    toOutputStream(os, v)
//    val res = os.toByteArray
//    release(os)
//    res
//  }
  
  private val protobufOutput: ThreadLocal[ProtobufOutput] = new ThreadLocal[ProtobufOutput]{
    override protected def initialValue: ProtobufOutput = new ProtobufOutput()
  }
  
  def toBytes[@specialized T](v: T)(implicit serializer: Serializer[T]): Array[Byte] = {
    val out: ProtobufOutput = protobufOutput.get
    serializer.serializeRaw(out, v)
    val ret: Array[Byte] = out.toByteArray
    out.reset()
    ret
  }
  
//  private val fmByteArrayOutputStream: ThreadLocal[FMByteArrayOutputStream] = new ThreadLocal[FMByteArrayOutputStream]{
//    override protected def initialValue: FMByteArrayOutputStream = new FMByteArrayOutputStream()
//  }
//  
//  def toBytes[@specialized T](v: T)(implicit serializer: Serializer[T]): Array[Byte] = {
//    val os: FMByteArrayOutputStream = fmByteArrayOutputStream.get
//    serializer.serializeRaw(new ProtobufOutput(os), v)
//    val ret: Array[Byte] = os.toByteArray
//    os.reset()
//    ret
//  }
  
//  def toBytes[@specialized T](v: T)(implicit serializer: Serializer[T]): Array[Byte] = {
//    val os = new FastByteArrayOutputStream()
//    serializer.serializeRaw(new ProtobufOutputStreamOutput(os), v)
//    os.trim
//    os.array
//  }
  
  def toOutputStream[@specialized T](os: OutputStream, v: T)(implicit serializer: Serializer[T]): Unit = {
    serializer.serializeRaw(new ProtobufOutputStreamOutput(os), v)
  }
  
  def fromBytes[@specialized T](bytes: Array[Byte])(implicit deserializer: Deserializer[T]): T = {
    fromBytes(bytes, ProtobufOptions.default)
  }
  
  def fromBytes[@specialized T](bytes: Array[Byte], options: ProtobufOptions)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new ProtobufByteArrayInput(bytes, options))
  }
  
  def fromInputStream[@specialized T](is: InputStream)(implicit deserializer: Deserializer[T]): T = {
    fromInputStream(is, ProtobufOptions.default)
  }
  
  def fromInputStream[@specialized T](is: InputStream, options: ProtobufOptions)(implicit deserializer: Deserializer[T]): T = {
    deserializer.deserializeRaw(new ProtobufInputStreamInput(is, options))
  }
  
  def toBase64String[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    Base64.encodeBytes(toBytes[T](v))
  }
  
  def fromBase64String[@specialized T](str: String)(implicit deserializer: Deserializer[T]): T = {
    fromBase64String(str, ProtobufOptions.default)
  }
  
  def fromBase64String[@specialized T](str: String, options: ProtobufOptions)(implicit deserializer: Deserializer[T]): T = {
    fromBytes[T](Base64.decode(str), options)
  }
  
  def toBase64URLSafeString[@specialized T](v: T)(implicit serializer: Serializer[T]): String = {
    Base64.encodeBytes(toBytes[T](v), Base64.URL_SAFE)
  }
  
  def fromBase64URLSafeString[@specialized T](str: String)(implicit deserializer: Deserializer[T]): T = {
    fromBase64URLSafeString(str, ProtobufOptions.default)
  }
  
  def fromBase64URLSafeString[@specialized T](str: String, options: ProtobufOptions)(implicit deserializer: Deserializer[T]): T = {
    fromBytes[T](Base64.decode(str, Base64.URL_SAFE), options)
  }
}
