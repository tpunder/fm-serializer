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

import fm.serializer.{FieldOutput, NestedOutput, Output}
import fm.serializer.FMByteArrayOutputStream
import fm.serializer.base64.Base64
import java.nio.charset.StandardCharsets.UTF_8

final class JSONOutput(outputNulls: Boolean = true) extends Output {
  import JSONOutput._
  
  def allowStringMap: Boolean = true
  
  private[this] val out: FMByteArrayOutputStream = new FMByteArrayOutputStream()
  
  private[this] var inObjectOrArray: Boolean = false
  private[this] var isFirst: Boolean = false
  
  def toByteArray: Array[Byte] = out.toByteArray
  def reset(): Unit = out.reset()
  
  private def doComma(): Unit = {
    if (inObjectOrArray) {
      if (isFirst) isFirst = false
      else out.write(',')
    }
  }
  
  @inline private def withCommas(f: => Unit): Unit = {
    val prevInObjeftOrArray: Boolean = inObjectOrArray
    val prevIsFirst: Boolean = isFirst
    
    inObjectOrArray = true
    isFirst = true
    
    f
    
    inObjectOrArray = prevInObjeftOrArray
    isFirst = prevIsFirst
  }
  
  //
  // RAW Output
  //
  
  // Basic Types
  def writeRawBool(value: Boolean): Unit = out.writeASCII(if (value) "true" else "false")
  def writeRawFloat(value: Float): Unit = out.appendFloatAsString(value)
  def writeRawDouble(value: Double): Unit = out.appendDoubleAsString(value)
  
  private def isSimpleChar(ch: Char): Boolean = {
    // Any ASCII printable char except quote and slashes
    ch > 31 && ch < 127 && ch != '"' && ch != '\\' && ch != '/'
  }
  
  def writeRawString(value: String): Unit = {
    if (null == value) {
      writeNull()
      return
    }
        
    out.write('"')
    
    val len: Int = value.length
    var i: Int = 0
    
    while(i < len) {
      val start: Int = i
      
      // While there are characters that don't need special handling
      while(i < len && isSimpleChar(value.charAt(i))) i += 1
      
      // Bulk write them
      if (i > start) out.write(value, start, i - start)
      
      if (i < len) {
        appendSpecialChar(value.charAt(i))
        i += 1
      }
      
    }
    
    out.write('"')
  }
  
  private def appendSpecialChar(ch: Char): Unit = ch match {
    case '"'  => out.writeASCII("\\\"")
    case '\\' => out.writeASCII("\\\\")
    case '/'  => out.writeASCII("\\/")
    case '\b' => out.writeASCII("\\b")
    case '\f' => out.writeASCII("\\f")
    case '\n' => out.writeASCII("\\n")
    case '\r' => out.writeASCII("\\r")
    case '\t' => out.writeASCII("\\t")
      case _ =>
        if (Character.isISOControl(ch)) {
          // Unicode escaped character
          out.writeASCII("\\u")
          val hex: String = Integer.toHexString(ch)
          
          // Pad with leading zeroes
          var count: Int = hex.length
          while(count < 4) {
            out.write('0')
            count += 1
          }
          
          out.writeASCII(hex)
        } else {
          // Normal Character
          out.append(ch)
        }
    }
  
  def writeRawByteArray(value: Array[Byte]): Unit = {
    if (null == value) {
      writeNull()
      return
    }
    
    writeRawString(Base64.encodeBytes(value))
  }
  
  // Ints
  def writeRawInt(value: Int): Unit = out.appendIntAsString(value)
  
  def writeRawUnsignedInt(value: Int): Unit = writeRawInt(value)
  def writeRawSignedInt(value: Int): Unit = writeRawInt(value)
  def writeRawFixedInt(value: Int): Unit = writeRawInt(value)
  
  // Longs
  def writeRawLong(value: Long): Unit = out.appendLongAsString(value)
  
  def writeRawUnsignedLong(value: Long): Unit = writeRawLong(value)
  def writeRawSignedLong(value: Long): Unit = writeRawLong(value)
  def writeRawFixedLong(value: Long): Unit = writeRawLong(value)
  
  // Objects
  def writeRawObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    if (null == obj) {
      writeNull() 
    } else {
      out.write('{')
      withCommas{ f(this, obj) }
      out.write('}')
    }
  }

  // Collections
  def writeRawCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null == col) {
      writeNull()
    } else {
      out.write('[')
      withCommas{ f(this, col) }
      out.write(']')
    }
  }
  
  //
  // NESTED Output - Same as RAW Output for JSON
  //

  // Basic Types
  def writeNestedBool(value: Boolean): Unit = {
    doComma()
    writeRawBool(value)
  }
  
  def writeNestedFloat(value: Float): Unit = {
    doComma()
    writeRawFloat(value)
  }
  
  def writeNestedDouble(value: Double): Unit = {
    doComma()
    writeRawDouble(value)
  }
  
  def writeNestedString(value: String): Unit = {
    doComma()
    writeRawString(value)
  }
  
  // Bytes
  def writeNestedByteArray(value: Array[Byte]): Unit = {
    doComma()
    writeRawByteArray(value)
  }
  
  // Ints  
  def writeNestedInt(value: Int): Unit = {
    doComma()
    writeRawInt(value)
  }
  
  def writeNestedUnsignedInt(value: Int): Unit = writeNestedInt(value)
  def writeNestedSignedInt(value: Int): Unit = writeNestedInt(value)
  def writeNestedFixedInt(value: Int): Unit = writeNestedInt(value)
  
  // Longs
  def writeNestedLong(value: Long): Unit = {
    doComma()
    writeRawLong(value)
  }
  
  def writeNestedUnsignedLong(value: Long): Unit = writeNestedLong(value)
  def writeNestedSignedLong(value: Long): Unit = writeNestedLong(value)
  def writeNestedFixedLong(value: Long): Unit = writeNestedLong(value)
  
  def writeNestedObject[T](obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    doComma()
    writeRawObject(obj)(f)
  }
  
  def writeNestedCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = {
    doComma()
    writeRawCollection(col)(f)
  }
  
  //
  // FIELD Output
  //
  
  // Basic Types
  def writeFieldBool(number: Int, name: String, value: Boolean): Unit = {
    doComma()
    writeFieldName(name)
    writeRawBool(value)
  }
  
  def writeFieldFloat(number: Int, name: String, value: Float): Unit = {
    doComma()
    writeFieldName(name)
    writeRawFloat(value)
  }
  
  def writeFieldDouble(number: Int, name: String, value: Double): Unit = {
    doComma()
    writeFieldName(name)
    writeRawDouble(value)
  }
  
  def writeFieldString(number: Int, name: String, value: String): Unit = {
    if (null != value || outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawString(value)
    }
  }
  
  // Bytes
  def writeFieldByteArray(number: Int, name: String, value: Array[Byte]): Unit = {
    if (null != value || outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawByteArray(value)
    }
  }
  
  // Ints  
  def writeFieldInt(number: Int, name: String, value: Int): Unit = {
    doComma()
    writeFieldName(name)
    writeRawInt(value)
  }
  
  def writeFieldUnsignedInt(number: Int, name: String, value: Int): Unit = writeFieldInt(number, name, value)
  def writeFieldSignedInt(number: Int, name: String, value: Int): Unit = writeFieldInt(number, name, value)
  def writeFieldFixedInt(number: Int, name: String, value: Int): Unit = writeFieldInt(number, name, value)
  
  // Longs
  def writeFieldLong(number: Int, name: String, value: Long): Unit = {
    doComma()
    writeFieldName(name)
    writeRawLong(value)
  }
  
  def writeFieldUnsignedLong(number: Int, name: String, value: Long): Unit = writeFieldLong(number, name, value)
  def writeFieldSignedLong(number: Int, name: String, value: Long): Unit = writeFieldLong(number, name, value)
  def writeFieldFixedLong(number: Int, name: String, value: Long): Unit = writeFieldLong(number, name, value)
  
  // Objects
  def writeFieldObject[T](number: Int, name: String, obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    if (null != obj || outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawObject(obj)(f)
    }
  }
  
  // Collections
  def writeFieldCollection[T](number: Int, name: String, col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null != col || outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawCollection(col)(f)
    }
  }
  
  def writeFieldNull(number: Int, name: String): Unit = {
    doComma()
    writeFieldName(name)
    writeNull()
  }
  
  private def writeFieldName(name: String): Unit = {
    writeRawString(name)
    out.write(':')
  }
  
  private def writeNull(): Unit = {
    out.writeASCII("null")
  }
}