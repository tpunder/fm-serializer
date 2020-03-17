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
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}

final class JSONOutput(options: JSONSerializerOptions) extends Output {
  def allowStringMap: Boolean = true
  
  private[this] val out: FMByteArrayOutputStream = new FMByteArrayOutputStream()
  private[this] var level: Int = 0
  
  private[this] var inObjectOrArray: Boolean = false
  private[this] var isFirst: Boolean = false
  private[this] var objectOrArrayElemCount: Int = 0
  
  def toByteArray: Array[Byte] = out.toByteArray
  def reset(): Unit = out.reset()
  
  private def doIndent(): Unit = {
    out.write("\n")

    if (level > 0) {
      var i: Int = 0
      while (i < level) {
        out.write(options.indent)
        i += 1
      }
    }
  }

  private def doComma(): Unit = {
    if (inObjectOrArray) {
      objectOrArrayElemCount += 1
      
      if (isFirst) isFirst = false
      else out.write(',')
      
      if (options.prettyFormat) doIndent()
    }
  }
  
  @inline private def withCommas(f: => Unit): Int = {
    val prevInObjeftOrArray: Boolean = inObjectOrArray
    val prevIsFirst: Boolean = isFirst
    val prevObjectOrArrayElemCount: Int = objectOrArrayElemCount
    
    inObjectOrArray = true
    isFirst = true
    objectOrArrayElemCount = 0
    
    f
    
    val count: Int = objectOrArrayElemCount
    
    inObjectOrArray = prevInObjeftOrArray
    isFirst = prevIsFirst
    objectOrArrayElemCount = prevObjectOrArrayElemCount
    
    count
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

  def writeRawBigInteger(value: JavaBigInteger): Unit = {
    if (null == value) writeNull()
    else out.writeASCII(value.toString)
  }

  def writeRawBigDecimal(value: JavaBigDecimal): Unit = {
    if (null == value) writeNull()
    else out.writeASCII(value.toString)
  }
  
  def writeRawString(value: String): Unit = {
    if (null == value) {
      writeNull()
      return
    }
        
    out.write('"')
    
    val len: Int = value.length
    var i: Int = 0
    
    while (i < len) {
      val start: Int = i
      
      // While there are characters that don't need special handling
      while (i < len && isSimpleChar(value.charAt(i))) i += 1
      
      // Bulk write them
      if (i > start) out.write(value, start, i - start)
      
      if (i < len) {
        val ch: Char = value.charAt(i)

        // Need to handle Supplementary characters: http://www.oracle.com/us/technologies/java/supplementary-142654.html
        val codePoint: Int = if (Character.isSurrogate(ch) && i+1 < len) {
          i += 1 // Need to increment i to account for the second char
          Character.toCodePoint(ch, value.charAt(i)) // Note: i is incremented and is now reading the second char
        } else {
          ch.toInt
        }

        appendSpecialChar(codePoint)

        i += 1
      }
      
    }
    
    out.write('"')
  }

  // Note: ch should be the codepoint (which means supplementary chars must be collapsed before calling this)
  // http://www.oracle.com/us/technologies/java/supplementary-142654.html
  private def appendSpecialChar(ch: Int): Unit = ch match {
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
          while (count < 4) {
            out.write('0')
            count += 1
          }
          
          out.writeASCII(hex)
        } else {
          // Normal Character
          out.appendCodePoint(ch)
        }
    }
  
  def writeRawByteArray(value: Array[Byte]): Unit = {
    if (null == value) writeNull()
    else writeRawString(Base64.encodeBytes(value))
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
      level += 1
      val count: Int = withCommas{ f(this, obj) }
      level -= 1
      if (options.prettyFormat) {
        if (count > 0) doIndent() else out.write(' ')
      }
      out.write('}')
    }
  }

  // Collections
  def writeRawCollection[T](col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null == col) {
      writeNull()
    } else {
      out.write('[')
      level += 1
      val count: Int = withCommas{ f(this, col) }
      level -= 1
      if (options.prettyFormat) {
        if (count > 0) doIndent() else out.write(' ')
      }
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

  def writeNestedBigInteger(value: JavaBigInteger): Unit = {
    doComma()
    writeRawBigInteger(value)
  }

  def writeNestedBigDecimal(value: JavaBigDecimal): Unit = {
    doComma()
    writeRawBigDecimal(value)
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
    if (options.outputFalse || value) {
      doComma()
      writeFieldName(name)
      writeRawBool(value)
    }
  }
  
  def writeFieldFloat(number: Int, name: String, value: Float): Unit = {
    if (options.outputZeros || value != 0f) {
      doComma()
      writeFieldName(name)
      writeRawFloat(value)
    }
  }
  
  def writeFieldDouble(number: Int, name: String, value: Double): Unit = {
    if (options.outputZeros || value != 0d) {
      doComma()
      writeFieldName(name)
      writeRawDouble(value)
    }
  }

  def writeFieldBigInteger(number: Int, name: String, value: JavaBigInteger): Unit = {
    if (null != value || options.outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawBigInteger(value)
    }
  }

  def writeFieldBigDecimal(number: Int, name: String, value: JavaBigDecimal): Unit = {
    if (null != value || options.outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawBigDecimal(value)
    }
  }
  
  def writeFieldString(number: Int, name: String, value: String): Unit = {
    if (null != value || options.outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawString(value)
    }
  }
  
  // Bytes
  def writeFieldByteArray(number: Int, name: String, value: Array[Byte]): Unit = {
    if (null != value || options.outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawByteArray(value)
    }
  }
  
  // Ints  
  def writeFieldInt(number: Int, name: String, value: Int): Unit = {
    if (options.outputZeros || value != 0) {
      doComma()
      writeFieldName(name)
      writeRawInt(value)
    }
  }
  
  def writeFieldUnsignedInt(number: Int, name: String, value: Int): Unit = writeFieldInt(number, name, value)
  def writeFieldSignedInt(number: Int, name: String, value: Int): Unit = writeFieldInt(number, name, value)
  def writeFieldFixedInt(number: Int, name: String, value: Int): Unit = writeFieldInt(number, name, value)
  
  // Longs
  def writeFieldLong(number: Int, name: String, value: Long): Unit = {
    if (options.outputZeros || value != 0L) {
      doComma()
      writeFieldName(name)
      writeRawLong(value)
    }
  }
  
  def writeFieldUnsignedLong(number: Int, name: String, value: Long): Unit = writeFieldLong(number, name, value)
  def writeFieldSignedLong(number: Int, name: String, value: Long): Unit = writeFieldLong(number, name, value)
  def writeFieldFixedLong(number: Int, name: String, value: Long): Unit = writeFieldLong(number, name, value)
  
  // Objects
  def writeFieldObject[T](number: Int, name: String, obj: T)(f: (FieldOutput, T) => Unit): Unit = {
    if (null != obj || options.outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawObject(obj)(f)
    }
  }
  
  // Collections
  def writeFieldCollection[T](number: Int, name: String, col: T)(f: (NestedOutput, T) => Unit): Unit = {
    if (null != col || options.outputNulls) {
      doComma()
      writeFieldName(name)
      writeRawCollection(col)(f)
    }
  }
  
  def writeFieldNull(number: Int, name: String): Unit = {
    if (options.outputNulls) {
      doComma()
      writeFieldName(name)
      writeNull()
    }
  }
  
  private def writeFieldName(name: String): Unit = {
    writeRawString(name)
    out.write(':')
    if (options.prettyFormat) out.write(' ')
  }
  
  private def writeNull(): Unit = {
    out.writeASCII("null")
  }
}