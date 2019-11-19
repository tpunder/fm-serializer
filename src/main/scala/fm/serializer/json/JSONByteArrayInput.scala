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

import fm.common.Implicits._

object JSONByteArrayInput {
  private def countLeadingBitsInByte(v: Int): Int = {
    var count: Int = 0
    var mask: Int = 0x80

    while (count < 8 && (mask & v) == mask) {
      mask = mask >> 1
      count += 1
    }

    count
  }
}

final class JSONByteArrayInput(bytes: Array[Byte], options: JSONOptions) extends JSONInput(options) {
  private[this] val length: Int = bytes.length
  private[this] var idx: Int = 0

  // ScalaTest needs access to this so it is not a private[this]
  private var peekCodePoint: Int = -1
  private[this] var nextIsLowSurrogate: Boolean = false

  /** Peek at the next character without consuming it */
  protected var peek: Int = readChar()

  /** Return peek and advance to the next character */
  protected def next: Char = {
    if (peek === -1) throw new IllegalArgumentException("EOF")
    val ch: Char = peek.toChar
    peek = readChar()
    ch
  }

  private def readChar(): Int = {
    if (nextIsLowSurrogate) {
      nextIsLowSurrogate = false
      Character.lowSurrogate(peekCodePoint)
    } else {
      peekCodePoint = readCodePoint()

      if (Character.isSupplementaryCodePoint(peekCodePoint)) {
        nextIsLowSurrogate = true
        Character.highSurrogate(peekCodePoint)
      } else {
        peekCodePoint
      }
    }
  }

  private def readCodePoint(): Int = {
    val firstByte: Int = readByte()
    if (firstByte === -1) return -1

    val count: Int = JSONByteArrayInput.countLeadingBitsInByte(firstByte)

    if (count > 6) throw new IllegalArgumentException("Invalid UTF-8 Encoding for first byte: "+firstByte.toByte)

    var codepoint: Int = (count: @scala.annotation.switch) match {
      case 0 => firstByte // 0xxxxxxx
      case 1 => throw new IllegalArgumentException(s"Invalid UTF-8 Encoding.  Invalid first byte: $firstByte")
      case 2 => firstByte & 0x1F // 110xxxxx
      case 3 => firstByte & 0x0F // 1110xxxx
      case 4 => firstByte & 0x07 // 11110xxx
      case 5 => firstByte & 0x03 // 111110xx
      case 6 => firstByte & 0x01 // 1111110x
      case _ => throw new IllegalArgumentException(s"Invalid UTF-8 Encoding.  Invalid first byte: $firstByte")
    }

    var i: Int = 1 // First byte is already taken care of
    while (i < count) {
      val next: Int = readByte()
      // EOF check and 10xxxxxx mask check
      if (next === -1 || (next & 0xC0) =!= 0x80) throw new IllegalArgumentException(s"Invalid UTF-8 Encoding.  Expecting $count bytes for first byte: $firstByte")
      codepoint = (codepoint << 6) | (next & 0x3F)
      i += 1
    }

    codepoint
  }

  private def readByte(): Int = {
    if (idx < length) {
      val ch: Int = bytes(idx)
      idx += 1
      ch & 0xff
    } else {
      -1
    }
  }

}