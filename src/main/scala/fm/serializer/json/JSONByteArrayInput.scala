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

final class JSONByteArrayInput(bytes: Array[Byte], options: JSONOptions) extends JSONInput(options) {
  private[this] var length: Int = bytes.length
  private[this] var idx: Int = 0
  
  /** Peek at the next character without consuming it */
  protected var peek: Int = if (bytes.length == 0) -1 else readChar()
  
  /** Return peek and advance to the next character */
  protected def next: Char = {
    require(peek != -1, "EOF")
    val ch: Char = peek.toChar
    peek = readChar()
    ch
  }
  
  private def readChar(): Int = {
    if (idx < length) {
      val ch: Int = bytes(idx)
      idx += 1
      ch
    } else {
      -1
    }
  }
}