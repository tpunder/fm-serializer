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
package fm.serializer

import java.io.OutputStream
import java.util.concurrent.ConcurrentLinkedQueue

object LinkedByteArrayOutputStream {
  
}

object Buffer {
  val DefaultBufferSize: Int = 512
  
  private val pool: ConcurrentLinkedQueue[Buffer] = new ConcurrentLinkedQueue()
  
  /**
   * Take a Buffer from the Pool (or create a new one if the pool is empty)
   */
  def takeFromPool(): Buffer = {
    val buf: Buffer = pool.poll()
    if (null == buf) new Buffer(new Array[Byte](DefaultBufferSize), 0, 0, readOnly = false, fromPool = true) else buf 
  }
  
  def allocate(size: Int): Buffer = new Buffer(new Array(size))
  
  def wrap(bytes: Array[Byte]): Buffer = new Buffer(bytes, 0, bytes.length, readOnly = true)
  
  def wrap(bytes: Array[Byte], start: Int, length: Int): Buffer = new Buffer(bytes, start, start + length, readOnly = true)
}

final class Buffer private (val buffer: Array[Byte], val start: Int = 0, var offset: Int = 0, readOnly: Boolean = false, fromPool: Boolean = false) {
  /** Return this Buffer to the pool if it originally came from the pool */
  def release(): Unit = {
    if (fromPool) {
      offset = 0
      Buffer.pool.add(this)
    }
  }
  
  /** Available space left in this buffer for writing */
  def available: Int = if (readOnly) 0 else buffer.length - offset
  
  def size: Int = offset - start
}



final class LinkedByteArrayOutputStream() extends OutputStream {
  
  /** Our buffers that we will be writing into */
  private var buffers: Vector[Buffer] = Vector.empty
  
  /** The current buffer we are working with (the last buffer in the buffers Vector) */
  private var current: Buffer = null
  
  private var _size: Int = 0
  
  /** The total number of bytes we've written */
  final def size: Int = _size
  final def length: Int = _size
  
  /**
   * Append another LinkedByteArrayOutputStream onto this one.
   * Note: The other LinkedByteArrayOutputStream is empty
   */
  final def += (other: LinkedByteArrayOutputStream): Unit = {
    _size += other._size
    buffers = buffers ++ other.buffers
    current = other.current
    
    other.clear()
  }
  
  final def release(): this.type = {
    buffers.foreach{ _.release() }
    this
  }
  
  final def clear(): this.type = {
    buffers = Vector.empty
    current = null
    _size = 0
    this
  }
  
  private def isEmpty: Boolean = null == current
  
  //private def available: Int = if (null == current) 0 else current.available
  
  private def grow(): Unit = {
    current = Buffer.takeFromPool()
    buffers = buffers :+ current
  }

  final def write(b: Int): Unit = {
    if (isEmpty || current.available == 0) grow()
    current.buffer(current.offset) = b.toByte
    current.offset += 1
    _size += 1
  }
  
  final override def write(b: Array[Byte]): Unit = write(b, 0, b.length)
  
  final override def write(b: Array[Byte], origOff: Int, origLen: Int): Unit = {    
    _size += origLen
    
    if (origLen > Buffer.DefaultBufferSize) {
      current = Buffer.wrap(b, origOff, origLen)
      buffers = buffers :+ current
      return
    }
    
    var off: Int = origOff
    var len: Int = origLen
    
    while (len > 0) {
      if (isEmpty || current.available == 0) grow()
      val toWrite: Int = math.min(len, current.available)
      System.arraycopy(b, off, current.buffer, current.offset, toWrite)
      current.offset += toWrite
      off += toWrite
      len -= toWrite
    }
  }
  
  final def toByteArray(): Array[Byte] = {    
    val byteArray: Array[Byte] = new Array[Byte](size)
    var offset: Int = 0
    
    buffers.foreach{ buf: Buffer =>
      val len: Int = buf.offset - buf.start
      if (len > 0) {
        System.arraycopy(buf.buffer, buf.start, byteArray, offset, len)
        offset += len
      }
    }
      
    byteArray
  }
}