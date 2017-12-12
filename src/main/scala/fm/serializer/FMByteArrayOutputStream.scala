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
import scala.annotation.elidable

object FMByteArrayOutputStream {
  
  val DefaultInitialArrayCapacity: Int = 128
  val DefaultBufferSize: Int = 1024
  val DefaultMinUsefulBufferSize: Int = 8
  val DefaultCompactThresholdSize: Int = 8
  val DefaultSpliceThresholdSize: Int = 8
  
  // We are only supporting up to 3-byte UTF-8 characters
  val MAX_UTF8_CHAR_BYTES: Int = 3
  
  private final class Pool(bufferSize: Int) {
    private[this] val pool: Array[Array[Byte]] = new Array(32)
    private[this] var count: Int = 0
    
    final def take(): Array[Byte] = {
      if (count == 0) return new Array(bufferSize)
      
      count -= 1
      pool(count)
    }
    
    final def release(buffer: Array[Byte]): Unit = {      
      assert(buffer.length == bufferSize)
      uniqueCheck(buffer)
      
      if (count < pool.length) {
        pool(count) = buffer
        count += 1
      }
    }
    
    @elidable(elidable.ASSERTION)
    private def uniqueCheck(buf: Array[Byte]): Unit = {
      var i: Int = 0
      while (i < count) {
        assert(buf ne pool(i), s"buf $buf already exists in pool at idx $i!!!")
        i += 1
      }
    }
  }
}

/**
 * A ByteArrayOutputStream implementation optimized for writing binary serialized
 * data (e.g. Protocol Buffers).
 * 
 * Tries to avoid excessive memory allocations and array resizing by using an Array
 * of Byte Arrays to represent the data.  Supports directly appending Byte Arrays
 * (zero-copy), writing length prefixed data, optimized writing of ASCII and UTF-8
 * strings without going through a java.io.Writer.
 * 
 * @param InitialArrayCapacity The initial capacity of the internal arrays
 * @param BufferSize The size of each buffer
 * @param MinUsefulBufferSize If we are splicing data keep using the existing buffer if there is at least this much space in it
 * @param CompactThresholdSize If writing length prefixed data we have a gap then compact the array if the written data is less than this amount
 * @param SpliceThresholdSize If writing a Byte Array that is over this length then splice it in.  If it's under this length then just write it to our buffers
 */
final class FMByteArrayOutputStream(
  InitialArrayCapacity: Int = FMByteArrayOutputStream.DefaultInitialArrayCapacity,
  BufferSize: Int = FMByteArrayOutputStream.DefaultBufferSize,
  MinUsefulBufferSize: Int = FMByteArrayOutputStream.DefaultMinUsefulBufferSize,
  CompactThresholdSize: Int = FMByteArrayOutputStream.DefaultCompactThresholdSize,
  SpliceThresholdSize: Int = FMByteArrayOutputStream.DefaultSpliceThresholdSize
) extends OutputStream with Appendable {
  
  require(MinUsefulBufferSize <= BufferSize, s"MinUsefulBufferSize ($MinUsefulBufferSize) should be less than or equal to BufferSize ($BufferSize)")
  require(CompactThresholdSize <= BufferSize, s"CompactThresholdSize ($CompactThresholdSize) should be less than or equal to BufferSize ($BufferSize)")
  
  private[this] val MAX_UTF8_CHAR_BYTES: Int = 3
  
  private[this] val pool = new FMByteArrayOutputStream.Pool(BufferSize)
  
  /** The actual buffers that contain the data */
  private[this] var buffers: Array[Array[Byte]] = null
  
  /** Start indexes for the buffers */
  private[this] var starts: Array[Int] = null
  
  /** Length of the buffers (from the start index) */
  private[this] var lengths: Array[Int] = null
  
  /** Did this buffer come from the pool? */
  private[this] var isPooled: Array[Boolean] = null
  
  /** The index of the next buffer to add */
  private[this] var _bufferCount: Int = 0
  
  /** Public accessor for tests */
  @inline final def bufferCount: Int = _bufferCount
  
  /** A reference to the current active array that should be written into (e.g. this is the tail) */
  private[this] var _array: Array[Byte] = null
  
  @inline final def array: Array[Byte] = _array
  
  /** The starting idx of the current buffer */
  private[this] var _start: Int = 0
  
  @inline final def start: Int = _start
  
  /** The offset that in the array that should be written to */
  private[this] var _offset: Int = 0
  
  @inline final def offset: Int = _offset
  @inline final def offset_=(v: Int): Unit = _offset = v
  
  /**
   * The max offset we can write into.
   *
   * Note: this is really the start+length but should be easier to code this way.
   *       e.g. we can just say while (offset < length) { ...; length += 1 }
   */
  private[this] var _length: Int = 0
  
  @inline final def length: Int = _length
  
  /**
   * Did this buffer come from the pool?
   */
  private[this] var pooled: Boolean = false
  
  /** How many bytes are available in the current buffer */
  @inline def available: Int = _length - _offset
  
  /**
   * Ensure that there are at least size bytes available in the current buffer (or point to a new buffer).
   */
  @inline def ensureAvailable(size: Int): Unit = {
    if (available < size) addBuffer(size)
  }
  
  /** OutputStream Implementation */
  def write(b: Int): Unit = {
    assert(b >= Byte.MinValue && b <= Byte.MaxValue, s"Invalid Value for byte: $b")
    ensureAvailable(1)
    _array(offsetPlusPlus) = b.toByte
  }
  
  /** OutputStream Implementation */
  override def write(bytes: Array[Byte]): Unit = write(bytes, 0, bytes.length)
  
  /** OutputStream Implementation */
  override def write(bytes: Array[Byte], off: Int, len: Int): Unit = {
    checkOffsetAndLength("write", bytes, off, len)
    
    if (len == 0) return
   
    if (len < SpliceThresholdSize) {
      val end: Int = off + len
      var i: Int = off
      
      if (len < available) {
        var arrOff: Int = _offset
        while (i < end) {
          _array(arrOff) = bytes(i)
          arrOff += 1
          i += 1
        }
        _offset = arrOff
      } else {
        while (i < end) {
          ensureAvailable(1)
          var arrOff: Int = _offset
          val len: Int = _length
          while (i < end && arrOff < len) {
            _array(arrOff) = bytes(i)
            arrOff += 1
            i += 1
          }
          _offset = arrOff
        }
      }
    } else {
      splice(bytes, off, len)
    }
  }
  
  private def sizeOfUTF8Char(ch: Int): Int = {
    if (ch <= 0x007F) return 1
    if (ch <= 0x07FF) return 2
    if (ch <= 0xFFFF) return 3
    0
    //else if (ch <= 0x10FFFF) 4
    //else throw new AssertionError("Invalid UTF-8 Character: "+ch)
  }
  
  /** Appendable Implementation */
  def append(ch: Char): this.type = {
    ensureAvailable(sizeOfUTF8Char(ch))
    appendCharNoBoundsCheck(ch)
    this
  }
  
  /** Appendable Implementation */
  def append(str: CharSequence): this.type = {
    append(str, 0, str.length())
    this
  }
  
  /** Appendable Implementation */
  def append(str: CharSequence, start: Int, end: Int): this.type = {
    write(str.toString(), start, end - start)
    this
  }
  
  def write(str: String): Unit = write(str, 0, str.length())
  
  def write(str: String, start: Int, len: Int): Unit = {
    writeUTF8Bytes(str, start, len)
  }
  
  /**
   * Optimized for when we know the characters are ASCII
   */
  def writeASCII(str: String): Unit = {    
    val len: Int = str.length
    
    if (len == 0) return
    
    if (len < available) {
      // Fits into the current array
      var i: Int = _start
      var arrOff: Int = _offset
      val arr: Array[Byte] = _array
      while (i < len) {
        arr(arrOff) = str.charAt(i).toByte
        arrOff += 1
        i += 1
      }
      _offset = arrOff
    } else {
      // Requires multiple buffers
      var i: Int = _start
      while (i < len) {
        ensureAvailable(1)
        var arrOff: Int = _offset
        val arrLen: Int = _length
        val arr: Array[Byte] = _array
        while (i < len && arrOff < arrLen) {
          arr(arrOff) = str.charAt(i).toByte
          arrOff += 1
          i += 1
        }
        _offset = arrOff
      }
    }
    
  }
  
  def offsetPlusPlus: Int = {
    val oldValue: Int = _offset
    _offset = oldValue + 1
    oldValue
  }
  
  def appendIntAsString(value: Int): Unit = {
    val size: Int = StringUtils.stringSize(value)
    ensureAvailable(size)
    StringUtils.writeIntChars(value, _offset + size, _array)
    _offset += size
  }
  
  def appendLongAsString(value: Long): Unit = {
    val size: Int = StringUtils.stringSize(value)
    ensureAvailable(size)
    StringUtils.writeLongChars(value, _offset + size, _array)
    _offset += size
  }
  
  def appendFloatAsString(value: Float): Unit = {
    write(java.lang.Float.toString(value))
  }
  
  def appendDoubleAsString(value: Double): Unit = {
    write(java.lang.Double.toString(value))
  }
  
  /** Returns the number of bytes written */
  def writeUTF8Bytes(str: String): Int = writeUTF8Bytes(str, 0, str.length)
    
  /** Returns the number of bytes written */
  def writeUTF8Bytes(str: String, start: Int, len: Int): Int = {
    if (len == 0) return 0
    
    val maxSize: Int = MAX_UTF8_CHAR_BYTES * len
    
    if (maxSize < available) return writeUTF8BytesFast(str, start, len)
    writeUTF8BytesSlow(str, start, len)
  }
  
  /**
   * Write UTF8 bytes for the string into the current array with no bounds checking (i.e. we know there is enough space)
   */
  private def writeUTF8BytesFast(str: String, start: Int, len: Int): Int = {
    val startingOffset: Int = _offset
    var off: Int = startingOffset
    val arr: Array[Byte] = _array
    
    var i: Int = start
    var ch: Char = str.charAt(i) // Note: this fails if the len is 0 so that needs to be check prior to calling this method
    val end: Int = start + len
    
    // Fast path for ASCII chars
    while (i < end && ch <= 0x7F) {
      arr(off) = ch.toByte
      off += 1
      i += 1
      if (i < end) ch = str.charAt(i)
    }
    
    // If the fast path terminated check for any remaining chars (which could be a mix of ASCII and non-ASCII)
    while (i < end) {
      off += appendCharNoBoundsCheck(str.charAt(i), arr, off)
      i += 1
    }
    
    _offset = off
    
    off - startingOffset
  }

  private def writeUTF8BytesSlow(str: String, start: Int, strLen: Int): Int = {
    var bytesWritten: Int = 0
    var i: Int = start
    val end: Int = start + strLen
    
    while (i < end) {
      ensureAvailable(1)
      
      var arrOff: Int = _offset
      var ch: Char = str.charAt(i)
      val startingOffset = arrOff
      var arrLen: Int = _length
      var arr: Array[Byte] = _array
      
      while (i < end && arrOff < arrLen && ch <= 0x7F) {
        arr(arrOff) = ch.toByte
        arrOff += 1
        i += 1
        if (i < end) ch = str.charAt(i)
      }
      
      bytesWritten += arrOff - startingOffset
      _offset = arrOff
      
      if (arrOff < arrLen && i < end) {
        ensureAvailable(MAX_UTF8_CHAR_BYTES)
        // _offset and _length might have changed based on the ensureAvailable call
        arrOff = _offset
        arrLen = _length
        arr = _array
        val innerStartingOffset: Int = arrOff        
        while (i < end && arrOff < arrLen - (MAX_UTF8_CHAR_BYTES - 1)) {
          arrOff += appendCharNoBoundsCheck(str.charAt(i), arr, arrOff)
          i += 1
        }
        
        bytesWritten += arrOff - innerStartingOffset
        _offset = arrOff
      }
    }
    
    bytesWritten
  }

  /**
   * Returns the number of bytes written
   */
  private def appendCharNoBoundsCheck(ch: Int): Int = {
    val off: Int = _offset
    val arr: Array[Byte] = _array
    val bytesWritten: Int = appendCharNoBoundsCheck(ch, arr, off)
    _offset = off + bytesWritten
    bytesWritten
  }
  
  private def appendCharNoBoundsCheck(ch: Int, arr: Array[Byte], off: Int): Int = {    
    val bytesWritten: Int = 
      if (ch <= 0x007F) {
        arr(off) = ch.toByte
        1
      } else if (ch <= 0x07FF) {
        arr(off)   = (0xC0 | ((ch >> 6) & 0x1F)).toByte
        arr(off+1) = (0x80 | ((ch     ) & 0x3F)).toByte
        2
      } else if (ch <= 0xFFFF) {
        arr(off)   = (0xE0 | ((ch >> 12) & 0x0F)).toByte
        arr(off+1) = (0x80 | ((ch >>  6) & 0x3F)).toByte
        arr(off+2) = (0x80 | ((ch      ) & 0x3F)).toByte
        3
      } else 0
    
    bytesWritten
  }
   
  def reset(): Unit = {
    
    var i: Int = 0
    while (i < _bufferCount) {
      if (isPooled(i)) pool.release(buffers(i))
      buffers(i) = null
      starts(i) = 0
      lengths(i) = 0
      isPooled(i) = false
      i += 1
    }
    
    if (null != array && pooled) pool.release(array)
    
    _bufferCount = 0
    _array = null
    _start = 0
    _offset = 0
    _length = 0
    pooled = false
  }

  @inline def countBytes(f: => Unit): Int = {
    val startingCount: Int = totalSize
    f
    totalSize - startingCount
  }
  
  /**
   * Remove or Skip over the bytes at the starting index to length.
   * 
   * These are expected to be contiguous bytes in one of the buffers.
   * The buffer will be looked up based on the start index and then data
   * will either be compacted or spliced around based on how much data comes
   * after the skipped bytes
   */
  def skipBytes(globalIdx: Int, len: Int): Unit = {
    debug(s"skipBytes($globalIdx, $len)")
    
    assert(globalIdx >= 0 && globalIdx <= totalSize && len >= 0, s"Invalid Params:  skipBytes($globalIdx, $len)")
    check()
    
    if (len == 0) return
    
    // Fast Path if there aren't any other buffers
    if (_bufferCount == 0) return skipBytesInCurrentBufferWithNoOtherBuffers(globalIdx, len)

    skipBytesSlow(globalIdx, len)
  }
  
  private def skipBytesSlow(globalIdx: Int, len: Int): Unit = {
    debug(s"skipBytesSlow($globalIdx, $len)")
    
    val total: Int = totalSize
    
    // Faster path if we are still working in the same buffer
    if (isIdxInCurrentBuffer(globalIdx, total)) return skipBytesInCurrentBuffer(globalIdx, len, total)
    
    skipBytesInPreviousBuffer(globalIdx, len)
  }
  
  /** Is this global byte index in the current (i.e. array) buffer? */
  private def isIdxInCurrentBuffer(globalIdx: Int, total: Int): Boolean = globalIdx >= total - (_offset - _start)
  
  /**
   * This assumes that (array != null && (buffers == null || buffers.length == 0))
   */
  private def skipBytesInCurrentBufferWithNoOtherBuffers(globalIdx: Int, len: Int): Unit = {
    debug(s"skipBytesInCurrentBufferWithNoOtherBuffers($globalIdx, $len)")
    //debug(debugInfo)
    
    val dataSize: Int = _offset - (globalIdx + len)
    
    assert(dataSize >= 0, s"Invalid dataSize: $dataSize   offset: $offset  totalSize: $totalSize  globalIdx: $globalIdx  len: $len")
    
    if (dataSize <= CompactThresholdSize) {
      shiftArrayNegative(array, globalIdx + len, dataSize, -len)
      _offset -= len
      check()
      return
    }
    
    saveBuffer(array, 0, globalIdx, pooled)
    _start = globalIdx + len
    pooled = false
    
    check()
  }
  
  /**
   * The current array is where we are skipping bytes.
   */
  private def skipBytesInCurrentBuffer(globalIdx: Int, len: Int, totalSize: Int): Unit = {
    debug(s"skipBytesInCurrentBuffer($globalIdx, $len, $totalSize)")
    //debug(debugInfo)

    val dataSize: Int = totalSize - (globalIdx + len)
    
    dumpOnAssert{ assert(dataSize >= 0, s"Invalid dataSize: $dataSize   totalSize: $totalSize  globalIdx: $globalIdx  len: $len") }
    
    if (dataSize <= CompactThresholdSize) {
      shiftArrayNegative(array, offset - dataSize, dataSize, -len)
      _offset -= len
      check()
    } else {
      saveBuffer(array, start, offset - start - dataSize - len, pooled)
      pooled = false
      _start = _offset - dataSize 
      check()
    }
  }
  
  private def skipBytesInPreviousBuffer(globalIdx: Int, len: Int): Unit = {
    //debug(s"skipBytesInPreviousBuffer($globalIdx, $len)")
    //debug(debugInfo)
    
    val bufferIdx: Int = bufferIndexOfGlobalByteIndex(globalIdx)
    val buffer: Array[Byte] = buffers(bufferIdx)
    val bufferStart: Int = starts(bufferIdx)
    val bufferLength: Int = lengths(bufferIdx)
    
    // Where the globalIdx starts in the array for this buffer
    val startIdx: Int = globalIdx - startingGlobalIndexOfBuffer(bufferIdx) + bufferStart

    assert(startIdx >= bufferStart, s"Invalid startIdx: $startIdx  starts(bufferIdx): ${bufferStart}")
    
    // Where the data ends for this buffer
    val endIdx: Int = bufferStart + bufferLength - 1
    
    val dataSize: Int = endIdx - (startIdx + len) + 1
    
    assert(dataSize >= 0, s"Invalid dataSize: $dataSize   endIdx: $endIdx  startIdx: $startIdx  globalIdx: $globalIdx  len: $len")
    
    if (dataSize <= CompactThresholdSize) {
      shiftArrayNegative(buffer, startIdx + len, dataSize, -len)
      lengths(bufferIdx) = bufferLength - len
      assert(lengths(bufferIdx) >= 0, s"Invalid Length: ${lengths(bufferIdx)}   endIdx: $endIdx  startIdx: $startIdx  globalIdx: $globalIdx  len: $len")
      check()
    } else {
      spliceBuffer(bufferIdx + 1, buffer, startIdx + len, dataSize)
      lengths(bufferIdx) = bufferLength - dataSize - len
      assert(lengths(bufferIdx) >= 0, s"Invalid Length: ${lengths(bufferIdx)}   endIdx: $endIdx  startIdx: $startIdx  globalIdx: $globalIdx  len: $len")
      check()
    }
  }
  
  /**
   * Given the global index of a byte, find which buffer it belongs to.
   * 
   * Note: Assumes that it doesn't belong to the current buffer (e.g. array)
   */
  private def bufferIndexOfGlobalByteIndex(globalIdx: Int): Int = {
    var i: Int = 0
    var size: Int = 0
    var done: Boolean = false
    
    while (!done) {
      size += lengths(i)
      if (globalIdx < size) done = true else i += 1
    }
    
    i
  }
  
  private def startingGlobalIndexOfBuffer(bufferIdx: Int): Int = {
    var size: Int = 0
    var i: Int = 0
    while (i < bufferIdx) {
      size += lengths(i)
      i += 1
    }
    size
  }
  
  /**
   * This is for writing data that needs to have it's length prefixed when the prefixed length is variable.
   * e.g. When write Protocol Buffers length delimited fields the length is a varint.
   * 
   * This method takes the maximum prefix size we expect to write, a function that performs the writing the data,
   * and a function that takes the number of bytes written via writeData and performs the length prefix write.  If
   * the length prefix write is less than the maxPrefixSize parameter then the array will either be compacted
   * or split into two.
   */
  @inline def lengthPrefixed(maxPrefixSize: Int)(writeData: => Unit)(writePrefix: Int => Unit): Unit = {
    assert(maxPrefixSize >= 0, s"lengthPrefixed($maxPrefixSize)")
    
    ensureAvailable(maxPrefixSize)
    
    val startingArray: Array[Byte] = _array
    val startingOffset: Int = _offset
    
    offset += maxPrefixSize
    
    val startingSize: Int = totalSize
    writeData
    val endingSize: Int = totalSize
    
    val dataSize: Int = endingSize - startingSize
    
    assert(dataSize >= 0, s"Invalid dataSize: $dataSize  startingSize: $startingSize  endingSize: $endingSize")
    
    val endingArray: Array[Byte] = _array
    val endingOffset: Int = _offset
    
    _array = startingArray
    _offset = startingOffset
    
    writePrefix(dataSize)
    
    val prefixSize: Int = offset - startingOffset
    
    assert(prefixSize >= 0 && prefixSize <= maxPrefixSize, s"Invalid prefixSize: $prefixSize  maxPrefixSize: $maxPrefixSize")

    //debug("=======================================================================================================================")
    //debug(s"startingSize: $startingSize")
    //debug(s"endingSize: $endingSize")
    //debug(s"bytesWritten: $dataSize")
    //debug(s"maxPrefixSize: $maxPrefixSize")
    //debug(s"prefixSize: $prefixSize")
    //debug("=======================================================================================================================")
    
    _array = endingArray
    _offset = endingOffset
    
    val gap: Int = maxPrefixSize - prefixSize
    
    if (dataSize == 0) {
      _offset -= gap
      return
    }
    
    if (prefixSize < maxPrefixSize) {
      skipBytes(startingSize - gap, gap)
    }
  }
  
  /**
   * Directly use this byte array (zero-copy).  If there are bytes left
   * in our current buffer to make it useful then we will splice this into
   * our current array by saving the current array, saving this array, and
   * then using the rest of the original array as our current buffer with
   * updated start/offset/length
   */
  def splice(bytes: Array[Byte], off: Int, len: Int): Unit = {
    checkOffsetAndLength("splice", bytes, off, len)
    
    saveCurrentBuffer()
    saveBuffer(bytes, off, len, pooled = false)
    if (available >= MinUsefulBufferSize) {
      // Keep using the current buffer but just adjust the start
      _start = offset
    } else {
      _array = null
      _start = 0
      _offset = 0
      _length = 0
    }
    
    check()
  }
  
  private def addBuffer(minSize: Int): Unit = {  
    saveCurrentBuffer()
    
    if (minSize <= BufferSize) {
      _array = pool.take()
      pooled = true
      _length = BufferSize
    } else {
      _array = new Array(minSize)
      pooled = false
      _length = minSize
    }
    
    _start = 0
    _offset = 0
    
    check()
  }
  
  private def saveCurrentBuffer(): Unit = {
    check()
    
    val arr: Array[Byte] = _array
    val start: Int = _start
    val len: Int = _offset - start
    
    if (null != arr && len > 0) {
      saveBuffer(arr, start, len, pooled)
      pooled = false
    }
  }
  
  private def saveBuffer(buf: Array[Byte], start: Int, len: Int, pooled: Boolean): Unit = {
    checkOffsetAndLength("saveBuffer", buf, start, len)

    if (len == 0) return
    
    val bufIdx: Int = _bufferCount
    
    ensureAvailableBufferSlot()
    buffers(bufIdx) = buf
    starts(bufIdx) = start
    lengths(bufIdx) = len
    isPooled(bufIdx) = pooled
    _bufferCount = bufIdx + 1
    
    check()
  }
  
  /**
   * Insert a buffer into idx by shifting everything after idx by 1
   */
  private def spliceBuffer(idx: Int, buf: Array[Byte], start: Int, len: Int): Unit = {
    checkOffsetAndLength("spliceBuffer", buf, start, len)
    check()
    
    ensureAvailableBufferSlot()
    
    val bufferShiftLength: Int = _bufferCount - idx
    
    shiftArrayPositive(buffers, idx, bufferShiftLength, 1)
    shiftArrayPositive(starts, idx, bufferShiftLength, 1)
    shiftArrayPositive(lengths, idx, bufferShiftLength, 1)
    shiftArrayPositive(isPooled, idx, bufferShiftLength, 1)
    
    buffers(idx) = buf
    starts(idx) = start
    lengths(idx) = len
    isPooled(idx) = false
    
    _bufferCount += 1
    
    check()
  }

  private def ensureAvailableBufferSlot(): Unit = {
    if (null == buffers || buffers.length - _bufferCount == 0) {
      val newSize: Int = if (null == buffers) InitialArrayCapacity else buffers.length * 2
      resizeArrays(newSize)
    }
  }
  
  private def resizeArrays(newSize: Int): Unit = {
    buffers = resizeByteArrayArray(buffers, newSize)
    starts = resizeIntArray(starts, newSize)
    lengths = resizeIntArray(lengths, newSize)
    isPooled = resizeBoolArray(isPooled, newSize)
  }
  
  private def resizeByteArrayArray(arr: Array[Array[Byte]], newSize: Int): Array[Array[Byte]] = {
    val buf = new Array[Array[Byte]](newSize)
    if (null != arr) System.arraycopy(arr, 0, buf, 0, bufferCount)
    buf
  }
  
  private def resizeIntArray(arr: Array[Int], newSize: Int): Array[Int] = {
    val buf = new Array[Int](newSize)
    if (null != arr) System.arraycopy(arr, 0, buf, 0, bufferCount)
    buf
  }
  
  private def resizeBoolArray(arr: Array[Boolean], newSize: Int): Array[Boolean] = {
    val buf = new Array[Boolean](newSize)
    if (null != arr) System.arraycopy(arr, 0, buf, 0, bufferCount)
    buf
  }
    
  final def shiftArray(arr: Array[Array[Byte]], start: Int, len: Int, shift: Int): Unit = {
    checkOffsetAndLength("shiftArray", arr, start, len)
    if (shift < 0) return shiftArrayNegative(arr, start, len, shift)
    if (shift > 0) return shiftArrayPositive(arr, start, len, shift)
    assert(false, "Shift was 0")
  }
  
  final def shiftArray(arr: Array[Byte], start: Int, len: Int, shift: Int): Unit = {
    checkOffsetAndLength("shiftArray", arr, start, len)
    if (shift < 0) return shiftArrayNegative(arr, start, len, shift)
    if (shift > 0) return shiftArrayPositive(arr, start, len, shift)
    assert(false, "Shift was 0")
  }
  
  final def shiftArray(arr: Array[Int], start: Int, len: Int, shift: Int): Unit = {
    checkOffsetAndLength("shiftArray", arr, start, len)
    if (shift < 0) return shiftArrayNegative(arr, start, len, shift)
    if (shift > 0) return shiftArrayPositive(arr, start, len, shift)
    assert(false, "Shift was 0")
  }
  
  final def shiftArray(arr: Array[Boolean], start: Int, len: Int, shift: Int): Unit = {
    checkOffsetAndLength("shiftArray", arr, start, len)
    if (shift < 0) return shiftArrayNegative(arr, start, len, shift)
    if (shift > 0) return shiftArrayPositive(arr, start, len, shift)
    assert(false, "Shift was 0")
  }
  
  private def shiftArrayNegative(arr: Array[Array[Byte]], start: Int, len: Int, shift: Int): Unit = {
    if (len == 0) return
    assert(shift < 0 && start - shift >= 0, s"Invalid Shift Params: arr: $arr  start: $start  len: $len  shift: $shift")
    checkOffsetAndLength("shiftArrayNegative", arr, start, len)
    var i: Int = start
    val end: Int = start + len
    while (i <= end) {
      arr(i + shift) = arr(i)
      i += 1
    }
  }
  
  private def shiftArrayNegative(arr: Array[Byte], start: Int, len: Int, shift: Int): Unit = {
    if (len == 0) return
    assert(shift < 0 && start - shift >= 0, s"Invalid Shift Params: arr: $arr  start: $start  len: $len  shift: $shift")
    checkOffsetAndLength("shiftArrayNegative", arr, start, len)
    var i: Int = start
    val end: Int = start + len
    while (i < end) {
      arr(i + shift) = arr(i)
      i += 1
    }
  }
  
  private def shiftArrayNegative(arr: Array[Int], start: Int, len: Int, shift: Int): Unit = {
    if (len == 0) return
    assert(shift < 0 && start - shift >= 0, s"Invalid Shift Params: arr: $arr  start: $start  len: $len  shift: $shift")
    checkOffsetAndLength("shiftArrayNegative", arr, start, len)
    var i: Int = start
    val end: Int = start + len
    while (i < end) {
      arr(i + shift) = arr(i)
      i += 1
    }
  }
  
  private def shiftArrayNegative(arr: Array[Boolean], start: Int, len: Int, shift: Int): Unit = {
    if (len == 0) return
    assert(shift < 0 && start - shift >= 0, s"Invalid Shift Params: arr: $arr  start: $start  len: $len  shift: $shift")
    checkOffsetAndLength("shiftArrayNegative", arr, start, len)
    var i: Int = start
    val end: Int = start + len
    while (i < end) {
      arr(i + shift) = arr(i)
      i += 1
    }
  }
  
  private def shiftArrayPositive(arr: Array[Array[Byte]], start: Int, len: Int, shift: Int): Unit = {
    if (len == 0) return
    assert(shift > 0 && start + len + shift <= arr.length, s"Invalid Shift Params: arr: $arr  start: $start  len: $len  shift: $shift")
    checkOffsetAndLength("shiftArrayPositive", arr, start, len)
    var i: Int = start + len - 1
    while (i >= start) {
      arr(i + shift) = arr(i)
      i -= 1
    }
  }
  
  private def shiftArrayPositive(arr: Array[Byte], start: Int, len: Int, shift: Int): Unit = {
    if (len == 0) return
    assert(shift > 0 && start + len + shift <= arr.length, s"Invalid Shift Params: arr: $arr  start: $start  len: $len  shift: $shift")
    checkOffsetAndLength("shiftArrayPositive", arr, start, len)
    var i: Int = start + len - 1
    while (i >= start) {
      arr(i + shift) = arr(i)
      i -= 1
    }
  }
  
  private def shiftArrayPositive(arr: Array[Int], start: Int, len: Int, shift: Int): Unit = {
    if (len == 0) return
    assert(shift > 0 && start + len + shift <= arr.length, s"Invalid Shift Params: arr: $arr  start: $start  len: $len  shift: $shift")
    checkOffsetAndLength("shiftArrayPositive", arr, start, len)
    var i: Int = start + len - 1
    while (i >= start) {
      arr(i + shift) = arr(i)
      i -= 1
    }
  }
  
  private def shiftArrayPositive(arr: Array[Boolean], start: Int, len: Int, shift: Int): Unit = {
    if (len == 0) return
    assert(shift > 0 && start + len + shift <= arr.length, s"Invalid Shift Params: arr: $arr  start: $start  len: $len  shift: $shift")
    checkOffsetAndLength("shiftArrayPositive", arr, start, len)
    var i: Int = start + len - 1
    while (i >= start) {
      arr(i + shift) = arr(i)
      i -= 1
    }
  }
  
  def totalSize: Int = {
    check()
    
    var size: Int = 0
    
    // Any saved buffers
    var i: Int = 0
    val bufCount: Int = _bufferCount
    while (i < bufCount) {
      size += lengths(i)
      i += 1
    }

    // The current array
    if (null != _array) size += _offset - _start
    
    size
  }
  
  /** Create a byte array out of the current data */
  def toByteArray: Array[Byte] = {
    check()
    
    val dest: Array[Byte] = new Array(totalSize)
    var destIdx: Int = 0
    
    var i: Int = 0
    val bufCount: Int = _bufferCount
    while (i < bufCount) {
      val buf: Array[Byte] = buffers(i)
      val starting: Int = starts(i)
      val len: Int = lengths(i)
      checkOffsetAndLength(s"Buffer Idx: $i", buf, starting, len)
      System.arraycopy(buf, starting, dest, destIdx, len)
      i += 1
      destIdx += len
    }
    
    if (null != array) {
      checkOffsetAndLength()
      val len: Int = _offset - _start
      System.arraycopy(_array, _start, dest, destIdx, len)
      destIdx += len
    }
    
    assert(destIdx == dest.length, s"Expected destIdx ($destIdx) == dest.length (${dest.length})")
    
    dest
  }
  
  /** Write the current data in the buffer to an OutputStream */
  def writeTo(os: OutputStream): Unit = {
    var idx: Int = 0
    val bufCount: Int = _bufferCount
    while (idx < bufCount) {
      os.write(buffers(idx), starts(idx), lengths(idx))
      idx += 1
    }
    
    if (null != _array) {
      os.write(_array, _start, _offset - _start)
    }
  }
  
  @elidable(elidable.ASSERTION)
  private def checkOffsetAndLength(): Unit = {
    if (null == array) return
    assert(length == array.length, s"Expected length ($length) == array.length (${array.length})")
    checkOffsetAndLength("Current Array", array, start, offset - start)
  }
  
  @elidable(elidable.ASSERTION)
  private def checkOffsetAndLength[T](msg: => String, arr: Array[T], off: Int, len: Int): Unit = {
    checkOffsetAndLength(msg, arr, arr.length, off, len)
  }
  
  @elidable(elidable.ASSERTION)
  private def checkOffsetAndLength(msg: => String, str: CharSequence, off: Int, len: Int): Unit = {
    checkOffsetAndLength(msg, str, str.length, off, len)
  }
  
  @elidable(elidable.ASSERTION)
  private def checkOffsetAndLength(msg: => String, obj: AnyRef, actualLength: Int, off: Int, len: Int): Unit = {
    lazy val fullMsg: String = s"$msg - Invalid Start/Offset or Length:  obj: $obj  obj.length: ${actualLength}  off: $off  len: $len"
    assert(off == 0 || (off > 0 && off < actualLength), fullMsg)
    assert(len >= 0, fullMsg)
    assert(len - off <= actualLength, fullMsg)
  }
  
  @elidable(elidable.FINE)
  def debug(msg: String): Unit = {
    println(msg)
  }
  
  @elidable(elidable.ASSERTION)
  def check(): Unit = dumpOnAssert {
    var i: Int = 0
    while (i < bufferCount) {
      val buf: Array[Byte] = buffers(i)
      val s: Int = starts(i)
      val len: Int = lengths(i)

      assert(null != buf, s"buf is null for idx: $i")
      checkOffsetAndLength(s"BufferIndex: $i", buf, s, len)
      
      i += 1
    }
    
    checkOffsetAndLength()
  }
  
  @elidable(elidable.ASSERTION)
  def dumpOnAssert(f: => Unit): Unit = try {
    f
  } catch {
    case ex: AssertionError =>
      println("================================================START=======================================================================")
      println("ASSERTION ERROR - "+ex.getMessage)
      ex.printStackTrace()
      println(debugInfo)
      println("=================================================END========================================================================")
      throw ex
  }
 
  @elidable(elidable.FINE)
  def debugInfo: String = {
s"""
  buffers: ${Option(buffers).map{ _.toVector.take(bufferCount).zipWithIndex.map{ case (arr, idx) => s"\n    $idx - $arr - ${arr.toVector}" }.mkString("") }.orNull}

  starts: ${Option(starts).map{ _.toVector.take(bufferCount) }.orNull}
  lengths: ${Option(lengths).map{ _.toVector.take(bufferCount) }.orNull}
  
  bufferCount: $bufferCount
  
  array: $array - ${if (null != array) array.toVector}
  start: $start
  offset: $offset
  length: $length
  available: $available
"""
  }
}