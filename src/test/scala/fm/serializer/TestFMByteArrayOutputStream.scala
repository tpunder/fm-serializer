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

import java.util.Random
import org.scalatest.{FunSuite, Matchers}

final class TestFMByteArrayOutputStreamDefaultSizes extends TestFMByteArrayOutputStream {
  def InitialArrayCapacity: Int = FMByteArrayOutputStream.DefaultInitialArrayCapacity
  def BufferSize: Int = FMByteArrayOutputStream.DefaultBufferSize
  def MinUsefulBufferSize: Int = FMByteArrayOutputStream.DefaultMinUsefulBufferSize
  def CompactThresholdSize: Int = FMByteArrayOutputStream.DefaultCompactThresholdSize
  def SpliceThresholdSize: Int = FMByteArrayOutputStream.DefaultSpliceThresholdSize
}

final class TestFMByteArrayOutputStreamSmallBuffers extends TestFMByteArrayOutputStream {
  // Don't change these values since the tests rely on them
  def InitialArrayCapacity: Int = 8
  def BufferSize: Int = 16
  def MinUsefulBufferSize: Int = 8
  def CompactThresholdSize: Int = 8
  def SpliceThresholdSize: Int = 8


  test("skipBytes - 0") {
    val os = newFMByteArrayOutputStream()
    os.skipBytes(0, 0)
    checkEquals(os, Array[Byte]())
  }
  
  private val SampleData = Array(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
  private val SampleDataAfterGaps = Array(2,4,5,6,7,8,9,13,14,15,16)
  private val SampleDataAfterGapsBufferCount: Int = 3
  
  // Assumes you have written sample data to the OS
  private def makeGapsInSampleData(os: FMByteArrayOutputStream, offset: Int = 0): Unit = {
    val checkBufferCount: Boolean = offset == 0 && os.totalSize == SampleData.length
    val beforeBufferCount: Int = os.bufferCount
    
    // Should splice buffer
    os.skipBytes(offset + 0, 2)
    if (checkBufferCount) os.bufferCount should equal (beforeBufferCount + 1)
    
    // Should compact buffer
    os.skipBytes(offset + 8, 3)
    if (checkBufferCount) os.bufferCount should equal (beforeBufferCount + 1)
    
    // Should splice buffer
    os.skipBytes(offset + 1, 1)
    if (checkBufferCount) os.bufferCount should equal (beforeBufferCount + 2)
    
    // Should compact buffer
    os.skipBytes(offset + 11, 2)
    if (checkBufferCount) os.bufferCount should equal (beforeBufferCount + 2)
    
    os.toByteArray.drop(offset).take(SampleDataAfterGaps.length).toVector should equal (SampleDataAfterGaps.toVector)
  }
  
  test("skipBytes - single buffer - basic") {
    val os = newFMByteArrayOutputStream()
    Array(0,1,2,3,4,5,6,7,8,9,10).foreach{ os.write }
    os.skipBytes(4, 3)
    os.skipBytes(7, 1)
    os.skipBytes(0, 1)
    os.bufferCount should equal(0)
    checkEquals(os, Array(1,2,3,7,8,9))
  }
  
  test("skipBytes - single buffer - shift") {
    val os = newFMByteArrayOutputStream()
    Array(0,1,2,3,4,5,6,7,8,9,10).foreach{ os.write }
    os.skipBytes(2, 1)
    os.bufferCount should equal(0)
    checkEquals(os, Array(0,1,3,4,5,6,7,8,9,10))
  }
  
  test("skipBytes - single buffer - splice") {
    val os = newFMByteArrayOutputStream()
    Array(0,1,2,3,4,5,6,7,8,9,10).foreach{ os.write }
    os.skipBytes(1, 1)
    os.bufferCount should equal(1)
    checkEquals(os, Array(0,2,3,4,5,6,7,8,9,10))
  }
  
  test("skipBytes - multi buffer - basic in current array") {
    val os = newFMByteArrayOutputStream()
    SampleData.foreach{ os.write }
    Array(0,1,2,3,4,5,6,7,8,9,10).foreach{ os.write }
    os.skipBytes(SampleData.length + 4, 3)
    os.skipBytes(SampleData.length + 7, 1)
    os.skipBytes(SampleData.length + 0, 1)
    os.bufferCount should equal(1)
    checkEquals(os, SampleData++Array(1,2,3,7,8,9))
  }
  
  test("skipBytes - multi buffer - shift current array") {
    val os = newFMByteArrayOutputStream()
    SampleData.foreach{ os.write }
    Array(0,1,2,3,4,5,6,7,8,9,10).foreach{ os.write }
    os.skipBytes(SampleData.length + 2, 1)
    os.bufferCount should equal(1)
    checkEquals(os, SampleData++Array(0,1,3,4,5,6,7,8,9,10))
  }
  
  test("skipBytes - multi buffer - splice current array") {
    val os = newFMByteArrayOutputStream()
    SampleData.foreach{ os.write }
    Array(0,1,2,3,4,5,6,7,8,9,10).foreach{ os.write }
    os.skipBytes(SampleData.length + 1, 1)
    os.bufferCount should equal(2)
    checkEquals(os, SampleData++Array(0,2,3,4,5,6,7,8,9,10))
  }
  
  test("skipBytes - multi buffer with gaps - basic in current array") {
    val os = newFMByteArrayOutputStream()
    SampleData.foreach{ os.write }
    makeGapsInSampleData(os)
    Array(0,1,2,3,4,5,6,7,8,9,10).foreach{ os.write }
    os.skipBytes(SampleDataAfterGaps.length + 4, 3)
    os.bufferCount should equal(SampleDataAfterGapsBufferCount)
    checkEquals(os, SampleDataAfterGaps++Array(0,1,2,3,7,8,9,10))
  }
  
  test("skipBytes - multi buffer with gaps - shift current array") {
    val os = newFMByteArrayOutputStream()
    SampleData.foreach{ os.write }
    makeGapsInSampleData(os)
    Array(0,1,2,3,4,5,6,7,8,9,10).foreach{ os.write }
    os.skipBytes(SampleDataAfterGaps.length + 2, 1)
    os.bufferCount should equal(SampleDataAfterGapsBufferCount)
    checkEquals(os, SampleDataAfterGaps++Array(0,1,3,4,5,6,7,8,9,10))
  }
  
  test("skipBytes - multi buffer with gaps - splice current array") {
    val os = newFMByteArrayOutputStream()
    SampleData.foreach{ os.write }
    makeGapsInSampleData(os)
    Array(0,1,2,3,4,5,6,7,8,9,10).foreach{ os.write }
    os.skipBytes(SampleDataAfterGaps.length + 1, 1)
    os.bufferCount should equal(SampleDataAfterGapsBufferCount + 1)
    checkEquals(os, SampleDataAfterGaps++Array(0,2,3,4,5,6,7,8,9,10))
  }
  
  test("skipBytes - makeGapsInSampleData * 2") {
    val os = newFMByteArrayOutputStream()
    
    SampleData.foreach{ os.write }
    makeGapsInSampleData(os)
    os.totalSize should equal (SampleDataAfterGaps.length)

    //println(os.debugInfo)
    
    SampleData.foreach{ os.write }
    makeGapsInSampleData(os, SampleDataAfterGaps.length)
    os.totalSize should equal (SampleDataAfterGaps.length * 2)
    
    checkEquals(os, SampleDataAfterGaps ++ SampleDataAfterGaps)
  }
  
  test("skipBytes - makeGapsInSampleData * 3") {
    val os = newFMByteArrayOutputStream()
    
    SampleData.foreach{ os.write }
    makeGapsInSampleData(os)
    os.totalSize should equal (SampleDataAfterGaps.length)
    
    SampleData.foreach{ os.write }
    makeGapsInSampleData(os, SampleDataAfterGaps.length)
    os.totalSize should equal (SampleDataAfterGaps.length * 2)
    
    SampleData.foreach{ os.write }
    makeGapsInSampleData(os, SampleDataAfterGaps.length * 2)
    os.totalSize should equal (SampleDataAfterGaps.length * 3)
    
    checkEquals(os, SampleDataAfterGaps ++ SampleDataAfterGaps ++ SampleDataAfterGaps)
  }
  
  test("skipBytes - mix of SampleData and SampleDataAfterGaps") {
    val os = newFMByteArrayOutputStream()
    
    SampleData.foreach{ os.write }
    SampleData.foreach{ os.write }
    SampleData.foreach{ os.write }
    
    makeGapsInSampleData(os, 0)
    makeGapsInSampleData(os, SampleDataAfterGaps.length)
    makeGapsInSampleData(os, SampleDataAfterGaps.length * 2)
        
    checkEquals(os, SampleDataAfterGaps ++ SampleDataAfterGaps ++ SampleDataAfterGaps)
  }
  
  test("skipBytes - Complex example") {
//    val os = newFMByteArrayOutputStream()
//
//    os.write(1)
//    os.write(2)
//    
//    // should save current buffer and splice in SampleData
//    os.write(SampleData.map{ _.toByte })
//    os.bufferCount should equal (2)
//    
//    os.write(3)
//    os.write(4)
//    
//    os.write(Array[Byte](5,6,7,8,9,10,11,12,13,14,15,16))
  }
  
}

abstract class TestFMByteArrayOutputStream extends FunSuite with Matchers {
  private val UseRandom = false
  
  def InitialArrayCapacity: Int
  def BufferSize: Int
  def MinUsefulBufferSize: Int
  def CompactThresholdSize: Int
  def SpliceThresholdSize: Int
  
  protected def newFMByteArrayOutputStream() = new FMByteArrayOutputStream(InitialArrayCapacity = InitialArrayCapacity, BufferSize = BufferSize)
  
  protected def makeBytes(length: Int): Array[Byte] = {
    val bytes: Array[Byte] = new Array(length)
    
    if (UseRandom) {
      val rand: Random = new Random()
      rand.nextBytes(bytes)
    } else {
      var i = 0
      while(i < length) {
        bytes(i) = ((i % 128) + 1).toByte
        i += 1
      }
    }
    
    bytes
  }
  
  protected def testRandom(length: Int): Unit = {
    val rand: Random = new Random()
    val bytes: Array[Byte] = makeBytes(length)
    
    val os = newFMByteArrayOutputStream()
    
    var i: Int = 0
    while(i < length) {
      val bulkLen: Int = 20
      
      if (rand.nextBoolean && length - i > bulkLen) {
        os.write(bytes, i, bulkLen)
        i += bulkLen
      } else {
        os.write(bytes(i))
        i += 1
      }
    }
    
    checkEquals(os, bytes)
  }
  
  protected def checkEquals(os: FMByteArrayOutputStream, expected: Array[Int]): Unit = {
    checkEquals(os, expected.map{ _.toByte })
  }
  
  protected def checkEquals(os: FMByteArrayOutputStream, expected: Array[Byte]): Unit = {
    os.totalSize should equal (expected.length)
    
    os.toByteArray.toVector should equal (expected.toVector)
    
    val bos = new java.io.ByteArrayOutputStream
    os.writeTo(bos)
    bos.toByteArray().toVector should equal (expected.toVector)
  }
  
//  test("Random Output / Random Length Tests") {
//    val seed: Long = 1234567L
//    val rand: Random = new Random(seed)
//    
//    (1 to 1000).foreach { i =>
//      val length: Int = rand.nextInt(10000) + 1
//      testRandom(length)
//    }
//  }
  
  test("Total Size") {
    val os = newFMByteArrayOutputStream()
    
    os.write('H')
    os.totalSize should equal (1)
    
    os.write('e')
    os.totalSize should equal (2)
    
    os.write(Array[Byte]('l','l','o'))
    os.totalSize should equal (5)
    
    os.write(makeBytes(BufferSize))
    os.totalSize should equal (BufferSize + 5)
    
    os.write(makeBytes(BufferSize*2))
    os.totalSize should equal (BufferSize * 3 + 5)
  }
  
//  test("skipBytes - 0") {
//    val os = newFMByteArrayOutputStream()
//    os.skipBytes(0, 0)
//    checkEquals(os, Array())
//  }
//  
//  test("skipBytes - same buffer") {
//    val os = newFMByteArrayOutputStream()
//    (0 to 8).foreach{ os.write }
//    os.skipBytes(4, 3)
//    os.bufferCount should equal(0)
//    checkEquals(os, Array(0,1,2,3,7,8))
//  }
  
  test("lengthPrefixed - same buffer / shifting") {
    val os = newFMByteArrayOutputStream()
    
    // BufferSize - 4            => To allow enough room for the gap without overflowing into another buffer
    // CompactThresholdSize + 2  => The CompactThresholdSize + our 2 bytes of length prefix
    val bytes = makeBytes(math.min(BufferSize - 4, CompactThresholdSize + 2))
    
    //println("BEFORE - lengthPrefixed - same buffer / shifting")
    //println(os.debugInfo)
    
    os.lengthPrefixed(4){
      (2 until bytes.length).foreach{ i => os.write(bytes(i)) }
    }{ _ => // We don't care what the length was
      os.write(bytes(0))
      os.write(bytes(1))
    }
    
    //println("AFTER - lengthPrefixed - same buffer / shifting")
    //println(os.debugInfo)
    
    // There should only be our current array (which means an empty buffers array)
    os.bufferCount should equal (0)
    
    checkEquals(os, bytes)
  }
  
  test("lengthPrefixed - same buffer / shifting (with an existing filled buffer)") {
    val os = newFMByteArrayOutputStream()
    
    val extra: Int = BufferSize + 2
    
    // BufferSize - 4            => To allow enough room for the gap without overflowing into another buffer
    // CompactThresholdSize + 2  => The CompactThresholdSize + our 2 bytes of length prefix
    val bytes = makeBytes(math.min(BufferSize - 4, CompactThresholdSize + 2) + extra)
    
    //println("BEFORE - lengthPrefixed - same buffer / shifting (with an existing filled buffer)")
    //println(os.debugInfo)
    
    // Fill up a complete buffer
    (0 until extra).foreach{ i => os.write(bytes(i)) }
    
    os.lengthPrefixed(4){
      ((extra + 2) until bytes.length).foreach{ i => os.write(bytes(i)) }
    }{ _ => // We don't care what the length was
      os.write(bytes(extra))
      os.write(bytes(extra+1))
    }
    
    //println("AFTER - lengthPrefixed - same buffer / shifting (with an existing filled buffer)")
    //println(os.debugInfo)
    
    // There should now be one buffer (in addition to the current array)
    os.bufferCount should equal (1)
    
    checkEquals(os, bytes)
  }
  
  test("lengthPrefixed - same buffer / spliced") {
    val os = newFMByteArrayOutputStream()
    
    // BufferSize - 4            => To allow enough room for the gap without overflowing into another buffer
    // CompactThresholdSize + 3  => The CompactThresholdSize + our 2 bytes of length prefix plus an extra byte to avoid compacting
    val bytes = makeBytes(math.max(BufferSize - 4, CompactThresholdSize + 3))
    
    //println(os.debugInfo)
    
    os.lengthPrefixed(4){
      (2 until bytes.length).foreach{ i => os.write(bytes(i)) }
    }{ _ => // We don't care what the length was
      os.write(bytes(0))
      os.write(bytes(1))
    }
    
    //println(os.debugInfo)
    
    // There should still be a single buffer
    os.bufferCount should equal (1)
    os.start should equal (4)
    
    checkEquals(os, bytes)
  }
  
  test("lengthPrefixed - same buffer / spliced (with existing filled buffer)") {
    val os = newFMByteArrayOutputStream()
    
    val extra: Int = BufferSize + 2
    
    // BufferSize - 4            => To allow enough room for the gap without overflowing into another buffer
    // CompactThresholdSize + 3  => The CompactThresholdSize + our 2 bytes of length prefix plus an extra byte to avoid compacting
    val bytes = makeBytes(math.max(BufferSize - 4, CompactThresholdSize + 3) + extra)
    
    //println("BEFORE - lengthPrefixed - same buffer / spliced (with existing filled buffer)")
    //println(os.debugInfo)
    
    (0 until extra).foreach{ i => os.write(bytes(i)) }
    
    os.lengthPrefixed(4){
      ((extra + 2) until bytes.length).foreach{ i => os.write(bytes(i)) }
    }{ _ => // We don't care what the length was
      os.write(bytes(extra))
      os.write(bytes(extra+1))
    }
    
    //println("AFTER - lengthPrefixed - same buffer / spliced (with existing filled buffer)")
    //println(os.debugInfo)
    
    os.bufferCount should equal (2)
    os.start should equal (6)
    
    checkEquals(os, bytes)
  }
  
  test("lengthPrefixed - multi buffer / shifting in the original buffer") {
    val os = newFMByteArrayOutputStream()
    
    val bytes = makeBytes(BufferSize * 2 - 2)
    
    // Need to make sure os.available > 0
    os.ensureAvailable(1)
    
    // Need to start writing bytes to make sure there are less than CompactThresholdSize bytes left in our original buffer
    var i = 0
    while(os.available > CompactThresholdSize - 2) {
      os.write(bytes(i))
      i += 1
    }

    //println("BEFORE - lengthPrefixed - multi buffer / shifting in the original buffer")
    //println(os.debugInfo)
    
    val lengthIdx: Int = i
    i += 2
    
    os.lengthPrefixed(4){
      (i until bytes.length).foreach{ i => os.write(bytes(i)) }
    }{ _ => // We don't care what the length was
      os.write(bytes(lengthIdx))
      os.write(bytes(lengthIdx + 1))
    }
    
    //println("AFTER - lengthPrefixed - multi buffer / shifting in the original buffer")
    //println(os.debugInfo)
    
    // There should still be a single buffer
    os.bufferCount should equal (1)
    
    checkEquals(os, bytes)
  }
  
  test("lengthPrefixed - multi buffer / spliced") {
    val os = newFMByteArrayOutputStream()
    
    val bytes = makeBytes(BufferSize * 2 - 2)
    
    os.ensureAvailable(1)
    require(os.available > CompactThresholdSize)
    
    os.lengthPrefixed(4){
      (2 until bytes.length).foreach{ i => os.write(bytes(i)) }
    }{ _ => // We don't care what the length was
      os.write(bytes(0))
      os.write(bytes(1))
    }
    
    //println("AFTER - lengthPrefixed - multi buffer / spliced")
    //println(os.debugInfo)
    
    // There should still be a single buffer
    os.bufferCount should equal (2)
    
    checkEquals(os, bytes)
  }
  
  test("spliced data") {
    val bytes = makeBytes(BufferSize * 4)
    val os = newFMByteArrayOutputStream()
    os.write(bytes, 0, 4)
    os.write(bytes, 4, BufferSize)
    os.totalSize should equal (BufferSize + 4)
    os.write(bytes, BufferSize + 4, BufferSize)
    os.totalSize should equal (BufferSize * 2 + 4)
    os.write(bytes, BufferSize*2+4, BufferSize * 2 - 4)
    os.totalSize should equal (BufferSize * 4)
    checkEquals(os, bytes)
  }
  
  test("lengthPrefixed") {
    val os = newFMByteArrayOutputStream()
    os.write(Array[Byte](1,2,3,4,5,6))
    
    //println("BEFORE - lengthPrefixed")
    //println(os.debugInfo)
    
    os.toByteArray.toVector should equal (Array[Byte](1,2,3,4,5,6))
    os.lengthPrefixed(4){ os.write(Array[Byte](9,10,11,12,13,14,15)) }{ len: Int => os.write(Array[Byte](len.toByte,8)) }
    
    //println("AFTER - lengthPrefixed")
    //println(os.debugInfo)
    
    os.toByteArray.toVector should equal ((1 to 15).map{ _.toByte }.toVector)
  }
  
  test("lengthPrefixed - 0 data") {
    val os = newFMByteArrayOutputStream()
    os.write(Array[Byte](1,2,3,4,5,6))
    //println(os.debugInfo)
    os.toByteArray.toVector should equal (Array[Byte](1,2,3,4,5,6))
    os.lengthPrefixed(4){ /* no data written */ }{ len: Int => os.write(Array[Byte](7,8,9,10)) }
    //println(os.debugInfo)
    os.toByteArray.toVector should equal ((1 to 10).map{ _.toByte }.toVector)
  }
  
  test("lengthPrefixed - nested") {    
    val os = newFMByteArrayOutputStream()
    os.write(Array[Byte](1,2,3,4,5,6))
    
    //println("BEFORE - lengthPrefixed - nested")
    //println(os.debugInfo)
    
    os.lengthPrefixed(4){
      os.write(Array[Byte](9,10,11,12,13,14,15))
      os.lengthPrefixed(4) {
        os.write(Array[Byte](18,19,20,21,22))
      } { len: Int =>
        len should equal(5)
        os.write(Array[Byte](16,17))
      }
    }{ len: Int =>
      len should equal (14)
      os.write(Array[Byte](7,8))
    }
    
    //println("AFTER - lengthPrefixed - nested")
    //println(os.debugInfo)
    
    os.toByteArray.toVector should equal ((1 to 22).map{ _.toByte }.toVector)
  }
  
  test("lengthPrefixed - nested - existing buffer filled") {    
    val os = newFMByteArrayOutputStream()

    (-100 to 0).foreach{ i => os.write(i) }
    
    os.write(Array[Byte](1,2,3,4,5,6))
    
    //println("BEFORE - lengthPrefixed - nested - existing buffer filled")
    //println(os.debugInfo)
    
    os.lengthPrefixed(4){
      os.write(Array[Byte](9,10,11,12,13,14,15))
      os.lengthPrefixed(4) {
        os.write(Array[Byte](18,19,20,21,22))
      } { len: Int =>
        len should equal(5)
        os.write(Array[Byte](16,17))
      }
    }{ len: Int =>
      len should equal (14)
      os.write(Array[Byte](7,8))
    }
    
    //println("AFTER - lengthPrefixed - nested - existing buffer filled")
    //println(os.debugInfo)
    
    os.toByteArray.toVector should equal ((-100 to 22).map{ _.toByte }.toVector)
  }
  
}