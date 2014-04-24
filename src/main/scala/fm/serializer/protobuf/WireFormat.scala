// Protocol Buffers - Google's data interchange format
// Copyright 2008 Google Inc.  All rights reserved.
// http://code.google.com/p/protobuf/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
package fm.serializer.protobuf

object WireFormat {
  final val WIRETYPE_VARINT           = 0
  final val WIRETYPE_FIXED64_LE       = 1 // Little Endian
  final val WIRETYPE_LENGTH_DELIMITED = 2
  final val WIRETYPE_START_GROUP      = 3
  final val WIRETYPE_END_GROUP        = 4
  //final val WIRETYPE_FIXED64_BE      = 3 // Big Endian -- Was WIRETYPE_START_GROUP
  //final val WIRETYPE_FIXED32_BE      = 4 // Big Endian -- Was WIRETYPE_END_GROUP
  final val WIRETYPE_FIXED32_LE      = 5 // Little Endian
  final val WIRETYPE_NULL            = 6 // Non-Standard -- For NULL values
  
  final val TAG_TYPE_BITS: Int = 3
  final val TAG_TYPE_MASK: Int = (1 << TAG_TYPE_BITS) - 1 // This is 7 when TAG_TYPE_BITS is 3
  
  /** Given a tag value, determines the wire type (the lower 3 bits). */
  @inline final def getTagWireType(tag: Int): Int = tag & TAG_TYPE_MASK
  
  /** Given a tag value, determines the field number (the upper 29 bits). */
  @inline final def getTagFieldNumber(tag: Int): Int = tag >>> TAG_TYPE_BITS
  
  /** Makes a tag value given a field number and wire type. */
  @inline final def makeTag(fieldNumber: Int, wireType: Int): Int = (fieldNumber << TAG_TYPE_BITS) | wireType
}