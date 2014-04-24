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

/**
 * Generic Output trait to be implemented by Serialization Implementations
 * 
 * There are 3 classes of outputs:
 *  - RAW
 *  - NESTED
 *  - FIELD
 *  
 * RAW
 *  Raw output is what you get if you serialize something by itself.  Depending on the serialization implementation it will
 *  probably have an implicit length determined by the length of an Array[Byte], String, InputStream, etc.  The starting point
 *  for serializing something it usually invoking one of the writeRawXXX(...) methods.  The writeRawXXX(...) methods should be implemented
 *  by all serialization implementations.
 *  
 * NESTED
 *  Nested output is what we use when something is serialized as part of something else and may or may not be different than RAW output
 *  depending on the serialization implementation.  For example, when serializing a collection each element would be serialized using
 *  the writeNestedXXX(...) methods.  The nested format might have additional length information compared to the RAW format since there
 *  is no implicit length.  For example, in protocol buffers a string/object/collection is prefixed with its length.  Most serialization
 *  implementations can probably write optional length information followed by calling the corresponding writeRawXXX(...) method.
 *  
 *  Another way to think about nested output is what we should be able to deserialize a NESTED value that is in the middle of an array of
 *  bytes (or a string or whatever).  This means we need to know when to stop reading the value.  For something like Protocol Buffers we
 *  will be prepending the length for string/object/repeated field or have a marker bit for varints to know when to stop.  For something like
 *  JSON we will hit a double-quote (for strings) for a comma or closing brace (for all other types).
 *  
 * FIELD
 *  Field output is used when writing fields of an object.  In addition to the value we are serializing it contains the name/number of the field
 *  in the object.  Most implementations will probably write out the field name/number information followed by a call to the corresponding
 *  writeNestedXXX(...) method.  Some implementations, such as Protocol Buffers, writes out the type of the field as part of the name/number
 *  which is why there isn't just a writeFieldName(...) which the framework would call automatically followed by the appropriate writeNestedXXX(...).
 *  
 *  NOTE - Reading field output (via Input) is broken into a readFieldNumber() call to get the name/number of the field followed by calls to readNestedXXX(). 
 *  
 * Things are broken out this way to mainly support more complex formats (like Protocol Buffers).  For something like a JSON implementation the 
 * RAW and NESTED formats will probably be the same.  The way in which we write out JSON fields as part of an object will also be the same no matter
 * what the type is unlike something like Protocol Buffers which needs to encode the type of field as part of the name/number of the field.
 */
trait Output extends FieldOutput with NestedOutput with RawOutput {

}
