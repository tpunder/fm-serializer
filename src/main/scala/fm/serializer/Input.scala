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
 * Generic Input trait to be implemented by Serialization Implementations
 * 
 * See the docs for Output for the distinction between RAW, NESTED, and FIELD Input/Output.  The only difference
 * for Input is that there aren't readNestedXXX() methods.  Instead the way fields for objects are read is:
 *  - readFieldNumber(...) to get the name/number of the field
 *  - readNestedXXX() to get the value of the field (based on looking up the proper type given the name/number)
 * 
 */
trait Input extends FieldInput with CollectionInput with NestedInput with RawInput {
  
}