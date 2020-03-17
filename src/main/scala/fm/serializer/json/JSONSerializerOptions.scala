/*
 * Copyright 2019 Frugal Mechanic (http://frugalmechanic.com)
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

object JSONSerializerOptions {
  val default: JSONSerializerOptions = JSONSerializerOptions()
  val defaultWithoutNulls: JSONSerializerOptions = default.copy(outputNulls = false)
  val minimal: JSONSerializerOptions = default.copy(outputNulls = false, outputFalse = false, outputZeros = false)
  val pretty: JSONSerializerOptions = default.copy(prettyFormat = true)
  val prettyWithoutNulls: JSONSerializerOptions = pretty.copy(outputNulls = false)
}

/**
 *
 * @param outputNulls Write out fields with null values
 * @param outputFalse Write out boolean fields that are false
 * @param outputZeros Write out fields with numbers that are zero
 * @param prettyFormat Use pretty formatting.
 * @param indent The indent to use for prettyFormat.  Note: Not currently hooked up for Jackson
 */
final case class JSONSerializerOptions(
  outputNulls: Boolean = true,
  outputFalse: Boolean = true,
  outputZeros: Boolean = true,
  prettyFormat: Boolean = false,
  indent: String = "  "
)
