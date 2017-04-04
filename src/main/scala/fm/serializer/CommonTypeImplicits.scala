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

import fm.common.IP
import java.io.File
import java.math.{BigInteger => JavaBigInteger}
import java.util.Calendar

/**
 * These are implicit serializers/deserializers for common types that do not require the use of a macro to generate.
 * 
 * Common types that DO require a macro are embedded into the makeSerializer/makeDeserializer via MacroHelpers.tryCommonType()
 * since we can't call macros from here without creating a separate compilation package.
 */
trait CommonTypeImplicits {

  implicit val javaFile: MappedSimpleSerializer[String,File] = Primitive.string.map(_.toString, new File(_), null)
  implicit val javaBigInteger: MappedSimpleSerializer[Array[Byte],JavaBigInteger] = Primitive.byteArray.map(_.toByteArray, new JavaBigInteger(_), null)

  implicit val ip: IPSerializer = new IPSerializer()

  // Handled by the BSON Implicits
  //implicit val javaDate: MappedSimpleSerializer[Long,Date] = Primitive.long.map(_.getTime, new Date(_), null)

  implicit val javaCalendar: MappedSimpleSerializer[Long,Calendar] = Primitive.long.map(_.getTime.getTime, toCalendar, null)
  
  private def toCalendar(millis: Long): Calendar = {
    val calendar: Calendar = Calendar.getInstance()
    calendar.setTimeInMillis(millis)
    calendar
  }
  
  //
  // javax.xml stuff
  //
  import javax.xml.datatype.{DatatypeFactory, Duration, XMLGregorianCalendar}
  
  // The JavaDocs don't specify if this is thread-safe or not so let's play it safe
  private[this] val xmlDatatypeFactory: ThreadLocal[DatatypeFactory] = new ThreadLocal[DatatypeFactory] { override protected def initialValue(): DatatypeFactory = DatatypeFactory.newInstance() }
  
  implicit val xmlGregorianCalendar: MappedSimpleSerializer[String, XMLGregorianCalendar] = Primitive.string.map(_.toXMLFormat, xmlDatatypeFactory.get.newXMLGregorianCalendar(_), null)
  implicit val xmlDuration: MappedSimpleSerializer[String, Duration] = Primitive.string.map(_.toString, xmlDatatypeFactory.get.newDuration(_), null)
  
  //
  // Joda Time
  //  
  implicit val jodaLocalDate: MappedSimpleSerializer[String, org.joda.time.LocalDate] = Primitive.string.map(_.toString, org.joda.time.LocalDate.parse(_), null)
  
  //
  // java.time
  //
  implicit val javaLocalDate: MappedSimpleSerializer[String, java.time.LocalDate] = Primitive.string.map(_.toString, java.time.LocalDate.parse(_), null)
  
  // NOTE: CANNOT CURRENTLY USE THIS VERSION SINCE WE HAVE EXISTING CODE THAT USES THE STRING BASED VERSION
//  implicit val javaLocalDate: MappedSimpleSerializer[Int,LocalDate] = Primitive.int.map(localDateToInt, intToLocalDate, null)
//  
//  // Converts to: yyyymmdd
//  private def localDateToInt(d: LocalDate): Int = d.getYear * 10000 + d.getMonthValue * 100 + d.getDayOfMonth
//  
//  // Parses from: yyyymmdd
//  private def intToLocalDate(n: Int): LocalDate = {
//    val year: Int = n / 10000
//    val month: Int = n / 100 % 100
//    val dayOfMonth: Int = n % 100
//    LocalDate.of(year, month, dayOfMonth)
//  }
}

final class IPSerializer extends SimpleSerializer[IP] {
  // Note: We are using a Long to represent the IPv4 to avoid any Int32 signed issues
  //       and specifically to make ranged checks work on IPs that are stored in a
  //       database (e.g. MongoDB).
  private[this] val primitive: LongPrimitive = Primitive.long

  def serializeRaw(output: RawOutput, v: IP): Unit = primitive.serializeRaw(output, v.longValue)
  def serializeNested(output: NestedOutput, v: IP): Unit = primitive.serializeNested(output, v.longValue)
  def serializeField(output: FieldOutput, number: Int, name: String, v: IP): Unit = primitive.serializeField(output, number, name, v.longValue)

  def defaultValue: IP = IP.empty
  def deserializeRaw(input: RawInput): IP = IP(primitive.deserializeRaw(input))
  def deserializeNested(input: NestedInput): IP = IP(primitive.deserializeNested(input))
}