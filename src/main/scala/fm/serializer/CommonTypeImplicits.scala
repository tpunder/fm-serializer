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

import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
import java.util.{Date => JavaDate}

/**
 * These are implicit serializers/deserializers for common types that do not require the use of a macro to generate.
 * 
 * Common types that DO require a macro are embedded into the makeSerializer/makeDeserializer via MacroHelpers.tryCommonType()
 * since we can't call macros from here without creating a separate compilation package.
 */
trait CommonTypeImplicits {
//  implicit val javaBitInteger: SimpleSerializer[JavaBigInteger] = new MappedSimpleSerializer[Array[Byte], JavaBigInteger](Primitive.byteArray) {
//    protected def defaultValue: JavaBigInteger = null 
//    protected def serialize(obj: JavaBigInteger): Array[Byte] = obj.toByteArray
//    protected def deserialize(value: Array[Byte]): JavaBigInteger = new JavaBigInteger(value)
//  }
  
  implicit val javaBigInteger: MappedSimpleSerializer[Array[Byte],JavaBigInteger] = Primitive.byteArray.map(_.toByteArray, new JavaBigInteger(_), null)
  
  implicit val javaDate: MappedSimpleSerializer[Long,JavaDate] = Primitive.long.map(_.getTime, new JavaDate(_), null)
  
//  implicit val javaDate: MappedSimpleSerializer[Long,JavaDate] = new MappedSimpleSerializer[Long,JavaDate](Primitive.long, new Mapper[Long,JavaDate]{
//    def defaultValue: JavaDate = null
//    def serialize(obj: JavaDate): Long = obj.getTime
//    def deserialize(value: Long): JavaDate = new JavaDate(value)
//  })
//  
//  implicit object javaDate extends SimpleSerializer[JavaDate] {
//    private[this] val orig: LongPrimitive = Primitive.long
//    
//    final def serializeRaw(output: RawOutput, v: JavaDate): Unit = if (null != v) orig.serializeRaw(output, v.getTime)
//    final def serializeNested(output: NestedOutput, v: JavaDate): Unit = if (null != v) orig.serializeNested(output, v.getTime)
//    final def serializeField(output: FieldOutput, number: Int, name: String, v: JavaDate): Unit = if (null != v) orig.serializeField(output, number, name, v.getTime)
//    
//    final def deserializeRaw(input: RawInput): JavaDate = new JavaDate(orig.deserializeRaw(input))
//    final def deserializeNested(input: NestedInput): JavaDate = new JavaDate(orig.deserializeNested(input))
//    
//    final def defaultValue: JavaDate = null
//  }
  
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
  import org.joda.time.LocalDate
  
  implicit val jodaLocalDate: MappedSimpleSerializer[String, LocalDate] = Primitive.string.map(_.toString, LocalDate.parse(_), null)
}