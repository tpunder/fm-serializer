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

object PrimitiveImplicits extends PrimitiveImplicits

/**
 * These are the default implicits for primitives
 */
trait PrimitiveImplicits {
  implicit val boolean:   BooleanPrimitive    = new BooleanPrimitive()
  implicit val float:     FloatPrimitive      = new FloatPrimitive()
  implicit val double:    DoublePrimitive     = new DoublePrimitive()
  implicit val string:    StringPrimitive     = new StringPrimitive()
  implicit val byteArray: ByteArrayPrimitive  = new ByteArrayPrimitive()
  implicit val int:       IntPrimitive        = new IntPrimitive()
  implicit val long:      LongPrimitive       = new LongPrimitive()
  implicit val char:      CharPrimitive       = new CharPrimitive()

  implicit val javaBigInteger: JavaBigIntegerPrimitive = new JavaBigIntegerPrimitive()
  implicit val javaBigDecimal: JavaBigDecimalPrimitive = new JavaBigDecimalPrimitive()

  implicit val scalaBigInt: ScalaBigIntPrimitive = new ScalaBigIntPrimitive()
  implicit val scalaBigDecimal: ScalaBigDecimalPrimitive = new ScalaBigDecimalPrimitive()

  implicit val immutableByteArray: ImmutableByteArrayPrimitive    = new ImmutableByteArrayPrimitive()
  implicit val immutableIntArray : ImmutableIntArrayDeserializer  = new ImmutableIntArrayDeserializer()
  implicit val immutableLongArray: ImmutableLongArrayDeserializer = new ImmutableLongArrayDeserializer()
  
  import java.lang.{Boolean => JavaBoolean, Character => JavaChar, Float => JavaFloat, Double => JavaDouble, Integer => JavaInt, Long => JavaLong}
  
  implicit val javaBoolean: SimpleSerializer[JavaBoolean] = boolean.map[JavaBoolean](null) { _.booleanValue } { JavaBoolean.valueOf }
  implicit val javaFloat:   SimpleSerializer[JavaFloat]   = float.map[JavaFloat](null)     { _.floatValue   } { JavaFloat.valueOf }
  implicit val javaDouble:  SimpleSerializer[JavaDouble]  = double.map[JavaDouble](null)   { _.doubleValue  } { JavaDouble.valueOf }
  implicit val javaInt:     SimpleSerializer[JavaInt]     = int.map[JavaInt](null)         { _.intValue     } { JavaInt.valueOf }
  implicit val javaLong:    SimpleSerializer[JavaLong]    = long.map[JavaLong](null)       { _.longValue    } { JavaLong.valueOf }
  implicit val javaChar:    SimpleSerializer[JavaChar]    = char.map[JavaChar](null)       { _.charValue    } { JavaChar.valueOf }
}
