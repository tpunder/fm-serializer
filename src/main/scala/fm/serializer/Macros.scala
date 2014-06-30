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

import scala.reflect.macros._

object Macros {
  private val isDebug: Boolean = false
  
  def makeSimpleObjectSerializer[T: c.WeakTypeTag](c: Context)(): c.Expr[SimpleObjectSerializer[T]] = wrap(c, s"makeSimpleObjectSerializer[${c.weakTypeOf[T]}]") {
    
    c.universe.reify { SimpleObjectSerializer[T]()(makeObjectSerializer[T](c).splice, makeObjectDeserializer[T](c).splice) }
  }
  
  def makeObjectSerializerFromFields[T: c.WeakTypeTag](c: Context)(field: c.Expr[Field], fields: c.Expr[Field]*): c.Expr[ObjectSerializer[T]] = wrap(c, s"makeObjectSerializer[${c.weakTypeOf[T]}]") {
    object helpers extends MacroHelpers(isDebug){ val ctx: c.type = c }
    helpers.makeObjectSerializer((Vector(field)++fields).map{ helpers.makeFieldImpl })
  }
  
  def makeObjectDeserializerFromFields[T: c.WeakTypeTag](c: Context)(field: c.Expr[Field], fields: c.Expr[Field]*): c.Expr[ObjectDeserializer[T]] = wrap(c, s"makeObjectDeserializer[${c.weakTypeOf[T]}]") {
    object helpers extends MacroHelpers(isDebug){ val ctx: c.type = c }
    helpers.makeObjectDeserializer((Vector(field)++fields).map{ helpers.makeFieldImpl })
  }
  
  def makeObjectSerializerForInterface[IFACE: c.WeakTypeTag, CONCRETE: c.WeakTypeTag](c: Context)(): c.Expr[ObjectSerializer[IFACE]] = wrap(c, s"makeObjectSerializerForInterface[${c.weakTypeOf[IFACE]},${c.weakTypeOf[CONCRETE]}]") {
    object helpers extends MacroHelpers(isDebug){ val ctx: c.type = c }
    import helpers._
    val ifaceTpe: c.Type = c.weakTypeOf[IFACE]
    val concreteTpe: c.Type = c.weakTypeOf[CONCRETE]
    tryMakeObjectSerializerForInterface[IFACE, CONCRETE] getOrElse { c.abort(c.enclosingPosition, s"Couldn't make ObjectSerializer for interface $ifaceTpe from concrete type $concreteTpe") }
  }
  
  def makeObjectSerializer[T: c.WeakTypeTag](c: Context)(): c.Expr[ObjectSerializer[T]] = wrap(c, s"makeObjectSerializer[${c.weakTypeOf[T]}]") {
    object helpers extends MacroHelpers(isDebug){ val ctx: c.type = c }
    import helpers._
    val tpe: c.Type = c.weakTypeOf[T]
    tryMakeObjectSerializer[T] getOrElse { c.abort(c.enclosingPosition, s"Couldn't make ObjectSerializer for $tpe") }
  }
  
  def makeObjectDeserializer[T: c.WeakTypeTag](c: Context)(): c.Expr[ObjectDeserializer[T]] = wrap(c, s"makeObjectDeserializer[${c.weakTypeOf[T]}]") {
    object helpers extends MacroHelpers(isDebug){ val ctx: c.type = c }
    import helpers._
    val tpe: c.Type = c.weakTypeOf[T]
    tryMakeObjectDeserializer[T] getOrElse { c.abort(c.enclosingPosition, s"Couldn't make ObjectDeserializer for $tpe") }
  }
  
  def makeSerializerNoImplicits[T: c.WeakTypeTag](c: Context): c.Expr[Serializer[T]] = wrap(c, s"makeSerializerNoImplicits[${c.weakTypeOf[T]}]") {
    makeSerializer[T](allowImplicits = false)(c)
  }
  
  def makeSerializerAllowImplicits[T: c.WeakTypeTag](c: Context): c.Expr[Serializer[T]] = wrap(c, s"makeSerializerAllowImplicits[${c.weakTypeOf[T]}]") {
    makeSerializer[T](allowImplicits = true)(c)
  }
  
  def makeSerializer[T: c.WeakTypeTag](allowImplicits: Boolean)(c: Context): c.Expr[Serializer[T]] = wrap(c, s"makeSerializer[${c.weakTypeOf[T]}]") {
    
    object helpers extends MacroHelpers(isDebug){ val ctx: c.type = c }
    import helpers._
    import c.universe._
    
    val tpe: c.Type = c.weakTypeOf[T]

    // The implicit macros seems to take priority over other macros so we first check if there is a non-macro implicit in scope that we can use
    val implicitTpe: Type = appliedType(typeOf[Serializer[_]], List(tpe))
    val nonMacroImplicit: Option[c.Expr[Serializer[T]]] = getImplicit(implicitTpe, withMacrosDisabled = true).map{ c.Expr[Serializer[T]](_) }
    
    nonMacroImplicit orElse findCommonType[T] orElse findOptionSerializer[T] orElse findCollectionSerializer[T] orElse tryMakeObjectSerializer[T] getOrElse { c.abort(c.enclosingPosition, s"Couldn't find Serializer for $tpe") }
  }
  
  def makeDeserializer[T: c.WeakTypeTag](c: Context): c.Expr[Deserializer[T]] = wrap(c, s"makeDeserializer[${c.weakTypeOf[T]}]") {
    object helpers extends MacroHelpers(isDebug){ val ctx: c.type = c }
    import helpers._
    import c.universe._
    
    val tpe: c.Type = c.weakTypeOf[T]
    
    // The implicit macros seems to take priority over other macros so we first check if there is a non-macro implicit in scope that we can use
    val implicitTpe: Type = appliedType(typeOf[Deserializer[_]], List(tpe))
    val nonMacroImplicit: Option[c.Expr[Deserializer[T]]] = getImplicit(implicitTpe, withMacrosDisabled = true).map{ c.Expr[Deserializer[T]](_) }
    
    nonMacroImplicit orElse findCommonType[T] orElse findOptionDeserializer[T] orElse findCollectionDeserializer[T] orElse tryMakeObjectDeserializer[T] getOrElse { c.abort(c.enclosingPosition, s"Couldn't find Deserializer for $tpe") }
  }
  
  private def wrap[T](c: Context, msg: String)(f: => T): T = {
    try {
      val res: T = f
      if (isDebug) c.info(c.enclosingPosition, msg+" => "+res, true)
      res
    } catch {
      case ex: Throwable =>
        println("ERROR: "+ex)
        ex.printStackTrace()
        c.error(c.enclosingPosition, msg+" - ERROR: "+ex)
        throw ex
    }
  }
}