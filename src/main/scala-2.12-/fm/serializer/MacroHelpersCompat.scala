package fm.serializer

import scala.collection.generic.CanBuildFrom
import scala.reflect.macros._

object MacroHelpersCompat {
  def canBuildFromOrFactoryType(ctx: Context)(tpe: ctx.Type, elemTpe: ctx.Type): ctx.Type = {
    import ctx.universe._
    appliedType(typeOf[CanBuildFrom[_,_,_]], List(WildcardType, elemTpe, tpe))
  }
}