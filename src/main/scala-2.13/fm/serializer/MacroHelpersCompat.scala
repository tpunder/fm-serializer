package fm.serializer

import scala.collection.Factory
import scala.reflect.macros._

object MacroHelpersCompat {
  def canBuildFromOrFactoryType(ctx: Context)(tpe: ctx.Type, elemTpe: ctx.Type): ctx.Type = {
    import ctx.universe._
    appliedType(typeOf[Factory[_,_]], List(elemTpe, tpe))
  }
}