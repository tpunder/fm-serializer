package fm.serializer

import scala.annotation.experimental
import scala.quoted.*
import scala.util.Try
import scala.util.control.NonFatal

@experimental
object Macros {
  /**
   * Can enable debugging by setting the FM_SERIALIZER_DEBUG environment variable:
   *
   * FM_SERIALIZER_DEBUG=true sbt
   */
  private val isDebug: Boolean = System.getenv("FM_SERIALIZER_DEBUG") == "true"

  final class StopMacroExpansionWithStackTrace(msg: String) extends Exception(msg)

  private def errorAndAbort(msg: String)(using Quotes): Nothing = {
    if (isDebug) {
      println(s"Macros.errorAndAbort(\"$msg\")")
      throw new StopMacroExpansionWithStackTrace(msg)
    } else {
      quotes.reflect.report.errorAndAbort(msg)
    }
  }
  
  def makeSimpleObjectSerializer[T: Type](using Quotes)(): Expr[SimpleObjectSerializer[T]] = wrap("makeSimpleObjectSerializer", Type.of[T]) {
    '{ SimpleObjectSerializer[T]()(${makeObjectSerializer[T]()}, ${makeObjectDeserializer[T]()}) }
  }

  def makeObjectSerializerFromFields[T: Type](field: Expr[Field], fields: Expr[Seq[Field]])(using Quotes): Expr[ObjectSerializer[T]] = wrap("makeObjectSerializerFromFields", Type.of[T], "...") {
    val helpers: MacroHelpers = MacroHelpers(isDebug)

    fields match {
      case Varargs(exprs: Seq[Expr[Field]]) => helpers.makeObjectSerializer(field +: exprs)
      case _ => errorAndAbort("Varargs match did not work. Bug!")
    }
  }

  def makeObjectDeserializerFromFields[T: Type](field: Expr[Field], fields: Expr[Seq[Field]])(using Quotes): Expr[ObjectDeserializer[T]] = wrap("makeObjectDeserializerFromFields", Type.of[T], "...") {
    val helpers: MacroHelpers = MacroHelpers(isDebug)

    fields match {
      case Varargs(exprs: Seq[Expr[Field]]) => helpers.makeObjectDeserializer(field +: exprs)
      case _ => errorAndAbort("Varargs match did not work. Bug!")
    }
  }

  def makeObjectSerializerForInterface[IFACE: Type, CONCRETE: Type](using Quotes)(): Expr[ObjectSerializer[IFACE]] = wrap("makeObjectSerializerForInterface", Type.of[IFACE], Type.of[CONCRETE]) {
    val helpers: MacroHelpers = MacroHelpers(isDebug)
    import quotes.reflect.*
    helpers.tryMakeObjectSerializerForInterface[IFACE, CONCRETE] getOrElse errorAndAbort(s"Couldn't make ObjectSerializer for interface ${TypeRepr.of[IFACE].show} from concrete type ${TypeRepr.of[CONCRETE].show}")
  }

  def makeObjectSerializer[T: Type](using Quotes)(): Expr[ObjectSerializer[T]] = wrap("makeObjectSerializer", Type.of[T]) {
    val helpers: MacroHelpers = MacroHelpers(isDebug)
    import quotes.reflect.*
    helpers.tryMakeObjectSerializer[T] getOrElse errorAndAbort(s"Couldn't make ObjectSerializer for ${TypeRepr.of[T].show}")
  }
  
  def makeObjectDeserializer[T: Type](using Quotes)(): Expr[ObjectDeserializer[T]] = wrap("makeObjectDeserializer", Type.of[T]) {
    val helpers: MacroHelpers = MacroHelpers(isDebug)
    import quotes.reflect.*
    helpers.tryMakeObjectDeserializer[T] getOrElse errorAndAbort(s"Couldn't make ObjectDeserializer for ${TypeRepr.of[T].show}")
  }

  def makeSerializer[T: Type](using Quotes): Expr[Serializer[T]] = wrap("makeSerializer", Type.of[T]) {
    val helpers: MacroHelpers = MacroHelpers(isDebug)
    import helpers.*
    import quotes.reflect.*

    // The implicit macros seems to take priority over other macros so we first check if there is a non-macro implicit in scope that we can use
    val nonMacroImplicit: Option[Expr[Serializer[T]]] = getImplicit[Serializer[T]]

    nonMacroImplicit orElse findPrimitive[T] orElse findCommonType[T] orElse findOptionSerializer[T] orElse findCollectionSerializer[T] orElse findAnyValSerializer[T] orElse tryMakeObjectSerializer[T] getOrElse this.errorAndAbort(s"Couldn't find Serializer for ${TypeRepr.of[T].show}")
  }
  
  def makeDeserializer[T: Type](using Quotes): Expr[Deserializer[T]] = wrap("makeDeserializer", Type.of[T]) {
    val helpers: MacroHelpers = MacroHelpers(isDebug)
    import helpers.*
    import quotes.reflect.*

    // The implicit macros seems to take priority over other macros so we first check if there is a non-macro implicit in scope that we can use
    val nonMacroImplicit: Option[Expr[Deserializer[T]]] = getImplicit[Deserializer[T]]

    nonMacroImplicit orElse findPrimitive[T] orElse findCommonType[T] orElse findOptionDeserializer[T] orElse findCollectionDeserializer[T] orElse findAnyValDeserializer[T] orElse tryMakeObjectDeserializer[T] getOrElse this.errorAndAbort(s"Couldn't find Deserializer for ${TypeRepr.of[T].show}")
  }

  private def wrap[A: Type, RES](method: String, tpe: Type[A], args: Any*)(f: => Expr[RES])(using Quotes): Expr[RES] = {
    import quotes.reflect.*
    wrapImpl(s"Macros.$method[${TypeRepr.of[A].show}]${args.mkString("(", ", ", ")")}")(f)
  }

  private def wrapImpl[T](msg: String)(f: => Expr[T])(using Quotes): Expr[T] = {
    if (isDebug) println(msg)

    try {
      val res: Expr[T] = f
      if (isDebug) {
        println(s"SUCCESS - $msg - ${Try{ res.show }}")
        quotes.reflect.report.info(msg+" => "+Try{ res.show })
      }
      res
    } catch {
//      case ex: StopMacroExpansionWithStackTrace =>
//        println(s"ERROR: $ex - ${ex.getMessage}")
//        ex.printStackTrace()
//        quotes.reflect.report.errorAndAbort(ex.getMessage)
      //case NonFatal(ex) =>
      case ex: Throwable if isDebug =>
        println(s"ERROR: $ex - ${ex.getMessage}")
        ex.printStackTrace()
        quotes.reflect.report.error(s"ERROR: $ex - ${ex.getMessage}")
        throw ex
    }
  }
}