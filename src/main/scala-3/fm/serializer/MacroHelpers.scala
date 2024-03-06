package fm.serializer

import fm.common.Implicits.*
import java.lang.Iterable as JavaIterable
import java.util.Collection as JavaCollection
import scala.annotation.{experimental, targetName}
import scala.collection.Factory
import scala.collection.mutable.Growable
import scala.quoted.*
import scala.reflect.ClassTag
import scala.util.Try

@experimental
final case class MacroHelpers(isDebug: Boolean)(using Quotes) {
  import quotes.reflect.*

  private class StacklessIllegalArgumentException(msg: String) extends IllegalArgumentException(msg) {
    override def fillInStackTrace(): Throwable = this
  }
  
  def errorAndAbort(msg: String)(using Quotes): Nothing = {
    if (isDebug) {
      println(s"MacroHelpers.errorAndAbort(\"$msg\")")
      throw new Macros.StopMacroExpansionWithStackTrace(msg)
    } else {
      quotes.reflect.report.errorAndAbort(msg)
    }
  }

  private inline def log(inline s: => Any): Unit = if (isDebug) println(s.toString)

  private case class ObjectSerializationInfo[T: Type](fields: Vector[FieldImpl]) {
    private val objTpe: TypeRepr = TypeRepr.of[T]

    private val sharedSerializers: Vector[(TypeRepr, Option[String])] = {
      fields.filter{ _.serializer == null }.map{ _.tpe }.toSet.toVector.map{ (tpe: TypeRepr) =>
        // If the field type is the same as the object type leave the name empty so we reference this serializer
        val name: Option[String] = if (tpe =:= objTpe) None else Some(Symbol.freshName("shared_ser"))
        (tpe, name)
      }
    }

    val infos: Vector[FieldSerializationInfo[T]] = fields.map{ new FieldSerializationInfo[T](_, sharedSerializers) }

    private val (sharedSerializerDecls: (Symbol => List[Symbol]), sharedSerializerValDefs: (Symbol => List[ValDef])) = {
      val res: Vector[((Symbol => Symbol), (Symbol => ValDef))] = sharedSerializers.collect{ case (tpe: TypeRepr, Some(name: String)) =>
        // Use a more specific type than fm.serializer.Serializer[$tpe] if we know it.
        // We can also use a non-lazy if the implicit is a non-macro
        type S
        given Type[S] = tpe.asType.asInstanceOf[Type[S]]

        def valDecl(cls: Symbol): Symbol = {
          // private[this] lazy val $name: fm.serializer.Serializer[$tpe] = implicitly[fm.serializer.Serializer[$tpe]]
          Symbol.newVal(cls, name, TypeRepr.of[Serializer[S]], /*Flags.Private |*/ Flags.Lazy, Symbol.noSymbol)
        }

        def valDef(cls: Symbol): ValDef = {
          ValDefQuotes(cls.declaredField(name), Some('{ scala.compiletime.summonInline[Serializer[S]] }.asTerm))
        }

//        def valDecl(cls: Symbol): Symbol = {
//          getImplicit[Serializer[S]] match {
//            case Some(expr) =>
//              // private[this] val $name: ${tree.tpe} = $tree
//              Symbol.newVal(cls, name, expr.asTerm.tpe, /*Flags.Private*/ Flags.EmptyFlags, Symbol.noSymbol)
//
//            case None =>
//              // private[this] lazy val $name: fm.serializer.Serializer[$tpe] = implicitly[fm.serializer.Serializer[$tpe]]
//              Symbol.newVal(cls, name, TypeRepr.of[Serializer[S]], /*Flags.Private |*/ Flags.Lazy, Symbol.noSymbol)
//          }
//        }
//
//        def valDef(cls: Symbol): ValDef = {
//          val fieldSym: Symbol = cls.declaredField(name)
//
//          // Note: The field needs to be the owner for the implicit expr
//          fieldSym.usingQuotes{
//            getImplicit[Serializer[S]] match {
//              case Some(expr) => ValDef(fieldSym, Some(expr.asTerm.changeOwner(fieldSym)))
//              case None       => ValDefQuotes(fieldSym, Some('{ scala.compiletime.summonInline[Serializer[S]] }.asTerm))
//            }
//          }
//        }

        (valDecl, valDef)

//        getImplicit[Serializer[S]](withMacrosDisabled = true) match {
//          case Some(expr) =>
//            // private[this] val $name: ${tree.tpe} = $tree
//            def valDecl(cls: Symbol): Symbol = Symbol.newVal(cls, name, expr.asTerm.tpe, /*Flags.Private*/ Flags.EmptyFlags, Symbol.noSymbol)
//            def valDef(cls: Symbol): ValDef = ValDef(cls.declaredField(name), Some(expr.asTerm.changeOwner(cls.declaredField(name)))) // Note: the ValDef symbol must own the implicit expr
//            (valDecl, valDef)
//          case None =>
//            // private[this] lazy val $name: fm.serializer.Serializer[$tpe] = implicitly[fm.serializer.Serializer[$tpe]]
//            def valDecl(cls: Symbol): Symbol = Symbol.newVal(cls, name, TypeRepr.of[Serializer[S]], /*Flags.Private |*/ Flags.Lazy, Symbol.noSymbol)
//            def valDef(cls: Symbol): ValDef = ValDefQuotes(cls.declaredField(name), Some('{ scala.compiletime.summonInline[Serializer[S]] }.asTerm))
//            (valDecl, valDef)
//        }
      }

      ((cls: Symbol) => res.map{ _._1(cls) }.toList, (cls: Symbol) => res.map{ _._2(cls) }.toList )
    }

    def serializerDeclarations(cls: Symbol): List[Symbol] = sharedSerializerDecls(cls) ++ infos.flatMap{ _.serializerValSym(cls) }
    def serializerDefinitions(cls: Symbol): List[ValDef] = sharedSerializerValDefs(cls) ++ infos.flatMap{ _.serializerValDef(cls) }

    def writes(cls: Symbol, output: Expr[FieldOutput], instance: Expr[T]): List[Statement] = infos.toList.map{ (field: FieldSerializationInfo[T]) =>
      val fieldNumber: Term = Literal(IntConstant(field.number))
      val fieldName: Term = Literal(StringConstant(field.name))
      val fieldAccessor: Term = field.fieldAccessor(instance.asTerm)

      field.serializerTermName match {
        case None =>
          // serializeField(output, ${field.number}, ${field.name}, ${field.fieldAccessor})
          Apply(Select.unique(Ident(cls.termRef), "serializeField"), List(output.asTerm, fieldNumber, fieldName, fieldAccessor))
        case Some(name: String) =>
          // ${name}.serializeField(output, ${field.number}, ${field.name}, ${field.fieldAccessor})
          Apply(Select.unique(Select.unique(Ident(cls.termRef), name), "serializeField"), List(output.asTerm, fieldNumber, fieldName, fieldAccessor))
      }
    }
  }

  private case class ObjectDeserializationInfo[T: Type](fields: Vector[FieldImpl]) { outer =>
    private val objTpe: TypeRepr = TypeRepr.of[T]

    private val sharedDeserializers: Vector[(TypeRepr, Option[String])] = {
      fields.filter{ _.deserializer == null }.map{ _.tpe }.toSet.toVector.map{ (tpe: TypeRepr) =>
        // If the field type is the same as the object type leave the name empty so we reference this serializer
        val name: Option[String] = if (tpe =:= objTpe) None else Some(Symbol.freshName("shared_deser"))
        (tpe, name)
      }
    }

    val infos: Vector[FieldDeserializationInfo[T]] = fields.map{ new FieldDeserializationInfo[T](_, sharedDeserializers) }

    private val (sharedDeserializerDecls: (Symbol => List[Symbol]), sharedDeserializerValDefs: (Symbol => List[ValDef])) = {
      val res: Vector[((Symbol => Symbol), (Symbol => ValDef))] = sharedDeserializers.collect{ case (tpe: TypeRepr, Some(name: String)) =>
        // Use a more specific type than fm.serializer.Deserializer[$tpe] if we know it.
        // We can also use a non-lazy if the implicit is a non-macro
        type S
        given Type[S] = tpe.asType.asInstanceOf[Type[S]]

        def valDecl(cls: Symbol): Symbol = {
          // private[this] lazy val $name: fm.serializer.Deserializer[$tpe] = implicitly[fm.serializer.Deserializer[$tpe]]
          Symbol.newVal(cls, name, TypeRepr.of[Deserializer[S]], /*Flags.Private |*/ Flags.Lazy, Symbol.noSymbol)
        }

        def valDef(cls: Symbol): ValDef = {
          ValDefQuotes(cls.declaredField(name), Some('{ scala.compiletime.summonInline[Deserializer[S]] }.asTerm))
        }

//        def valDecl(cls: Symbol): Symbol = {
//          getImplicit[Deserializer[S]] match {
//            case Some(expr) =>
//              // private[this] val $name: ${tree.tpe} = $tree
//              Symbol.newVal(cls, name, expr.asTerm.tpe, /*Flags.Private*/ Flags.EmptyFlags, Symbol.noSymbol)
//
//            case None =>
//              // private[this] lazy val $name: fm.serializer.Deserializer[$tpe] = implicitly[fm.serializer.Deserializer[$tpe]]
//              Symbol.newVal(cls, name, TypeRepr.of[Deserializer[S]], /*Flags.Private |*/ Flags.Lazy, Symbol.noSymbol)
//          }
//        }
//
//        def valDef(cls: Symbol): ValDef = {
//          val fieldSym: Symbol = cls.declaredField(name)
//
//          // Note: The field needs to be the owner for the implicit expr
//          fieldSym.usingQuotes{
//            getImplicit[Deserializer[S]] match {
//              case Some(expr) => ValDef(cls.declaredField(name), Some(expr.asTerm.changeOwner(cls.declaredField(name)))) // Note: The ValDef symbol must own the implicit expr
//              case None       => ValDefQuotes(cls.declaredField(name), Some('{ scala.compiletime.summonInline[Deserializer[S]] }.asTerm))
//            }
//          }
//        }

        (valDecl, valDef)

//        getImplicit[Deserializer[S]](withMacrosDisabled = true) match {
//          case Some(expr) =>
//            // private[this] val $name: ${tree.tpe} = $tree
//            def valDecl(cls: Symbol): Symbol = Symbol.newVal(cls, name, expr.asTerm.tpe, /*Flags.Private*/ Flags.EmptyFlags, Symbol.noSymbol)
//            def valDef(cls: Symbol): ValDef = ValDef(cls.declaredField(name), Some(expr.asTerm.changeOwner(cls.declaredField(name)))) // Note: The ValDef symbol must own the implicit expr
//            (valDecl, valDef)
//          case None =>
//            // private[this] lazy val $name: fm.serializer.Deserializer[$tpe] = implicitly[fm.serializer.Deserializer[$tpe]]
//            def valDecl(cls: Symbol): Symbol = Symbol.newVal(cls, name, TypeRepr.of[Deserializer[S]], /*Flags.Private |*/ Flags.Lazy, Symbol.noSymbol)
//            def valDef(cls: Symbol): ValDef = ValDefQuotes(cls.declaredField(name), Some('{ scala.compiletime.summonInline[Deserializer[S]] }.asTerm))
//            (valDecl, valDef)
//        }
      }

      ((cls: Symbol) => res.map{ _._1(cls) }.toList, (cls: Symbol) => res.map{ _._2(cls) }.toList )
    }

    def deserializerDeclarations(cls: Symbol): List[Symbol] = sharedDeserializerDecls(cls) ++ infos.flatMap{ _.deserializerValSym(cls) }
    def deserializerDefinitions(cls: Symbol): List[ValDef] = sharedDeserializerValDefs(cls) ++ infos.flatMap{ _.deserializerValDef(cls) }

    // For toPrettyString() and hasMatchingConstructor
    private def ctorFields: Vector[FieldDeserializationInfo[T]] = infos.filter{ _.field.constructorIdx >= 0 }.sortBy{ _.field.constructorIdx }
    def hasMatchingConstructor: Boolean = hasNoArgsConstructor[T] || hasConstructorWithSignature[T](ctorFields.map{ _.tpe })

    def forClassAndMethod(cls: Symbol, method: Symbol): ForClassAndMethod = new ForClassAndMethod(cls, method)

    class ForClassAndMethod(cls: Symbol, method: Symbol) {
      def info: ObjectDeserializationInfo[T] = outer
      val infos: Vector[FieldDeserializationInfo[T]#ForClassAndMethod] = outer.infos.map{ _.forClassAndMethod(cls, method) }

      def readCases(input: Expr[FieldInput]): Vector[CaseDef] = infos.map{ (field: FieldDeserializationInfo[T]#ForClassAndMethod) =>
        val deser: Term = field.deserializerTermName match {
          case None =>
          // deserializeNested(input)
          Apply(Select.unique(Ident(cls.termRef), "deserializeNested"), List(input.asTerm))

          case Some(name: String) =>
          // ${name}.deserializeNested(input)
          Apply(Select.unique(Select.unique(Ident(cls.termRef), name), "deserializeNested"), List(input.asTerm))
        }

        //  cq"""${field.number} =>
        //    ${field.readVarName} = $deser
        //    ${field.isSetVarName} = true
        //  """

        val code: Block = Block(List(
          Assign(Ident(field.readVarSym.termRef), deser),
          Assign(Ident(field.isSetVarSym.termRef), Literal(BooleanConstant(true)))
        ), Literal(UnitConstant()))

        CaseDef(Literal(IntConstant(field.number)), None, code)
      }

      def isSetVars: Vector[ValDef] = infos.map{ _.isSetVarDef }
      def readVars: Vector[ValDef] = infos.map{ _.readVarDef }

      def setDefaultValuesForNonSetVariables(input: Expr[FieldInput])(using Quotes): Vector[If] = infos.map{ (f: FieldDeserializationInfo[T]#ForClassAndMethod) =>
        val deserializer: Term = f.deserializerTermName match {
          case None => Literal(NullConstant())
          case Some(name: String) => Select.unique(Ident(cls.termRef), name)
        }

        //  if (!${f.isSetVarName}) {
        //    ${f.readVarName} = ${f.defaultValue}
        //    input.reportUnsetField(${f.number}, ${f.name}, ${f.hasUserDefinedDefaultValue}, ${deserializer})
        //  }

        val body: Block = Block(List(
          Assign(Ident(f.readVarSym.termRef), f.defaultValue),
          Apply(TypeApply(Select.unique(input.asTerm, "reportUnsetField"), List(f.tpe.tpt)), List(Literal(IntConstant(f.number)), Literal(StringConstant(f.name)), Literal(BooleanConstant(f.hasUserDefinedDefaultValue)), deserializer)),
        ), Literal(UnitConstant()))

        If('{ !${Ident(f.isSetVarSym.termRef).asExprOf[Boolean]} }.asTerm, body, Literal(UnitConstant()))
      }

      def ctorFields: Vector[FieldDeserializationInfo[T]#ForClassAndMethod] = infos.filter{ _.field.constructorIdx >= 0 }.sortBy{ _.field.constructorIdx }
      def ctorParams: List[Term] = ctorFields.map{ (f: FieldDeserializationInfo[T]#ForClassAndMethod) => Ident(f.readVarSym.termRef) }.toList

      def nonCtorSetters(obj: Term): Vector[Statement] = infos.filter{ _.field.setter != null }.map{ (f: FieldDeserializationInfo[T]#ForClassAndMethod) =>
        val setterName: String = f.field.setter

        val stmt: Statement = getSingleArgMethod[T](setterName).map{ (method: Symbol) =>
          //
          // This is the normal case where we have something like setName(...)
          //
          // obj.${method}(${f.readVarName})
          Apply(Select(obj, method), List(Ident(f.readVarSym.termRef)))
        }.orElse { getNoArgsMethodOrField[T](setterName).map{ (method: Symbol) =>
          //
          // This is the special case where we have a field.setter that is really a getter
          // that returns something like a java.util.List that has an addAll method that
          // we must use to set the value
          //
          require(method.returnType =:= f.tpe, s"Expected the return type of $setterName to be ${f.tpe}")

          type TMP
          given Type[TMP] = method.returnType.asType.asInstanceOf[Type[TMP]]

          getSingleArgMethod[TMP]("addAll") match {
            case None => errorAndAbort(s"Expected ${method.returnType} to have a method called addAll")
            case Some(addAll: Symbol) =>
              // obj.${method}().addAll(${f.readVarName})
              Apply(Select(Apply(Select(obj, method), List()), addAll), List(Ident(f.readVarSym.termRef)))
          }
        }}.getOrElse{ throw new StacklessIllegalArgumentException(s"Invalid Setter (${setterName}) for $objTpe") }

        stmt
      }
    }

    def toPrettyString(): String = {
      val sb: StringBuilder = new StringBuilder()

      sb += '\n'
      sb ++= "===================================================================================================================================="
      sb += '\n'
      sb ++= s"Type: $objTpe\n"
      sb += '\n'
      sb ++= s"ALL Fields:\n\n"
      fields.foreach { (field: FieldImpl) =>  sb ++= s"  $field\n" }
      sb += '\n'
      sb ++= s"Constructor Fields:\n\n"
      ctorFields.foreach { (field: FieldDeserializationInfo[T]) =>  sb ++= s"  ${field.field}\n" }
      sb += '\n'
      sb ++= "Looking for Constructor:\n\n"
      sb ++= s"  List(${ctorFields.toList.map{ _.tpe }})\n"
      sb += '\n'
      sb ++= "Detected Constructors:\n\n"
      constructors[T].foreach{ (method: Symbol) => sb ++= s"  ${method.paramSymss}\n" }
      sb += '\n'
      sb ++= "(Note: Generated by ObjectDeserializationInfo.toPrettyString.  Modify this method to add additional information as needed.)\n"
      sb ++= "===================================================================================================================================="
      sb += '\n'
      sb.toString
    }
  }

  private case class FieldSerializationInfo[T: Type](field: FieldImpl, sharedSerializers: Vector[(TypeRepr, Option[String])]) {
    def number: Int = field.number
    def name: String = field.name
    def tpe: TypeRepr = field.tpe

    require(number > 0, "number must be > 0")
    require(null != name && name.length > 0, "name is empty")
    require(null != tpe, "tpe is null")

    def fieldAccessor(instance: Term): Term = {
      require(field.getter != null, s"FieldImpl.getter is null for FieldImpl: $field")
      val getter: Symbol = noArgsMethodOrField[T](field.getter)
      val noArgs: Boolean = getter.paramSymss.isEmpty

      if (noArgs) Select(instance, getter) // instance.getter q"obj.${getter}"
      else Apply(Select(instance, getter), Nil) // instance.getter()
    }

    val (serializerTermName: Option[String], serializerValSym: (Symbol => Option[Symbol]), serializerValDef: (Symbol => Option[ValDef])) = {
      if (field.serializer != null) {
        // We are using a custom serializer and need a valdef
        val valName: String = Symbol.freshName(s"ser_${number}_${name}")
        // lazy val $valName: ${field.serializer.tpe} = ${field.serializer}

        def valSym(cls: Symbol): Option[Symbol] = Some(Symbol.newVal(cls, valName, field.serializer.tpe, Flags.Lazy, Symbol.noSymbol))
        def valDef(cls: Symbol): Option[ValDef] = Some(ValDef(cls.declaredField(valName), Some(field.serializer)))
        (Some(valName), valSym, valDef)
      } else {
        // We are using a shared Term and do not need a ValDef
        val term: Option[String] = sharedSerializers.find{ _._1 =:= field.tpe }.head._2
        (term, (_: Symbol) => None, (_: Symbol) => None)
      }
    }
  }

  private case class FieldDeserializationInfo[T: Type](field: FieldImpl, sharedDeserializers: Seq[(TypeRepr, Option[String])]) { outer =>
    def number: Int = field.number
    def name: String = field.name
    def tpe: TypeRepr = field.tpe

    // Use either the shared TermName or create a new one if we are using a custom deserializer
    val (deserializerTermName: Option[String], deserializerValSym: (Symbol => Option[Symbol]), deserializerValDef: (Symbol => Option[ValDef])) = {
      if (field.deserializer != null) {
        val valName: String = Symbol.freshName(s"deser_${number}_${name}")
        // lazy val $valName: ${field.deserializer.tpe} = ${field.deserializer}
        def valSym(cls: Symbol): Option[Symbol] = Some(Symbol.newVal(cls, valName, field.deserializer.tpe, Flags.Lazy, Symbol.noSymbol))
        def valDef(cls: Symbol): Option[ValDef] = Some(ValDef(cls.declaredField(valName), Some(field.deserializer)))
        (Some(valName), valSym, valDef)
      } else {
        val term: Option[String] = sharedDeserializers.find{ _._1 =:= field.tpe }.head._2
        (term, (_: Symbol) => None, (_: Symbol) => None)
      }
    }

    /** Does this field have a user-defined default value (e.g. foo: Int = 123) */
    def hasUserDefinedDefaultValue: Boolean = field.defaultValue != null

    def forClassAndMethod(cls: Symbol, method: Symbol): ForClassAndMethod = new ForClassAndMethod(cls, method)

    class ForClassAndMethod(cls: Symbol, method: Symbol) {
      def field: FieldImpl = outer.field
      def deserializerTermName: Option[String] = outer.deserializerTermName
      def hasUserDefinedDefaultValue: Boolean = outer.hasUserDefinedDefaultValue
      def number: Int = outer.number
      def name: String = outer.name
      def tpe: TypeRepr = outer.tpe

      // var $isSetVarName: Boolean = false
      val isSetVarSym: Symbol = Symbol.newVal(method, Symbol.freshName(s"isset_${number}_${name}"), TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)
      val isSetVarDef: ValDef = ValDef(isSetVarSym, Some(Literal(BooleanConstant(false))))

      // var $readVarName: $tpe = ${defaultValueForType(tpe)}
      val readVarSym: Symbol = Symbol.newVal(method, Symbol.freshName(s"value_${number}_${name}"), tpe, Flags.Mutable, Symbol.noSymbol)
      val readVarDef: ValDef = ValDef(readVarSym, Some(defaultValueForType(tpe)))

      /** The default (e.g. foo: Int = 123) or un-initialized value (e.g. 0 for Int) to use for this field  */
      val defaultValue: Term = Option(field.defaultValue).getOrElse{
        deserializerTermName match {
          case None => Select.unique(Ident(cls.termRef), "defaultValue") // defaultValue
          case Some(deser: String) => Select.unique(Select.unique(Ident(cls.termRef), deser), "defaultValue") // ${deserializerTermName}.defaultValue
        }
      }

    }
  }

// Commented out in Scala 2:
////  private case class FieldDeserializerInfo(
////    deserializerTermName: TermRef,  // The variable (actually a val) name of the serializer that we can reference when writing
////    deserializerValDef: ValDef,     // The ValDef that defined the serializer for this field
////    isSetVarName: TermRef,          // This tracks whether or not we have read a value for this field
////    isSetVarDef: ValDef,            // This is the ValDef for the isSetVarName
////    readVarName: TermRef,           // The name of the variable we will read the value into
////    readVarDef: ValDef,             // The ValDef of the variable we read the value info
////    defaultValue: Tree              // The default value to use for this field if a value isn't read
////  )

  /**
   * The companion class to Field
   *
   * We take the arguments to the Field class (either as an Annotation or as "new Field(...)" expressions)
   * and generate a FieldImpl from them.  This means that for the Int and String fields you can only use
   * literals that can be used a compile time.  The serializer/deserialize can be arbitrary trees.
   */
  private case class FieldImpl(
    number: Int = -1,
    name: String = null,
    getter: String = null,
    setter: String = null,
    constructorIdx: Int = -1,
    serializer: Term = null.asInstanceOf[Term],
    deserializer: Term = null.asInstanceOf[Term],
    tpe: TypeRepr = null.asInstanceOf[TypeRepr],
    defaultValue: Term = null.asInstanceOf[Term],
  ) {
    def combine(other: FieldImpl): FieldImpl = {
      copy(
        number = combine(number, other.number),
        name = combine(name, other.name),
        getter = combine(getter, other.getter),
        setter = combine(setter, other.setter),
        constructorIdx = combine(constructorIdx, other.constructorIdx),
        serializer = combine(serializer, other.serializer),
        deserializer = combine(deserializer, other.deserializer),
        tpe = combineType(tpe, other.tpe),
        defaultValue = combine(defaultValue, other.defaultValue)
      )
    }
//    catch {
//      case ex: Exception => errorAndAbort(s"Could not Combine: ${ex.getMessage()} \nThis: $this\nOther: $other\nStack Trace:\n  ${ex.getStackTrace.mkString("\n  ")}")
//    }

    private def combine(a: Int, b: Int): Int = {
      if (-1 === a) b
      else if (-1 === b) a
      else {
        require(a === b, s"Values don't match: $a != $b")
        a
      }
    }

    private def combine(a: String, b: String): String = combine(Option(a), Option(b))
    private def combine(a: Term, b: Term): Term = combine(Option(a), Option(b))

    // Might not be totally accurate for Types (should use =:= for equality checking)
    // but I think we are okay with how we are using it
    private def combineType(a: TypeRepr, b: TypeRepr): TypeRepr = combine(Option(a), Option(b))

    private def combine[T](a: Option[T], b: Option[T]): T = {
      require(a === b || a.isDefined ^ b.isDefined, s"Values are different: $a != $b")
      (a orElse b).getOrElse{ null.asInstanceOf[T] }
    }
  }

  private def cleanFieldImpls(fields: Seq[FieldImpl]): Vector[FieldImpl] = {
    val tmp: Vector[FieldImpl] = fields.toVector.sortBy{ _.number }

    tmp.foreach{ (f: FieldImpl) => require(f.number > 0, s"Number must be > 0: $f") }
    tmp.foreach{ (f: FieldImpl) => require(null != f.name && f.name.length > 0, s"Name must be non-blank: $f") }

    def requireUnique[T](label: String, vals: Seq[T]): Unit = {
      require(vals.size === vals.toSet.size, s"$label is not unique!  $tmp")
    }

    requireUnique("number", tmp.map{ _.number })
    requireUnique("name", tmp.map{ _.name })
    requireUnique("constructorIdx", tmp.map{ _.constructorIdx }.filterNot{ _ === -1 })

    tmp
  }

  private def fillInType[T: Type](fields: Vector[FieldImpl]): Vector[FieldImpl] = {
    val objTpe: TypeRepr = TypeRepr.of[T]

    fields.map{ (f: FieldImpl) =>
      if (f.tpe != null) f else {
        require(f.getter != null, "Getter is null, not sure how to determine type")
        val method: Symbol = noArgsMethodOrField[T](f.getter)
        require(method.paramSymss.isEmpty || method.paramSymss === List(List()), s"Getter should have an empty param list.  Tpe: ${objTpe}  Getter: ${f.getter}  Paramss: ${method.paramSymss}")

        val returnType: TypeRepr = method.returnType
        require(!(returnType =:= TypeRepr.of[Unit]), s"Getter return type should not be Unit.  Tpe: ${objTpe}  Getter: ${f.getter}  Paramss: ${returnType}")
        f.copy(tpe = returnType)
      }
    }
  }

  /**
   * Extract RenameField annotations and return a Map of the Old Name => New Name
   */
  def extractRenameFieldAnnotations[T: Type]: Map[String,String] = {
    val tpe: TypeRepr = TypeRepr.of[T]

    log(s"extractRenameFieldAnnotations(${tpe.show})")

    val b = Vector.newBuilder[(String,String)]

    val cls: Symbol = Type.of[T].classSymbol
    val annotationTpe: TypeRepr = TypeRepr.of[RenameField]

    // annotations on values & methods in the class
    for {
      member: Symbol <- (cls.fieldMembers ++ cls.methodMembers).toVector
      ann: Term <- member.annotations
      if ann.tpe =:= annotationTpe
    } {
      log(s"""extractRenameFieldAnnotations(${tpe.show}) - Member: $member - "${member.name}" (${member.getClass}) - Annotations: ${member.annotations}""")
      b += ((member.name, makeRenameFieldImpl(ann)))
    }

    // cls.isAbstractType does not seem accurate
    val isAbstract: Boolean = cls.flags.is(Flags.Abstract)

    // annotations on constructor params (for non-abstract classes)
    if (!isAbstract) for {
      ctor: Symbol <- constructors[T].toVector
      param: Symbol <- ctor.paramSymss.flatten
      ann: Term <- param.annotations
      if ann.tpe =:= annotationTpe
    } {
      log(s"extractRenameFieldAnnotations(${tpe.show}) - Constructor Param: $param - Annotations: ${param.annotations}")
      b += ((param.name, makeRenameFieldImpl(ann)))
    }

    b.result().distinct.toUniqueHashMap
  }

  /**
   * Extract Field annotations for a type and convert them into FieldImpls
   */
  private def extractFieldAnnotations[T: Type]: Seq[FieldImpl] = {
    val tpe: TypeRepr = TypeRepr.of[T]

    log(s"extractFieldAnnotations(${tpe.show})")

    val cls: Symbol = Type.of[T].classSymbol
    val annotationTpe: TypeRepr = TypeRepr.of[Field]

    // annotations on values & methods in the class
    val fields: Vector[FieldImpl] = for {
      member: Symbol <- (cls.fieldMembers ++ cls.methodMembers).toVector
      ann: Term <- member.annotations
      if ann.tpe =:= annotationTpe
    } yield {
      log(s"""extractFieldAnnotations(${tpe.show}) - Member: $member - "${member.name}" (${member.getClass}) - Annotations: ${member.annotations}""")

      val returnType: TypeRepr = member.returnType

      val isGetter: Boolean = !(returnType =:= TypeRepr.of[Unit]) && member.paramSymss.isEmpty

      val isSetter: Boolean = returnType =:= TypeRepr.of[Unit] && (member.paramSymss match {
        case List(List(_)) => true
        case _ => false
      })

      require(isGetter || isSetter, s"Neither Getter nor Setter??  $member")

      // This will probably work but not sure if it's 100% correct (vs using encoded)
      val name: String = member.name

      val spec: FieldImpl = makeFieldImpl(ann, name)

      val additionalInfo: FieldImpl =
        if (isGetter) {
          FieldImpl(getter = name, tpe = member.returnType)
        } else if (isSetter) {
          val List(List(paramTpe)) = member.paramSymss
          FieldImpl(setter = name, tpe = paramTpe.returnType)
        } else throw new StacklessIllegalArgumentException(s"Both Getter AND Setter?  $member")

      // Populate additional missing fields from FieldImpl
      val missingFields: FieldImpl = {
        val n: String = if (spec.name === null || spec.name === "") name else null
        FieldImpl(name = n)
      }

      spec combine additionalInfo combine missingFields
    }

    // cls.isAbstractType does not seem reliable
    val isAbstract: Boolean = cls.flags.is(Flags.Abstract)

    // annotations on constructor params (for non-abstract classes)
    val constructorParams: Vector[FieldImpl] = if (isAbstract) Vector.empty else for {
      ctor: Symbol <- constructors[T].toVector
      args: List[Symbol] = ctor.paramSymss.flatten
      defaults: List[Option[Term]] = ctor.defaultValues
      ((param: Symbol, default: Option[Term]), idx: Int) <- (args zip defaults).zipWithIndex // TODO: Handle multiple parameter lists
      ann: Term <- param.annotations
      if ann.tpe =:= annotationTpe
    } yield {
      log(s"extractFieldAnnotations(${tpe.show}) - Constructor Param: $param - Annotations: ${param.annotations} - Default: $default")

      val name: String = param.name

      // Scala 2 code:
      // If this param is a Val then we can also use it as a getter
      //val getter: String = if (param.isTerm && param.asTerm.isVal) name else null

      // Not sure how to do this in Scala 3 but it seems like it should be handled by the fieldMember/fieldMethods code above
      val getter: String = null

      // If the @Field annotation doesn't have a number give it a default
      val defaultNumber: Int = idx + 1

      val spec: FieldImpl = makeFieldImpl(ann, name, defaultNumber)

      val additionalInfo: FieldImpl = FieldImpl(
        constructorIdx = idx,
        getter = getter,
        tpe = param.returnType,
        defaultValue = default.orNull.asInstanceOf[Term]
      )

      spec combine additionalInfo
    }

    val all: Vector[FieldImpl] = fields ++ constructorParams

    // Combine by number and then by name
    val collapsed: Vector[FieldImpl] = combineFieldImpl(all)

    // Sorted by number
    val sorted: Vector[FieldImpl] = collapsed.sortBy{ _.number }

    sorted
  }

  // Combine compatible FieldImpl records by number and name
  private def combineFieldImpl(all: Vector[FieldImpl]): Vector[FieldImpl] = {
    combineFieldImplUsing(combineFieldImplUsing(all, _.number, (num: Int) => num === -1), _.name, (name: String) => name === null)
  }

  private def combineFieldImplUsing[T](all: Vector[FieldImpl], groupBy: FieldImpl => T, ignore: T => Boolean): Vector[FieldImpl] = {
    val (toCombine, ignored): (Vector[FieldImpl], Vector[FieldImpl]) = all.partition{ (f: FieldImpl) => !ignore(groupBy(f)) }

    val combined: Vector[FieldImpl] = toCombine.groupBy{ groupBy }.values.map{ _.reduceLeft{ (a,b) => a combine b } }.toVector
    combined ++ ignored
  }

  /**
   * Given the arguments for a RenameField (either from an expression creating a new instance of a Field or from an annotation)
   * return the name field
   */
  private def makeRenameFieldImpl(ann: Term): String = {
    ann.asExpr match {
      // The single Constructor for RenameField
      case '{ new RenameField($name: String) } => name.valueOrAbort
    }
  }

  /**
   * Given a Tree for a Field, extract out the parameters and turn them into a FieldImpl
   */
  private def makeFieldImpl(field: Expr[Field]): FieldImpl = makeFieldImpl(field.asTerm)

  /**
   * Given the arguments for a Field (either from an expression creating a new instance of a Field or from an annotation)
   * create a FieldImpl
   */
  private def makeFieldImpl(ann: Term, defaultName: String = null, defaultNumber: Int = -1): FieldImpl = {
    ann.asExpr match {
      // Full Constructor:
      case '{ new Field($number: Int, $name: String, $getter: String, $setter: String, $constructorIdx: Int, $ser: Serializer[_], $deser: Deserializer[_]) } => FieldImpl(number.valueOrAbort, name.valueOrAbort, getter.valueOrAbort, setter.valueOrAbort, constructorIdx.valueOrAbort, ser.asTerm, deser.asTerm)

      // Without Serializer Arg:
      case '{ new Field($number: Int, $name: String, $getter: String, $constructorIdx: Int) } => FieldImpl(number.valueOrAbort, name.valueOrAbort, getter.valueOrAbort, null, constructorIdx.valueOrAbort)
      case '{ new Field($number: Int, $name: String, $getter: String, $setter: String) } => FieldImpl(number.valueOrAbort, name.valueOrAbort, getter.valueOrAbort, setter.valueOrAbort, -1)
      case '{ new Field($number: Int, $name: String) } => FieldImpl(number.valueOrAbort, name.valueOrAbort, null, null, -1)
      case '{ new Field($number: Int) } => FieldImpl(number.valueOrAbort, defaultName, null, null, -1)
      case '{ new Field($name: String) } => FieldImpl(defaultNumber, name.valueOrAbort, null, null, -1)

      // With Serializer Arg:
      case '{ new Field($number: Int, $name: String, $getter: String, $constructorIdx: Int, $serializer: SimpleSerializer[_]) } => FieldImpl(number.valueOrAbort, name.valueOrAbort, getter.valueOrAbort, null, constructorIdx.valueOrAbort, serializer.asTerm, serializer.asTerm)
      case '{ new Field($number: Int, $name: String, $getter: String, $setter: String, $serializer: SimpleSerializer[_]) } => FieldImpl(number.valueOrAbort, name.valueOrAbort, getter.valueOrAbort, setter.valueOrAbort, -1, serializer.asTerm, serializer.asTerm)
      case '{ new Field($number: Int, $name: String, $serializer: SimpleSerializer[_]) } => FieldImpl(number.valueOrAbort, name.valueOrAbort, null, null, -1, serializer.asTerm, serializer.asTerm)
      case '{ new Field($number: Int, $serializer: SimpleSerializer[_]) } => FieldImpl(number.valueOrAbort, defaultName, null, null, -1, serializer.asTerm, serializer.asTerm)
      case '{ new Field($serializer: SimpleSerializer[_]) } => FieldImpl(defaultNumber, defaultName, null, null, -1, serializer.asTerm, serializer.asTerm)
      case '{ new Field($name: String, $serializer: SimpleSerializer[_]) } => FieldImpl(defaultNumber, name.valueOrAbort, null, null, -1, serializer.asTerm, serializer.asTerm)

      // Empty
      case '{ new Field() } => FieldImpl(defaultNumber, defaultName)
    }
  }

  /**
   * Common types that I would like to define in CommonTypeImplicits but require the use
   * of a macro to create and I don't want to create a separate project to avoid the
   * separate compilation issue with macros.
   */
  def findCommonType[T: Type]: Option[Expr[SimpleSerializer[T]]] = {
    val tpe: TypeRepr = TypeRepr.of[T]

    log(s"findCommonType[${tpe.show}]")

    import java.math.{BigDecimal => JavaBigDecimal}

    val expr: Expr[SimpleSerializer[_]] =
      if (tpe =:= TypeRepr.of[JavaBigDecimal]) {
        makeSimpleObjectSerializer(Vector(
          FieldImpl(number = 1, name = "unscaledVal", getter = "unscaledValue", constructorIdx = 0),
          FieldImpl(number = 2, name = "scale", getter = "scale", constructorIdx = 1)
        ))
      } else null

    Option(expr).map{ _.asInstanceOf[Expr[SimpleSerializer[T]]] } orElse findEnumSimpleSerializer[T]
  }

  private def findEnumSimpleSerializer[T: Type]: Option[Expr[SimpleSerializer[T]]] = {
    val tpe: Type[T] = Type.of[T]

    log(s"findEnumSimpleSerializer[${TypeRepr.of[T].show}]")

    if (TypeRepr.of[T] <:< TypeRepr.of[java.lang.Enum[_]]) {
      // In a future version of Scala 3 we should be able to pattern match to get this same behavior
      type E <: java.lang.Enum[E]
      given Type[E] = tpe.asInstanceOf[Type[E]]
      Some(makeEnumSimpleSerializer[E].asExprOf[SimpleSerializer[T]])
    } else {
      None
    }
  }

  private def makeEnumSimpleSerializer[T <: java.lang.Enum[_]: Type]: Expr[SimpleSerializer[T]] = {
    log(s"makeEnumSimpleSerializer[${TypeRepr.of[T].show}]")

    val companion: Symbol = TypeRepr.of[T].typeSymbol.companionModule

    // This is the expression that gives us the static values() method on the java.lang.Enum object
    val valuesExpr: Expr[Array[T]] = Apply(Select.unique(Ident(companion.termRef), "values"), List()).asExpr.asExprOf[Array[T]]

    '{ fm.serializer.Primitive.int.map[T]((value: T) => value.ordinal(), (idx: Int) => $valuesExpr(idx), null.asInstanceOf[T]) }
  }

  /**
   * Lookup an Option Serializer
   */
  def findOptionSerializer[T: Type]: Option[Expr[Serializer[T]]] = {
    val tpe: Type[T] = Type.of[T]

    log(s"findOptionSerializer(${TypeRepr.of[T].show})")

    val expr: Expr[Serializer[_]] = tpe match {
      case '[Option[inner]] => '{ fm.serializer.OptionSerializer[inner]()(${implicitOr[Serializer[inner]]{ Macros.makeSerializer[inner] }}) }
//      case '[Option[inner]] => '{ fm.serializer.OptionSerializer[inner]()(scala.compiletime.summonInline[Serializer[inner]]) }
      case _ => null
    }

    Option(expr).map { (makeSerializer: Expr[Serializer[_]]) =>
      val res = makeProxyForSerializer(makeSerializer.asExprOf[Serializer[T]])
      log(s"SUCCESSFULLY CREATED OPTION SERIALIZER FOR ${TypeRepr.of[T].show}: ${res.asTerm.show}")
      res
    }
  }

  def findOptionDeserializer[T: Type]: Option[Expr[Deserializer[T]]] = {
    val tpe: Type[T] = Type.of[T]

    log(s"findOptionDeserializer(${TypeRepr.of[T].show})")

    val expr: Expr[Deserializer[_]] = tpe match {
      case '[Option[Boolean]] => '{ fm.serializer.BooleanOptionDeserializer }
      case '[Option[Char]]    => '{ fm.serializer.CharOptionDeserializer }
      case '[Option[Int]]     => '{ fm.serializer.IntOptionDeserializer }
      case '[Option[Long]]    => '{ fm.serializer.LongOptionDeserializer }
      case '[Option[inner]]   => '{ fm.serializer.OptionDeserializer[inner]()(${implicitOr[Deserializer[inner]]{ Macros.makeDeserializer[inner] }}) }
//      case '[Option[inner]]   => '{ fm.serializer.OptionDeserializer[inner]()(scala.compiletime.summonInline[Deserializer[inner]]) }
      case _ => null
    }

    Option(expr).map { (makeDeserializer: Expr[Deserializer[_]]) =>
      val res = makeProxyForDeserializer(makeDeserializer.asExprOf[Deserializer[T]])
      log(s"SUCCESSFULLY CREATED OPTION DESERIALIZER FOR ${TypeRepr.of[T].show}: ${res.asTerm.show}")
      res
    }
  }

  /**
   * Lookup an AnyVal Serializer
   */
  def findAnyValSerializer[T: Type]: Option[Expr[Serializer[T]]] = {
    val tpe: Type[T] = Type.of[T]

    log(s"findAnyValSerializer(${TypeRepr.of[T].show})")

    if (!(TypeRepr.of[T] <:< TypeRepr.of[AnyVal])) return None

    val name: String = Symbol.freshName("anyValSerializer")

    // Since this is an AnyVal there should be a constructor that
    // only has a single val parameter. makeFieldImplsForCaseClass
    // should return that single param
    val fields: Vector[FieldImpl] = fillInType[T](cleanFieldImpls(makeFieldImplsForCaseClass[T]))

    if (fields.length =!= 1) return None

    val serInfo: ObjectSerializationInfo[T] = ObjectSerializationInfo[T](fields)
    val field: FieldSerializationInfo[T] = serInfo.infos.head

    log(s"field: $field")

    // Here is what we are trying to generate:
    //
    //  q"""
    //    implicit object $name extends fm.serializer.Serializer[$tpe] {
    //      ..${serInfo.serializerDeclarations}
    //
    //      def serializeRaw(output: fm.serializer.RawOutput, obj: $tpe): Unit = ${field.serializerTermName}.serializeRaw(output, ${field.fieldAccessor})
    //      def serializeNested(output: fm.serializer.NestedOutput, obj: $tpe): Unit = ${field.serializerTermName}.serializeNested(output, ${field.fieldAccessor})
    //      def serializeField(output: fm.serializer.FieldOutput, number: Int, name: String, obj: $tpe): Unit = ${field.serializerTermName}.serializeField(output, number, name, ${field.fieldAccessor})
    //    }
    //
    //    $name
    //   """

    def decls(cls: Symbol): List[Symbol] = serInfo.serializerDeclarations(cls) ++ List(
      Symbol.newMethod(cls, "serializeRaw", MethodType(List("output", "obj"))(_ => List(TypeRepr.of[RawOutput], TypeRepr.of[T]), _ => TypeRepr.of[Unit]), Flags.Override, Symbol.noSymbol),
      Symbol.newMethod(cls, "serializeNested", MethodType(List("output", "obj"))(_ => List(TypeRepr.of[NestedOutput], TypeRepr.of[T]), _ => TypeRepr.of[Unit]), Flags.Override, Symbol.noSymbol),
      Symbol.newMethod(cls, "serializeField", MethodType(List("output", "number", "name", "obj"))(_ => List(TypeRepr.of[FieldOutput], TypeRepr.of[Int], TypeRepr.of[String], TypeRepr.of[T]), _ => TypeRepr.of[Unit]), Flags.Override, Symbol.noSymbol),
    )

    val modSym: Symbol = Symbol.newModule(Symbol.spliceOwner, name, Flags.Implicit, Flags.EmptyFlags, List(TypeRepr.of[Object], TypeRepr.of[Serializer[T]]), decls, Symbol.noSymbol)

    type F
    given Type[F] = field.tpe.asType.asInstanceOf[Type[F]]

    val serializer: Expr[Serializer[F]] = Select.unique(Ident(modSym.termRef), field.serializerTermName.getOrElse{ errorAndAbort("serializerTermName is not defined. Probably a code bug. Need to handle the case when it is None.") }).asExprOf[Serializer[F]]
    def fieldAccessor(instance: Term): Expr[F] = field.fieldAccessor(instance).asExprOf[F]

    val body: List[Statement] = serInfo.serializerDefinitions(modSym) ++ List(
      DefDefQuotes(modSym.declaredMethod("serializeRaw").head, { case List(List(output, obj)) => Some('{ $serializer.serializeRaw(${output.asExprOf[RawOutput]}, ${fieldAccessor(obj.asExprOf[T].asTerm)}) }.asTerm) }),
      DefDefQuotes(modSym.declaredMethod("serializeNested").head, { case List(List(output, obj)) => Some('{ $serializer.serializeNested(${output.asExprOf[NestedOutput]}, ${fieldAccessor(obj.asExprOf[T].asTerm)}) }.asTerm) }),
      DefDefQuotes(modSym.declaredMethod("serializeField").head, { case List(List(output, number, name, obj)) => Some('{  $serializer.serializeField(${output.asExprOf[FieldOutput]}, ${number.asExprOf[Int]}, ${name.asExprOf[String]}, ${fieldAccessor(obj.asExprOf[T].asTerm)}) }.asTerm) }),
    )

    log(s"body: ${body.map{ _.show }.mkString("\n  ","\n  ", "")}")

    val (modValDef: ValDef, modClassDef: ClassDef) = ClassDef.module(modSym, List(TypeTree.of[Object], TypeTree.of[Serializer[T]]), body)

    log(s"modValDef: ${modValDef.show}")
    log(s"modClassDef: ${modClassDef.show}")

    val expr: Expr[Serializer[T]] = Block(List(modValDef, modClassDef), Ref(modSym)).asExprOf[Serializer[T]]
    Option(expr)
  }

  def findAnyValDeserializer[T: Type]: Option[Expr[Deserializer[T]]] = {
    val tpe: TypeRepr = TypeRepr.of[T]

    log(s"findAnyValDeserializer(${tpe.show})")
    if (!(tpe <:< TypeRepr.of[AnyVal])) return None

    val name: String = Symbol.freshName("anyValDeserializer")

    // Since this is an AnyVal there should be a constructor that
    // only has a single val parameter. makeFieldImplsForCaseClass
    // should return that single param
    val fields: Vector[FieldImpl] = fillInType[T](cleanFieldImpls(makeFieldImplsForCaseClass[T]))

    if (fields.length =!= 1) return None

    val deserInfo: ObjectDeserializationInfo[T] = ObjectDeserializationInfo[T](fields)
    val field: FieldDeserializationInfo[T] = deserInfo.infos.head

    log(s"field: $field")

    // Here is what we are trying to generate:
    //
    //  q"""
    //    implicit object $name extends fm.serializer.Deserializer[$tpe] {
    //      ..${deserInfo.deserializerDeclarations}
    //      def defaultValue: $tpe = make(${field.deserializerTermName}.defaultValue)
    //      def deserializeRaw(input: fm.serializer.RawInput): $tpe = make(${field.deserializerTermName}.deserializeRaw(input))
    //      def deserializeNested(input: fm.serializer.NestedInput): $tpe = make(${field.deserializerTermName}.deserializeNested(input))
    //      private def make(value: ${field.tpe}): $tpe = new ${tpe}(value)
    //    }
    //
    //    $name
    //   """

    type F
    given Type[F] = field.tpe.asType.asInstanceOf[Type[F]]

    def decls(cls: Symbol): List[Symbol] = deserInfo.deserializerDeclarations(cls) ++ List(
      Symbol.newMethod(cls, "defaultValue", ByNameType(TypeRepr.of[T]), Flags.Override, Symbol.noSymbol),
      Symbol.newMethod(cls, "deserializeRaw", MethodType(List("input"))(_ => List(TypeRepr.of[RawInput]), _ => TypeRepr.of[T]), Flags.Override, Symbol.noSymbol),
      Symbol.newMethod(cls, "deserializeNested", MethodType(List("input"))(_ => List(TypeRepr.of[NestedInput]), _ => TypeRepr.of[T]), Flags.Override, Symbol.noSymbol),
      Symbol.newMethod(cls, "make", MethodType(List("value"))(_ => List(TypeRepr.of[F]), _ => TypeRepr.of[T]), Flags.Private, Symbol.noSymbol),
    )

    val modSym: Symbol = Symbol.newModule(Symbol.spliceOwner, name, Flags.Implicit, Flags.EmptyFlags, List(TypeRepr.of[Object], TypeRepr.of[Deserializer[T]]), decls, Symbol.noSymbol)

    val deserializer: Expr[Deserializer[F]] = Select.unique(Ident(modSym.termRef), field.deserializerTermName.getOrElse{ errorAndAbort("deserializerTermName is not defined. Probably a code bug. Need to handle the case when it is None.") }).asExprOf[Deserializer[F]]
    def make(value: Expr[F]): Term = Apply(Ref(modSym.declaredMethod("make").head), List(value.asTerm))

    val body: List[Statement] = deserInfo.deserializerDefinitions(modSym) ++ List(
      DefDefQuotes(modSym.declaredMethod("defaultValue").head, { case _ => Some(make('{ $deserializer.defaultValue })) }),
      DefDefQuotes(modSym.declaredMethod("deserializeRaw").head, { case List(List(input)) => Some(make('{ $deserializer.deserializeRaw(${input.asExprOf[RawInput]}) })) }),
      DefDefQuotes(modSym.declaredMethod("deserializeNested").head, { case List(List(input)) => Some(make('{ $deserializer.deserializeNested(${input.asExprOf[NestedInput]}) })) }),
      DefDefQuotes(modSym.declaredMethod("make").head, { case List(List(value)) => Some(Apply(Select(New(TypeTree.of[T]), Type.of[T].classSymbol.primaryConstructor), List(value.asExprOf[F].asTerm))) }),
    )

    val (modValDef: ValDef, modClassDef: ClassDef) = ClassDef.module(modSym, List(TypeTree.of[Object], TypeTree.of[Deserializer[T]]), body)
    val expr: Expr[Deserializer[T]] = Block(List(modValDef, modClassDef), Ref(modSym)).asExprOf[Deserializer[T]]

    Option(expr)
  }

  /**
   * Lookup a Collection Serializer
   */
  def findCollectionSerializer[Col: Type]: Option[Expr[Serializer[Col]]] = {
    val tpe: TypeRepr = TypeRepr.of[Col]

    log(s"findCollectionSerializer(${tpe.show})")

    val makeSerializerOpt: Option[Expr[Serializer[_]]] = Type.of[Col] match {
      case '[IterableOnce[(String, value)]] =>
        type NewCol <: IterableOnce[(String, value)]
        given Type[NewCol] = Type.of[Col].as[NewCol]
        log(s"DETECTED IterableOnce[${TypeRepr.of[(String, value)].show}] => new StringMapSerializer[${TypeRepr.of[value].show}, ${TypeRepr.of[NewCol].show}]")
        val pairSerializer: Expr[Serializer[(String, value)]] = implicitOr[Serializer[(String, value)]]{ Macros.makeSerializer[(String, value)] }
        val valueSerializer: Expr[Serializer[value]] = implicitOr[Serializer[value]]{ Macros.makeSerializer[value] }
        Some('{ new StringMapSerializer[value, NewCol]()($pairSerializer, $valueSerializer) })

      case '[IterableOnce[elem]] =>
        type NewCol <: IterableOnce[elem]
        given Type[NewCol] = Type.of[Col].as[NewCol]
        log(s"DETECTED IterableOnce[${TypeRepr.of[elem].show}] => new TraversableOnceSerializer[${TypeRepr.of[elem].show}, ${TypeRepr.of[NewCol].show}]()")
        val elemSerializer: Expr[Serializer[elem]] = implicitOr[Serializer[elem]]{ Macros.makeSerializer[elem] }
        Some('{ new TraversableOnceSerializer[elem, NewCol]()($elemSerializer) })

      case '[JavaIterable[elem]] =>
        type NewCol <: JavaIterable[elem]
        given Type[NewCol] = Type.of[Col].as[NewCol]
        log(s"DETECTED JavaIterable[${TypeRepr.of[elem].show}] => new JavaIterableSerializer[${TypeRepr.of[elem].show}, ${TypeRepr.of[NewCol].show}]()")
        val elemSerializer: Expr[Serializer[elem]] = implicitOr[Serializer[elem]]{ Macros.makeSerializer[elem] }
        Some('{ new JavaIterableSerializer[elem, NewCol]()($elemSerializer) })

      case _ => None
    }

    log(s"findCollectionSerializer(${tpe.show}) => ${makeSerializerOpt.map{ _.asTerm.show }}")

    makeSerializerOpt.map{ (makeSerializer: Expr[Serializer[_]]) =>
      makeProxyForSerializer(makeSerializer.asExprOf[Serializer[Col]])
    }
  }

  private def makeProxyForSerializer[T: Type](expr: Expr[Serializer[T]])(using Quotes): Expr[SerializerProxy[T]] = {
    // We want to generate something like this
    // '{
    //   implicit val colSerializerProxy: fm.serializer.SerializerProxy[T] = new fm.serializer.SerializerProxy[T]
    //   val colSerializer: fm.serializer.Serializer[T] = ${makeSerializer.asExprOf[Serializer[T]]}
    //   colSerializerProxy.self = colSerializer
    //   colSerializer
    // }

    // We have to use a SerializerProxy here because StringMapSerializer/TraversableOnceSerializer/JavaIterableSerializer
    // all take implicit Serializer parameters that won't work if there is a nested type that is trying to reference
    // the implicit val we are trying to create.
    val name: String = Symbol.freshName("underlyingSerializer")
    val proxyName: String = Symbol.freshName("serializerProxy")

    // implicit val $proxyName: fm.serializer.SerializerProxy[T] = new fm.serializer.SerializerProxy[T]()
    val proxyNameSym: Symbol = Symbol.newVal(Symbol.spliceOwner, proxyName, TypeRepr.of[SerializerProxy[T]], Flags.Implicit, Symbol.noSymbol)
    val proxyNameValDef: ValDef = ValDefQuotes(proxyNameSym, Some('{ new SerializerProxy[T]() }.asTerm))

    // val name: fm.serializer.Serializer[T] = ${expr.asExprOf[Serializer[T]]}
    val nameSym: Symbol = Symbol.newVal(Symbol.spliceOwner, name, TypeRepr.of[Serializer[T]], Flags.EmptyFlags, Symbol.noSymbol)
    val nameValDef: ValDef = ValDef(nameSym, Some(expr.asExprOf[Serializer[T]].asTerm.changeOwner(nameSym)))

    // proxyName.self = name
    val assignSelf: Assign = Assign(Select.unique(Ref(proxyNameSym), "self"), Ref(nameSym))

    Block(List(
      proxyNameValDef,
      nameValDef,
      assignSelf,
    ), Ref(proxyNameSym)).asExprOf[SerializerProxy[T]]
  }

  /**
   * Lookup a Collection Serializer
   */
  def findCollectionDeserializer[Col: Type]: Option[Expr[Deserializer[Col]]] = {
    val tpe: TypeRepr = TypeRepr.of[Col]

    log(s"findCollectionDeserializer(${tpe.show})")

    val elemTpeOpt: Option[TypeRepr] = Type.of[Col] match {
      case '[IterableOnce[elem]] => Some(TypeRepr.of[elem])
      case '[Growable[elem]]     => Some(TypeRepr.of[elem])
      case '[JavaIterable[elem]] => Some(TypeRepr.of[elem])
      case _                     => tpe.typeArgs.headOption
    }

    if (elemTpeOpt.isEmpty) {
      log(s"elemTpe is empty for ${tpe.show}")
      return None
    }

    val elemTpe: TypeRepr = elemTpeOpt.get
    type Elem
    given Type[Elem] = elemTpe.asType.asInstanceOf[Type[Elem]]

    //val expr: Expr[Deserializer[Col]] = if (elemTpe <:< TypeRepr.of[(String,_)]) makeStringMapCollectionDeserializer[Col, Elem] else makeNormalCollectionDeserializer[Col, Elem]

    val expr: Expr[Deserializer[Col]] = Type.of[Elem] match {
      case '[(String, value)] => makeStringMapCollectionDeserializer[Col, value]
      case _                  => makeNormalCollectionDeserializer[Col, Elem]
    }

    Option(expr)
  }

  //
  // Helpers for constructing definitions with quotes
  //
  private def DefDefQuotes(symbol: Symbol, rhsFn: Quotes ?=> List[List[Tree]] => Option[Term]): DefDef = {
    given Quotes = symbol.asQuotes
    DefDef(symbol, rhsFn)
  }

  private def ValDefQuotes(symbol: Symbol, rhs: Quotes ?=> Option[Term]): ValDef = {
    given Quotes = symbol.asQuotes
    ValDef(symbol, rhs)
  }

//  private def BlockQuotes(Quotes ?=> List[Statement], expr: Quotes ?=> Term)

  extension (self: TypeRepr) {
    /** Convert from a TypeRepr to a TypeTree */
    private def tpt: TypeTree = TypeTree.of(using self.asType)

    /** Apply any concrete types params from T onto type params of TypeRepr */
    private def withConcreteTypes[T: Type]: TypeRepr = {
      //val from: List[Symbol] = TypeRepr.of[T].typeSymbol.typeMembers.filterNot{ _.isNoSymbol }
      val from: List[Symbol] = TypeRepr.of[T].typeSymbol.primaryConstructor.paramSymss.flatten.filter{ _.isTypeDef }
      val to: List[TypeRepr] = TypeRepr.of[T].typeArgs

      self.substituteTypes(from, to)
    }
  }

  extension [A](self: Type[A]) {
    def as[B]: Type[B] = self.asInstanceOf[Type[B]]
    private def classSymbol: Symbol = TypeRepr.of[A](using self).classSymbol.getOrElse{ errorAndAbort(s"${TypeRepr.of[A](using self)} is not a class symbol") }
  }

  extension (self: Term) {
    private def asInstanceOfType[T: Type]: Term = asInstanceOfTypeTree(TypeTree.of[T])
    private def asInstanceOfTypeRepr(tpe: TypeRepr): Term = asInstanceOfTypeTree(tpe.tpt)
    private def asInstanceOfTypeTree(tpt: TypeTree): Term = TypeApply(Select.unique(self, "asInstanceOf"), List(tpt))
  }

  extension (self: Symbol) {
    private def usingQuotes[A](f: Quotes ?=> A): A = f(using self.asQuotes)

    private def toOption: Option[Symbol] = if (self.isNoSymbol) None else Some(self)

    private def isPrivate: Boolean = self.flags.is(Flags.Private)
    private def isProtected: Boolean = self.flags.is(Flags.Protected)
    private def isPublic: Boolean = !isPrivate && !isProtected

    @targetName("symbolReturnType") private def returnType: TypeRepr = self.tree.returnType

//    /** This is meant to be called on symbols of classes or methods */
//    private def defaultValues: List[Option[Term]] = {
//      self match {
//        case _ if self.isClassDef => self.primaryConstructor.paramSymss.flatten.map{ _.defaultValue }
//        case _ if self.isDefDef => self.paramSymss.flatten.map{ _.defaultValue }
//        case _ => Nil
//      }
//    }

    /** This is meant to be called on symbols of classes or methods */
    private def defaultValues: List[Option[Term]] = {
      // We have 3 different cases we need to handle
      val (method: Symbol, cls: Symbol, namePrefix: String, makeTerm: (Symbol => Term)) = self match {
        // Our Symbol is a class def. The default value will be in the companion class.
        case _ if self.isClassDef =>
          // We are looking for the default of a class constructor (e.g. "$lessinit$greater$default${idx_of_param}").
          // The "$lessinit$greater$default${idx}" method will be in the companion class definition
          val cls: Symbol = self
          def makeTerm(symbol: Symbol): Term = Ref(cls.companionModule).select(symbol)
          (cls.primaryConstructor, cls.companionClass, "$lessinit$greater", makeTerm)

        // Our Symbol is a constructor. The default value will be in the companion class.
        case _ if self.isDefDef && self.name === "<init>" =>
          val method: Symbol = self
          val cls: Symbol = method.owner
          def makeTerm(symbol: Symbol): Term = Ref(cls.companionModule).select(symbol)
          (method, cls.companionClass, "$lessinit$greater", makeTerm)

        // Our symbol is a method. The default value will be in the class
        case _ if self.isDefDef =>
          // We are looking for the default of a method parameter which will be a method in the class.
          // The name of the method will be "{name_of_method}$default${idx_of_param}" with an index starting at 1
          val method: Symbol = self
          val cls: Symbol = method.owner
          def makeTerm(symbol: Symbol): Term = Select(This(cls), symbol)
          (method, cls, method.name, makeTerm)

        case _ =>
          //errorAndAbort(s"Cannot find default value for unexpected symbol ${self}")
          return Nil
      }

      def makeDefaultMethodName(idx: Int): String = s"${namePrefix}$$default$$${idx+1}"

      val paramCount: Int = method.paramSymss.flatten.size

      (0 until paramCount).map{ (idx: Int) =>
        cls.declaredMethod(makeDefaultMethodName(idx)).headOption.map{ makeTerm }
      }.toList

//      // "{namePrefix}$default${idx+1}"
//      val defaultMethodNamePrefix: String = s"${namePrefix}$$default$$"
//
//      cls.tree.asInstanceOf[ClassDef].body.collect{
//        case defdef @ DefDef(name, _, _, _) if name startsWith defaultMethodNamePrefix => Some(makeTerm(defdef))
//      }
    }

    /** This is meant to be called on symbols of fields, constructor parameters or method parameters */
    private def defaultValue: Option[Term] = {
      if (!self.isValDef) return None //errorAndAbort(s"defaultValue is only applicable to ValDef parameters. Got: $self")
      if (!self.flags.is(Flags.HasDefault)) return None

      // This will either be a ClassDef or a DefDef
      val owner: Symbol = self.owner

      // We have 3 different cases we need to handle
      val (cls: Symbol, namePrefix: String, idx: Int, makeTerm: (DefDef => Term)) = owner match {
        // Our Symbol is a field definition. The default value will be in the companion class.
        case _ if owner.isClassDef =>
          // We are looking for the default of a class constructor (e.g. "$lessinit$greater$default${idx_of_param}").
          // The "$lessinit$greater$default${idx}" method will be in the companion class definition
          val cls: Symbol = owner
          val idx: Int = cls.primaryConstructor.paramSymss.flatten.indexWhere{ _.name === self.name }
          if (-1 === idx) errorAndAbort(s"Unable to locate param idx for ${self.name}")
          def makeTerm(defdef: DefDef): Term = Ref(cls.companionModule).select(defdef.symbol)
          (cls.companionClass, "$lessinit$greater", idx, makeTerm)

        // Our Symbol is a constructor parameter. The default value will be in the companion class.
        case _ if owner.isDefDef && owner.name === "<init>" =>
          val method: Symbol = owner
          val cls: Symbol = owner.owner
          val idx: Int = method.paramSymss.flatten.indexWhere{ _.name === self.name }
          if (-1 === idx) errorAndAbort(s"Unable to locate param idx for ${self.name}")
          def makeTerm(defdef: DefDef): Term = Ref(cls.companionModule).select(defdef.symbol)
          (cls.companionClass, "$lessinit$greater", idx, makeTerm)

        // Our symbol is a method parameter. The default value will be in the class
        case _ if owner.isDefDef =>
          // We are looking for the default of a method parameter which will be a method in the class.
          // The name of the method will be "{name_of_method}$default${idx_of_param}" with an index starting at 1
          val method: Symbol = owner
          val cls: Symbol = owner.owner
          val idx: Int = method.paramSymss.flatten.indexWhere{ _.name === self.name }
          if (-1 === idx) errorAndAbort(s"Unable to locate param idx for ${self.name}")
          def makeTerm(defdef: DefDef): Term = Select(This(cls), defdef.symbol)
          (cls, method.name, idx, makeTerm)

        case _ =>
          errorAndAbort(s"Cannot find default value for unexpected symbol ${self}")
      }

      // "{namePrefix}$default${idx+1}"
      val defaultMethodName: String = s"${namePrefix}$$default$$${idx+1}"

      val defaultMethod: DefDef = cls.tree.asInstanceOf[ClassDef].body.collectFirst{
        case defdef @ DefDef(name, _, _, _) if name == defaultMethodName => defdef
      }.getOrElse{ errorAndAbort(s"Could not find DefDef with name $defaultMethodName. This is an error in the code!") }

      Some(makeTerm(defaultMethod))
    }
  }

  extension (self: Tree) {
    @targetName("treeReturnType") private def returnType: TypeRepr = {
      self match {
        case ValDef(_, tpt: TypeTree, _)    => tpt.tpe
        case DefDef(_, _, tpt: TypeTree, _) => tpt.tpe
        case TypeDef(_, tree: Tree)         => tree.returnType
        case tpt: TypeTree                  => tpt.tpe
        case tree => errorAndAbort(s"Not sure how to calculate return type for $tree")
      }
    }
  }

  private def hasAnnotation[Annotation: Type](sym: Symbol): Boolean = {
    sym.annotations.exists{ (ann: Term) => ann.tpe =:= TypeRepr.of[Annotation] }
  }

  private def makeNormalCollectionDeserializer[Col: Type, Elem: Type]: Expr[Deserializer[Col]] = {
    val colTpe: TypeRepr = TypeRepr.of[Col]
    val elemTpe: TypeRepr = TypeRepr.of[Elem]

    log(s"makeNormalCollectionDeserializer[${colTpe.show}, ${elemTpe.show}]")

    val VectorTpe: TypeRepr = TypeRepr.of[Vector[Elem]]
    val IndexedSeqTpe: TypeRepr = TypeRepr.of[scala.collection.IndexedSeq[Elem]]
    val ImmutableIndexedSeqTpe: TypeRepr = TypeRepr.of[scala.collection.immutable.IndexedSeq[Elem]]
    val SeqTpe: TypeRepr = TypeRepr.of[Seq[Elem]]
    val IterableTpe: TypeRepr = TypeRepr.of[Iterable[Elem]]

    def factory: Expr[Factory[Elem, Col]]  = implicitOrAbort[Factory[Elem, Col]]
    def classTag: Expr[ClassTag[Elem]] = implicitOrAbort[ClassTag[Elem]]
    def elemDeser: Expr[Deserializer[Elem]] = implicitOr[Deserializer[Elem]]{ Macros.makeDeserializer[Elem] }

    val expr: Expr[Deserializer[_]] =
      if (colTpe <:< VectorTpe) {
        // Note: tpe =:= VectorTpe doesn't work
        log(s"DETECTED VECTOR - ${colTpe.show} - new fm.serializer.VectorDeserializer[${elemTpe.show}, ${colTpe.show}]()")
        type VectorCol >: Vector[Elem]
        given Type[VectorCol] = Type.of[Col].as[VectorCol]
        '{ new fm.serializer.VectorDeserializer[Elem, VectorCol]()($elemDeser) }
      } else if (colTpe <:< TypeRepr.of[fm.common.ImmutableArray[Elem]]) {
        log(s"DETECTED IMMUTABLE_ARRAY - ${colTpe.show} - new fm.serializer.VectorDeserializer[${elemTpe.show}, ${colTpe.show}]()")
        type ImmutableArrayCol >: fm.common.ImmutableArray[Elem]
        given Type[ImmutableArrayCol] = Type.of[Col].as[ImmutableArrayCol]
        '{ new fm.serializer.ImmutableArrayDeserializer[Elem, ImmutableArrayCol]()($classTag, $elemDeser) }
      } else if ((colTpe <:< IndexedSeqTpe && IndexedSeqTpe <:< colTpe) || (colTpe <:< ImmutableIndexedSeqTpe && ImmutableIndexedSeqTpe <:< colTpe)) {
        // Note: tpe =:= IndexedSeqTpe doesn't work and we want to make sure we only match scala.collection.IndexedSeq
        //       and NOT sublcasses (since scala.collection.mutable.IndexedSeq is also a subtype but won't work with Vector)
        log(s"DETECTED INDEXED_SEQ - ${colTpe.show} - new fm.serializer.VectorDeserializer[${elemTpe.show}, ${colTpe.show}]()")
        // Default to the VectorDeserializer for any other IndexedSeq type
        type VectorCol >: Vector[Elem]
        given Type[VectorCol] = Type.of[Col].as[VectorCol]
        '{ new fm.serializer.VectorDeserializer[Elem, VectorCol]()($elemDeser) }
      } else if (colTpe <:< SeqTpe && SeqTpe <:< colTpe) {
        // Default to the VectorDeserializer for any other IndexedSeq type
        log(s"DETECTED SEQ - ${colTpe.show} - new fm.serializer.VectorDeserializer[${elemTpe.show}, ${colTpe.show}]()")
        type VectorCol >: Vector[Elem]
        given Type[VectorCol] = Type.of[Col].as[VectorCol]
        '{ new fm.serializer.VectorDeserializer[Elem, VectorCol]()($elemDeser) }
      } else if (colTpe <:< IterableTpe && IterableTpe <:< colTpe) {
        // Default to the VectorDeserializer for any other IndexedSeq type
        log(s"DETECTED ITERABLE - ${colTpe.show} - new fm.serializer.VectorDeserializer[${elemTpe.show}, ${colTpe.show}]()")
        type VectorCol >: Vector[Elem]
        given Type[VectorCol] = Type.of[Col].as[VectorCol]
        '{ new fm.serializer.VectorDeserializer[Elem, VectorCol]()($elemDeser) }
      } else if (hasImplicit[Factory[Elem, Col]]) {
        log(s"DETECTED CanBuildFrom - ${colTpe.show} - new fm.serializer.CanBuildFromDeserializer[${elemTpe.show}, ${colTpe.show}]()")
        '{ new fm.serializer.CanBuildFromDeserializer[Elem, Col]()($factory, $elemDeser) }
      } else if (colTpe <:< TypeRepr.of[Growable[Elem]] && hasNoArgsConstructor[Col]) {
        log(s"DETECTED Growable with no-args constructor - ${colTpe.show}")
        type GrowableCol <: Growable[Elem]
        given Type[GrowableCol] = Type.of[Col].as[GrowableCol]
        val newCls: Expr[GrowableCol] = newInstanceOfAs[Col, GrowableCol]
        '{ new fm.serializer.GrowableDeserializer[Elem, GrowableCol]($newCls)($elemDeser) }
      } else if (colTpe <:< TypeRepr.of[JavaCollection[Elem]]) {
        // colTpe.typeSymbol.isAbstractType does not seem to work
        val isAbstract: Boolean = colTpe.typeSymbol.flags.is(Flags.Abstract)
        log(s"DETECTED Java Collection - ${colTpe.show} - isAbstract: $isAbstract")
        type JavaCol <: JavaCollection[Elem]
        given Type[JavaCol] = Type.of[Col].as[JavaCol]
        // TODO: make this more robust (copied over from Scala 2 impl)
        val newTpe: Expr[_] = if (isAbstract) '{ new java.util.ArrayList[Elem]() } else newInstanceOf[Col]
        '{ new fm.serializer.JavaCollectionDeserializer[Elem, JavaCol](${newTpe.asExprOf[JavaCol]})($elemDeser) }
      } else {
        null
      }

    if (expr === null) return null

    val name: String = Symbol.freshName("colDeserializer")
    val proxyName: String = Symbol.freshName("colDeserializerProxy")

    makeProxyForDeserializer(expr.asExprOf[Deserializer[Col]])
  }

  private def makeStringMapCollectionDeserializer[Col: Type, Value: Type]: Expr[Deserializer[Col]] = {
    type Elem = (String, Value)

    val colTpe: TypeRepr = TypeRepr.of[Col]
    val valueTpe: TypeRepr = TypeRepr.of[Value]
    val elemTpe: TypeRepr = TypeRepr.of[Elem]

    log(s"makeStringMapCollectionDeserializer[${colTpe.show}, ${valueTpe.show}]")

    require(elemTpe <:< TypeRepr.of[(String,_)], s"Expected elemTpe $elemTpe to be <:< (String,_)")

    def factory: Expr[Factory[Elem, Col]] = implicitOrAbort[Factory[Elem, Col]]
    def elemDeser: Expr[Deserializer[Elem]] = implicitOr[Deserializer[Elem]]{ Macros.makeDeserializer[Elem] }
    def valueDeser: Expr[Deserializer[Value]] = implicitOr[Deserializer[Value]]{ Macros.makeDeserializer[Value] }

    val expr: Expr[_] = if (hasImplicit[Factory[Elem, Col]]) {
      '{ new fm.serializer.StringMapCanBuildFromDeserializer[Value, Col]()($factory, $elemDeser, $valueDeser) }
    } else if (colTpe <:< TypeRepr.of[Growable[Elem]] && hasNoArgsConstructor[Col]) {
      // Casting hack to workaround "Type argument Col does not conform to upper bound scala.collection.mutable.Growable[(String, Value)]"
      type GrowableCol <: Growable[Elem]
      given Type[GrowableCol] = Type.of[Col].as[GrowableCol]
      val newCol: Expr[GrowableCol] = newInstanceOfAs[Col, GrowableCol]
      '{ new fm.serializer.StringMapGrowableDeserializer[Value, GrowableCol]($newCol)($elemDeser, $valueDeser) }
    } else if (colTpe <:< TypeRepr.of[Vector[_]]) {
      '{ fm.serializer.StringMapCanBuildFromDeserializer.forVector[Value]($elemDeser, $valueDeser) }
    } else if (colTpe <:< TypeRepr.of[JavaCollection[_]]) {
      ???
      // Note: This was commented out in the Scala 2 implementation
      //val newTpe: Tree = if (isTrait) q"new java.util.ArrayList[$elemTpe]()" else q"new $tpe"
      //q"implicit val $name: fm.serializer.Deserializer[$tpe] = new fm.serializer.JavaCollectionDeserializer[$elemTpe, $tpe]($newTpe); $name"
    } else {
      null
    }

    if (expr === null) return null

    makeProxyForDeserializer(expr.asExprOf[Deserializer[Col]])
  }

  private def makeProxyForDeserializer[T: Type](expr: Expr[Deserializer[T]])(using Quotes): Expr[DeserializerProxy[T]] = {
    // See notes in checkCollectionSerializer for why we use this pattern
    // We want to generate code like this:
    //'{
    //  implicit val colDeserializerProxy: fm.serializer.DeserializerProxy[T] = new fm.serializer.DeserializerProxy[T]()
    //  val colDeserializer: fm.serializer.Deserializer[T] = ${expr.asExprOf[Deserializer[T]]}
    //  colDeserializerProxy.self = colDeserializer
    //  colDeserializer
    //}

    val name: String = Symbol.freshName("underlyingDeserializer")
    val proxyName: String = Symbol.freshName("deserializerProxy")

    // implicit val $proxyName: fm.serializer.DeserializerProxy[T] = new fm.serializer.DeserializerProxy[T]()
    val proxyNameSym: Symbol = Symbol.newVal(Symbol.spliceOwner, proxyName, TypeRepr.of[DeserializerProxy[T]], Flags.Implicit, Symbol.noSymbol)
    val proxyNameValDef: ValDef = ValDefQuotes(proxyNameSym, Some('{ new DeserializerProxy[T]() }.asTerm))

    // val name: fm.serializer.Deserializer[T] = ${expr.asExprOf[Deserializer[T]]}
    val nameSym: Symbol = Symbol.newVal(Symbol.spliceOwner, name, TypeRepr.of[Deserializer[T]], Flags.EmptyFlags, Symbol.noSymbol)
    val nameValDef: ValDef = ValDef(nameSym, Some(expr.asExprOf[Deserializer[T]].asTerm.changeOwner(nameSym)))

    // proxyName.self = name
    val assignSelf: Assign = Assign(Select.unique(Ref(proxyNameSym), "self"), Ref(nameSym))

    Block(List(
      proxyNameValDef,
      nameValDef,
      assignSelf,
    ), Ref(proxyNameSym)).asExprOf[DeserializerProxy[T]]
  }

  /** Equivalent of `(new Of()).asInstanceOf[As]` */
  private def newInstanceOfAs[Of: Type, As: Type]: Expr[As] = {
    newInstanceOf[Of].asExprOf[As]
  }

  /** Equivalent of `new A()` */
  private def newInstanceOf[A: Type]: Expr[A] = {
    if (!hasNoArgsConstructor[A]) errorAndAbort(s"Could not find a no-args constructor for ${TypeRepr.of[A].show}")
    val tpe: TypeRepr = TypeRepr.of[A]
    val cls: Symbol = tpe.classSymbol.getOrElse{ errorAndAbort(s"${tpe.show} is not a class symbol?!?") }
    Apply(Select(New(TypeIdent(cls)), noArgsConstructor[A]), Nil).asExprOf[A]
  }

  // TODO: How to handle the withMacrosDisabled parameter for Scala 3???
  def hasImplicit[T](using Type[T])(using Quotes): Boolean = getImplicit[T].isDefined

  def getImplicit[T](using Type[T])(using Quotes): Option[Expr[T]] = {
    val res: Option[Expr[T]] = Expr.summon[T]
    log(s"getImplicit[${TypeRepr.of[T].show}] - Expr: ${res.map{ _.asTerm.show }}")
    res
  }

  def implicitOr[A](or: => Expr[A])(using Type[A])(using Quotes): Expr[A] = {
    getImplicit[A].getOrElse(or)
  }

  def implicitOrAbort[A](using Type[A])(using Quotes): Expr[A] = {
    getImplicit[A].getOrElse{ errorAndAbort(s"Unable to find implicit ${TypeRepr.of[A].show}") }
  }

  /**
   * Lookup a Primitive given a type T.
   *
   * This is shared for both Serializer and Deserializer
   */
  def findPrimitive[T: Type]: Option[Expr[Primitive[T]]] = {

    val expr: Expr[_] = TypeRepr.of[T] match {
      case t if t =:= TypeRepr.of[String] => '{ Primitive.string }
      case t if t =:= TypeRepr.of[Boolean] => '{ Primitive.boolean }
//      case t if t =:= TypeRepr.of[Byte] => '{ Primitive.byte }
//      case t if t =:= TypeRepr.of[Short] => '{ Primitive.short }
      case t if t =:= TypeRepr.of[Char] => '{ Primitive.char }
      case t if t =:= TypeRepr.of[Int] => '{ Primitive.int }
      case t if t =:= TypeRepr.of[Long] => '{ Primitive.long }
      case t if t =:= TypeRepr.of[Float] => '{ Primitive.float }
      case t if t =:= TypeRepr.of[Double] => '{ Primitive.double }
      case _ => null
    }

    Option(expr.asInstanceOf[Expr[Primitive[T]]])
  }

//  /**
//   * Is the given Type a primitive?
//   *
//   * I'm sure there is a better way to do this...
//   */
//  private def isPrimitive(tpe: TypeRepr): Boolean = if (tpe <:< TypeRepr.of[AnyRef]) false else tpe match {
//    case t if t =:= TypeRepr.of[Boolean] => true
//    case t if t =:= TypeRepr.of[Byte]    => true
//    case t if t =:= TypeRepr.of[Short]   => true
//    case t if t =:= TypeRepr.of[Char]    => true
//    case t if t =:= TypeRepr.of[Int]     => true
//    case t if t =:= TypeRepr.of[Long]    => true
//    case t if t =:= TypeRepr.of[Float]   => true
//    case t if t =:= TypeRepr.of[Double]  => true
//    case _                               => false
//  }

  /**
   * Given a Type return a Tree that produces the default value (0/null/etc...) for that type
   */
  private def defaultValueForType(tpe: TypeRepr): Term = if (tpe <:< TypeRepr.of[AnyRef]) Literal(NullConstant()).asInstanceOfTypeTree(tpe.tpt) else tpe match {
    case t if t =:= TypeRepr.of[Unit]    => Literal(UnitConstant()).asInstanceOfTypeTree(tpe.tpt)
    case t if t =:= TypeRepr.of[Boolean] => Literal(BooleanConstant(false))
    case t if t =:= TypeRepr.of[Byte]    => Literal(ByteConstant(0: Byte))
    case t if t =:= TypeRepr.of[Short]   => Literal(ShortConstant(0: Short))
    case t if t =:= TypeRepr.of[Char]    => Literal(CharConstant('\u0000'))
    case t if t =:= TypeRepr.of[Int]     => Literal(IntConstant(0))
    case t if t =:= TypeRepr.of[Long]    => Literal(LongConstant(0L))
    case t if t =:= TypeRepr.of[Float]   => Literal(FloatConstant(0F))
    case t if t =:= TypeRepr.of[Double]  => Literal(DoubleConstant(0D))
    case _                               => Literal(NullConstant()).asInstanceOfTypeTree(tpe.tpt)
  }

  /**
   * Auto-Generate the FieldImpls for a Java Bean class following the standard bean naming conventions.
   *
   * NOTE: The following restrictions are currently in place:
   *    - no-args constructor
   *    - Each field must have a corresponding getter and setter*
   *    - There must be no extra getters or setters
   *
   *    * - The setter can be missing if the field is of type java.util.List
   *        to support the case where use the getter to get an instance of
   *        java.util.List and then call addAll the populate it.  (This pattern
   *        is used by some JAXB generated code that we have)
   */
  private def makeFieldImplsForJavaBean[T: Type]: Seq[FieldImpl] = {
    val tpe: TypeRepr = TypeRepr.of[T]
    log(s"makeFieldImplsForJavaBean(${tpe.show})")

    if (!hasNoArgsConstructor[T]) {
      log(s"makeFieldImplsForJavaBean(${tpe.show}) - No args constructor not found")
      return Nil
    }

    // Try to extract the java bean information from the type (will throw exceptions if it doesn't work)
    val beanFields: Vector[JavaBeanField] = try {
      getJavaBeanFields[T]()
    } catch {
      case ex: IllegalArgumentException =>
        log(s"makeFieldImplsForJavaBean(${tpe.show}) => IllegalArgumentException: ${ex.getMessage}")
        return Nil
    }

    beanFields.zipWithIndex.map{ case (f, idx) =>
      FieldImpl(
        number = idx + 1,
        name = f.name,
        getter = f.getter,
        setter = f.setter
      )
    }
  }

  /**
   * Auto-Generate the FieldImpls for an IMMUTABLE Java Bean class following the standard bean naming conventions.
   *
   * NOTE: The following restrictions are currently in place:
   *    - Must have a "Value Constructor"
   *    - Each field must have a corresponding getter
   *    - There must be no extra getters
   *
   *    * - This is targeted at the JAXB Value Constructor Plugin and Immutable Plugin:
   *
   *          https://java.net/projects/jaxb2-commons/pages/Value-constructor
   *          https://github.com/mklemm/jaxb2-rich-contract-plugin
   *
   */
  private def makeFieldImplsForJavaBeanImmutable[T: Type]: Seq[FieldImpl] = {
    val tpe: TypeRepr = TypeRepr.of[T]

    log(s"makeFieldImplsForJavaBeanImmutable(${tpe.show})")

    // Try to extract the java bean information from the type (will throw exceptions if it doesn't work)
    val beanFields: Vector[JavaBeanField] = try {
      // The Value-Constructor plugin needs the fields sorted via sortBottomUp=true
      getJavaBeanFields[T](allowMissingSetter = true, sortBottomUp = true)
    } catch {
      case ex: IllegalArgumentException =>
        log(s"makeFieldImplsForJavaBeanImmutable(${tpe.show}) => IllegalArgumentException: ${ex.getMessage}")
        return Nil
    }

    log(s"makeFieldImplsForJavaBeanImmutable(${tpe.show}) - Bean Fields: $beanFields")

    if (beanFields.map{ _.setter }.map{ Option(_) }.exists{ _.isDefined }) {
      log(s"makeFieldImplsForJavaBeanImmutable(${tpe.show}) => Setter(s) defined, not Immutable Java Bean")
      return Nil
    }

    if (!hasConstructorWithSignature[T](beanFields.map{ _.tpe })) {
      log(s"makeFieldImplsForJavaBeanImmutable(${tpe.show}) - Constructor that takes (${beanFields.map{ _.tpe.show }.mkString(", ")}) NOT FOUND")
      return Nil
    }

    beanFields.zipWithIndex.map{ case (f, idx) =>
      FieldImpl(
        number = idx + 1,
        name = f.name,
        getter = f.getter,
        constructorIdx = idx
      )
    }
  }

  /**
   * Auto-Generate the FieldImpls for a Scala Case Class or Case Class Like
   *
   * The class is considered case class like if it has a primary constructor consisting of
   * vals/vars with no other vars in the class.
   */
  private def makeFieldImplsForCaseClass[T: Type]: Seq[FieldImpl] = {
    log(s"makeFieldImplsForCaseClass[${TypeRepr.of[T].show}]")

    val cls: Symbol = Type.of[T].classSymbol

    // This method doesn't support java classes
    if (cls.flags.is(Flags.JavaDefined)) return Nil

    // This work on abstract classes
    // cls.isAbstractType does not seem accurate
    if (cls.flags.is(Flags.Abstract)) return Nil

    val primary: Symbol = cls.primaryConstructor
    if (primary.isNoSymbol) return Nil

    // TODO: Handle multiple parameter lists

    log(s"  primary.paramSymss: ${primary.paramSymss.map{ _.map{ _.returnType.withConcreteTypes[T].show } }}")

    // Tuple2 shows up as List(List(type T1, type T2), List(val _1, val _2))
    // So we need to filter out any types and only grab the parameter lists that are vals
    val params: List[List[Symbol]] = primary.paramSymss.filterNot{ _.forall{ _.isTypeDef }}

    require(params.size <= 1, s"Don't currently support multiple parameter lists. Params: ${primary.paramSymss}")

    val args: List[Symbol] = params.flatten
    val defaults: List[Option[Term]] = primary.defaultValues

    log(s"  args: $args")
    log(s"  defaults: $defaults")

    //
    // We need to verify that there are no other vars or setters outside of the primary constructor
    //
    val ctorArgNames: Set[String] = args.map{ _.name }.toSet

    // Scala 3 TODO: Check this logic for field members vs method members and stuff like 'def myStringVar_=(value: String): Unit'
    val varsOrSetters: Vector[Symbol] = (cls.fieldMembers ++ cls.methodMembers).toVector.filter{ (m: Symbol) => m.flags.is(Flags.Mutable) || m.flags.is(Flags.FieldAccessor /* Was m.isSetter in Scala 2 - Flags.ParamAccessor? */) }.filterNot { (m: Symbol) =>
      val decodedName: String = m.name
      val name: String = if (decodedName.endsWith("_=")) decodedName.substring(0, decodedName.length-2) else decodedName
      ctorArgNames.contains(name)
    }

    log(s"  ctorArgNames: $ctorArgNames")
    log(s"  varsOrSetters: $varsOrSetters")

    // There are left over vars or setters so let's bail
    if (varsOrSetters.nonEmpty) {
      log(s"  varsOrSetters is NON EMPTY: ${varsOrSetters} - BAILING")
      return Nil
    }

    (args zip defaults).zipWithIndex.map{ case ((arg: Symbol, default: Option[Tree]), idx: Int) =>
      val returnType: TypeRepr = arg.returnType.withConcreteTypes[T]

      log(s" FieldImpl - name: ${arg.name} - idx: $idx - returnType: ${returnType.show}")

      FieldImpl(
        number = idx + 1,
        name = arg.name,
        getter = arg.name,
        setter = null,
        constructorIdx = idx,
        serializer = null.asInstanceOf[Term],
        deserializer = null.asInstanceOf[Term],
        tpe = returnType,
        defaultValue = default.orNull.asInstanceOf[Term]
      )
    }
  }

  def tryMakeObjectSerializer[T: Type]: Option[Expr[ObjectSerializer[T]]] = {
    tryMakeObjectSerializerOrDeserializer[T, ObjectSerializer[T]](makeObjectSerializer)
  }

  /**
   * This creates an ObjectSerializer for an interface/trait based on the field information of a concrete type.
   *
   * e.g. You can have a "trait Foo { def name: String }" and a "case class FooImpl(name: String)".  The fields will be read
   *      from FooImpl but the serializer will be for Foo.  This means the Foo must have the same methods as FooImpl for it to work.
   */
  def tryMakeObjectSerializerForInterface[IFACE: Type, CONCRETE: Type]: Option[Expr[ObjectSerializer[IFACE]]] = {
    tryMakeObjectSerializerOrDeserializer[CONCRETE, ObjectSerializer[IFACE]]{ fields => makeObjectSerializer[IFACE](fields) }
  }

  def tryMakeObjectDeserializer[T: Type]: Option[Expr[ObjectDeserializer[T]]] = {
    tryMakeObjectSerializerOrDeserializer[T, ObjectDeserializer[T]](makeObjectDeserializer)
  }

  private def tryMakeObjectSerializerOrDeserializer[T: Type, RES](f: Seq[FieldImpl] => Expr[RES]): Option[Expr[RES]] = {
    val tpe: TypeRepr = TypeRepr.of[T]
    log(s"tryMakeObjectSerializerOrDeserializer(${tpe.show})")

    val fieldImpls: Seq[Seq[FieldImpl]] = Seq(
      extractFieldAnnotations[T],            // Try Annotations
      makeFieldImplsForCaseClass[T],         // Try Auto-Generate for Case Classes
      makeFieldImplsForJavaBeanImmutable[T], // Try Immutable Java Bean
      makeFieldImplsForJavaBean[T]           // Try as a Java Bean
    )

    val renamedFields: Map[String,String] = extractRenameFieldAnnotations[T]

    log(s"Renamed Fields: $renamedFields")

    fieldImpls.find{ _.nonEmpty }.map{ (fields: Seq[FieldImpl]) =>
      // Apply any @RenameField annotation rules
      fields.map{ (f: FieldImpl) => f.copy(name = renamedFields.getOrElse(f.name, f.name))  }
    }.map{ f }
  }

  private def makeSimpleObjectSerializer[T: Type](fields: Seq[FieldImpl]): Expr[SimpleObjectSerializer[T]] = {
    val tpe: TypeRepr = TypeRepr.of[T]

    log(s"makeSimpleObjectSerializer[${tpe.show}]($fields)")

//    val expr: Expr[SimpleObjectSerializer[T]] = '{
//      val $serName: ObjectSerializer[T] = ${makeObjectSerializer[T](fields).tree}
//      val $deserName: ObjectDeserializer[T] = ${makeObjectDeserializer[T](fields).tree}
//      implicit val $name: fm.serializer.SimpleObjectSerializer[T] = new fm.serializer.SimpleObjectSerializer[T]()($serName, $deserName)
//      $name
//    }

    val serSym: Symbol = Symbol.newVal(Symbol.spliceOwner, Symbol.freshName("ser"), TypeRepr.of[ObjectSerializer[T]], Flags.EmptyFlags, Symbol.noSymbol)
    val deserSym: Symbol = Symbol.newVal(Symbol.spliceOwner, Symbol.freshName("deser"), TypeRepr.of[ObjectDeserializer[T]], Flags.EmptyFlags, Symbol.noSymbol)
    val simpleObjectSerializerSym: Symbol = Symbol.newVal(Symbol.spliceOwner, Symbol.freshName("simpleObjectSerializer"), TypeRepr.of[SimpleObjectSerializer[T]], Flags.Implicit, Symbol.noSymbol)

    val block: Block = Block(List(
      ValDef(serSym, Some(makeObjectSerializer[T](fields).asTerm)),
      ValDef(deserSym, Some(makeObjectDeserializer[T](fields).asTerm)),
      ValDefQuotes(simpleObjectSerializerSym, Some('{ new fm.serializer.SimpleObjectSerializer[T]()(${Ident(serSym.termRef).asExprOf[ObjectSerializer[T]]}, ${Ident(deserSym.termRef).asExprOf[ObjectDeserializer[T]]}) }.asTerm))
    ), Ident(simpleObjectSerializerSym.termRef))

    block.asExprOf[SimpleObjectSerializer[T]]
  }

  def makeObjectSerializer[T: Type](fields: Seq[Expr[Field]]): Expr[ObjectSerializer[T]] = {
    makeObjectSerializer(fields.map{ makeFieldImpl(_) })
  }

  /**
   * Attempt to create a SimpleSerializer for the type
   */
  @targetName("makeObjectSerializerImpl")
  private def makeObjectSerializer[T: Type](fields: Seq[FieldImpl]): Expr[ObjectSerializer[T]] = {
    val tpe: TypeRepr = TypeRepr.of[T]

    log(s"makeObjectSerializer[${tpe.show}]($fields)")

    val sortedFields: Vector[FieldImpl] = fillInType[T](cleanFieldImpls(fields))

    val serInfo: ObjectSerializationInfo[T] = ObjectSerializationInfo[T](sortedFields)

    val name: String = Symbol.freshName("objectSerializer")

    // Here is what we are trying to generate:
    //  val expr: Expr[ObjectSerializer[T]] = '{
    //    implicit object $name extends fm.serializer.ObjectSerializer[T] {
    //      ..${serInfo.serializerDeclarations}
    //
    //      final def serializeRaw(output: fm.serializer.RawOutput, v: T): Unit = output.writeRawObject(v)(writeFun)
    //      final def serializeNested(output: fm.serializer.NestedOutput, v: T): Unit = output.writeNestedObject(v)(writeFun)
    //      final def serializeField(output: fm.serializer.FieldOutput, number: Int, name: String, v: T): Unit = output.writeFieldObject(number, name, v)(writeFun)
    //
    //      private[this] val writeFun: Function2[fm.serializer.FieldOutput, T, Unit] = new Function2[fm.serializer.FieldOutput, T, Unit] {
    //        def apply(output: fm.serializer.FieldOutput, obj: T): Unit = {
    //          ..${serInfo.writes}
    //        }
    //      }
    //    }
    //    $name
    //  }

    def decls(cls: Symbol): List[Symbol] = serInfo.serializerDeclarations(cls) ++ List(
      Symbol.newMethod(cls, "serializeRaw", MethodType(List("output", "v"))(_ => List(TypeRepr.of[RawOutput], TypeRepr.of[T]), _ => TypeRepr.of[Unit]), Flags.Override, Symbol.noSymbol),
      Symbol.newMethod(cls, "serializeNested", MethodType(List("output", "v"))(_ => List(TypeRepr.of[NestedOutput], TypeRepr.of[T]), _ => TypeRepr.of[Unit]), Flags.Override, Symbol.noSymbol),
      Symbol.newMethod(cls, "serializeField", MethodType(List("output", "number", "name", "v"))(_ => List(TypeRepr.of[FieldOutput], TypeRepr.of[Int], TypeRepr.of[String], TypeRepr.of[T]), _ => TypeRepr.of[Unit]), Flags.Override, Symbol.noSymbol),
      Symbol.newClass(cls, "WriteFun", List(TypeRepr.of[Object], TypeRepr.of[Function2[FieldOutput, T, Unit]]), writeFunDecls, None),
      Symbol.newVal(cls, "writeFun", TypeRepr.of[Function2[FieldOutput, T, Unit]], Flags.Private, cls),
    )

    def writeFunDecls(cls: Symbol): List[Symbol] = List(
      Symbol.newMethod(cls, "apply", MethodType(List("output", "obj"))(_ => List(TypeRepr.of[FieldOutput], TypeRepr.of[T]), _ => TypeRepr.of[Unit]), Flags.Override, Symbol.noSymbol)
    )

    val modSym: Symbol = Symbol.newModule(Symbol.spliceOwner, name, Flags.Implicit, Flags.EmptyFlags, List(TypeRepr.of[Object], TypeRepr.of[ObjectSerializer[T]]), decls, Symbol.noSymbol)
    val callWriteFun: Expr[Function2[FieldOutput, T, Unit]] = Ident(modSym.declaredField("writeFun").termRef).asExprOf[Function2[FieldOutput, T, Unit]]

    val writeFunCls: Symbol = modSym.declaredType("WriteFun").head
    val writeFunDef: DefDef = DefDef(writeFunCls.declaredMethod("apply").head, { case List(List(output, obj)) => Some(Block(serInfo.writes(modSym, output.asExprOf[FieldOutput], obj.asExprOf[T]), Literal(UnitConstant()))) })

    val body: List[Statement] = serInfo.serializerDefinitions(modSym) ++ List(
      DefDefQuotes(modSym.declaredMethod("serializeRaw").head, { case List(List(output, v)) => Some('{ ${output.asExprOf[RawOutput]}.writeRawObject(${v.asExprOf[T]})($callWriteFun) }.asTerm) }),
      DefDefQuotes(modSym.declaredMethod("serializeNested").head, { case List(List(output, v)) => Some('{ ${output.asExprOf[NestedOutput]}.writeNestedObject(${v.asExprOf[T]})($callWriteFun) }.asTerm) }),
      DefDefQuotes(modSym.declaredMethod("serializeField").head, { case List(List(output, number, name, v)) => Some('{ ${output.asExprOf[FieldOutput]}.writeFieldObject(${number.asExprOf[Int]}, ${name.asExprOf[String]}, ${v.asExprOf[T]})($callWriteFun) }.asTerm) }),
      ClassDef(writeFunCls, List(TypeTree.of[Function2[FieldOutput, T, Unit]]), List(writeFunDef)),
      ValDef(modSym.declaredField("writeFun"), Some(Apply(Select(New(TypeIdent(writeFunCls)), writeFunCls.primaryConstructor), List()))),
    )

    log(s"body: ${body.map{ _.show }.mkString("\n  ","\n  ", "")}")

    val (modValDef: ValDef, modClassDef: ClassDef) = ClassDef.module(modSym, List(TypeTree.of[Object], TypeTree.of[ObjectSerializer[T]]), body)

    log(s"modValDef: ${modValDef.show}")
    log(s"modClassDef: ${modClassDef.show}")

    Block(List(modValDef, modClassDef), Ref(modSym)).asExprOf[ObjectSerializer[T]]
  }

  def makeObjectDeserializer[T: Type](fields: Seq[Expr[Field]]): Expr[ObjectDeserializer[T]] = {
    makeObjectDeserializer(fields.map{ makeFieldImpl(_) })
  }

  @targetName("makeObjectDeserializerImpl")
  private def makeObjectDeserializer[T: Type](fields: Seq[FieldImpl]): Expr[ObjectDeserializer[T]] = {
    val tpe: TypeRepr = TypeRepr.of[T]

    log(s"makeObjectDeserializer[${tpe.show}]($fields)")

    val sortedFields: Vector[FieldImpl] = fillInType[T](cleanFieldImpls(fields))

    val deserInfo: ObjectDeserializationInfo[T] = ObjectDeserializationInfo[T](sortedFields)

    if (!deserInfo.hasMatchingConstructor) errorAndAbort(s"Not sure how to construct ${tpe}.  Details:\n${deserInfo.toPrettyString()}")

    // Here is what we are trying to generate:
    //
    //  val expr: Expr[ObjectDeserializer[T]] = '{
    //    implicit object $name extends fm.serializer.ObjectDeserializer[T] {
    //      ..${deserInfo.deserializerDeclarations}
    //
    //      private[this] val fieldNameToNumberMap: fm.serializer.FieldNameToNumberLookup = new fm.serializer.FieldNameToNumberLookup(..$fieldNameToNumberMapArgs)
    //
    //      final def defaultValue: T = null.asInstanceOf[T]
    //      final def deserializeRaw(input: fm.serializer.RawInput): T = input.readRawObject(readFun)
    //      final def deserializeNested(input: fm.serializer.NestedInput): T = input.readNestedObject(readFun)
    //
    //      private[this] val readFun: Function1[fm.serializer.FieldInput, T] = new Function1[fm.serializer.FieldInput, T] {
    //        def apply(input: fm.serializer.FieldInput): T = {
    //          ..${deserInfo.isSetVars}
    //          ..${deserInfo.readVars}
    //
    //          var done: Boolean = false
    //
    //          while (!done) {
    //            val number: Int = input.readFieldNumber(fieldNameToNumberMap)
    //            (number: @scala.annotation.switch) match {
    //              case ..$readCases
    //            }
    //          }
    //
    //          ..${deserInfo.setDefaultValuesForNonSetVariables}
    //
    //          val obj: T = new ${tpe}(..${deserInfo.ctorParams})
    //          ..${deserInfo.nonCtorSetters}
    //          obj
    //        }
    //      }
    //    }
    //    $name
    //  }

    val name: String = Symbol.freshName("objectDeserializer")

    def decls(cls: Symbol): List[Symbol] = deserInfo.deserializerDeclarations(cls) ++ List(
      Symbol.newVal(cls, "fieldNameToNumberMap", TypeRepr.of[FieldNameToNumberLookup], Flags.Private, cls),
      Symbol.newMethod(cls, "defaultValue", ByNameType(TypeRepr.of[T]), Flags.Override, Symbol.noSymbol),
      Symbol.newMethod(cls, "deserializeRaw", MethodType(List("input"))(_ => List(TypeRepr.of[RawInput]), _ => TypeRepr.of[T]), Flags.Override, Symbol.noSymbol),
      Symbol.newMethod(cls, "deserializeNested", MethodType(List("input"))(_ => List(TypeRepr.of[NestedInput]), _ => TypeRepr.of[T]), Flags.Override, Symbol.noSymbol),
      Symbol.newVal(cls, "readFun", TypeRepr.of[Function1[FieldInput, T]], Flags.Private, cls),
      Symbol.newClass(cls, "ReadFun", List(TypeRepr.of[Object], TypeRepr.of[Function1[FieldInput, T]]), readFunDecls, None),
    )

    def readFunDecls(cls: Symbol): List[Symbol] = List(
      Symbol.newMethod(cls, "apply", MethodType(List("input"))(_ => List(TypeRepr.of[FieldInput]), _ => TypeRepr.of[T]), Flags.Override, Symbol.noSymbol)
    )

    val modSym: Symbol = Symbol.newModule(Symbol.spliceOwner, name, Flags.Implicit, Flags.EmptyFlags, List(TypeRepr.of[Object], TypeRepr.of[ObjectDeserializer[T]]), decls, Symbol.noSymbol)
    val callReadFun: Expr[Function1[FieldInput, T]] = Ident(modSym.declaredField("readFun").termRef).asExprOf[Function1[FieldInput, T]]

    val readFunCls: Symbol = modSym.declaredType("ReadFun").head
    val readFunApplyMethod: Symbol = readFunCls.declaredMethod("apply").head

    val deserInfoForClassAndMethod: ObjectDeserializationInfo[T]#ForClassAndMethod = deserInfo.forClassAndMethod(modSym, readFunApplyMethod)

    val doneSym: Symbol = Symbol.newVal(readFunApplyMethod, "done", TypeRepr.of[Boolean], Flags.Mutable, Symbol.noSymbol)
    val objSym: Symbol = Symbol.newVal(readFunApplyMethod, "obj", TypeRepr.of[T], Flags.EmptyFlags, Symbol.noSymbol)
    val numberSym: Symbol = Symbol.newVal(readFunApplyMethod, "number", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)

    def readCases(input: Expr[FieldInput]): List[CaseDef] = List(
      List(CaseDef(Literal(IntConstant(0)), None, Assign(Ref(doneSym), Literal(BooleanConstant(true))))), // case 0 => done = true
      deserInfoForClassAndMethod.readCases(input),
      List(CaseDef(Wildcard(), None, Apply(Select.unique(input.asTerm, "skipUnknownField"), List()))), // case _ => input.skipUnknownField()
    ).flatten

    def whileBody(input: Expr[FieldInput]): Block = {
      Block(List(
        ValDefQuotes(numberSym, Some('{ $input.readFieldNumber(${Ident(modSym.declaredField("fieldNameToNumberMap").termRef).asExprOf[FieldNameToNumberLookup]}) }.asTerm)), // val number: Int = input.readFieldNumber(fieldNameToNumberMap)
        Match(Ident(numberSym.termRef), readCases(input)), // (number: @scala.annotation.switch) match { ... } TODO: How to annotate in Scala 3?
      ), Literal(UnitConstant()))
    }

    def readFunBody(input: Expr[FieldInput]): Block = {
      given Quotes = readFunApplyMethod.asQuotes

      val ctor: Symbol = constructorWithSignature[T](deserInfoForClassAndMethod.ctorFields.map{ _.tpe })

      val newInstance: Term = if (tpe.typeArgs.isEmpty) {
        // No type args - No need for TypeApply
        // new ${tpe}(..${deserInfo.ctorParams})
        Apply(Select(New(TypeTree.of[T]), ctor), deserInfoForClassAndMethod.ctorParams) // ??? TODO CTOR
      } else {
        // Type args - Must use TypeApply
        // new ${tpe}[$typeArgs](..${deserInfo.ctorParams})
        val typeArgs: List[TypeTree] = tpe.typeArgs.map{ _.tpt }
        Apply(TypeApply(Select(New(TypeTree.of[T]), ctor), typeArgs), deserInfoForClassAndMethod.ctorParams)  // ??? TODO CTOR
      }

      Block(List(
        deserInfoForClassAndMethod.isSetVars, // ..${deserInfo.isSetVars}
        deserInfoForClassAndMethod.readVars,  // ..${deserInfo.readVars}
        List(ValDef(doneSym, Some(Literal(BooleanConstant(false))))), // var done: Boolean = false
        List(While('{ !${Ident(doneSym.termRef).asExprOf[Boolean]} }.asTerm, whileBody(input))), // while (!done) ...
        deserInfoForClassAndMethod.setDefaultValuesForNonSetVariables(input), // ..${deserInfo.setDefaultValuesForNonSetVariables}
        List(ValDef(objSym, Some(newInstance))), // val obj: T = new ${tpe}(..${deserInfo.ctorParams})
        deserInfoForClassAndMethod.nonCtorSetters(Ident(objSym.termRef)), // ..${deserInfo.nonCtorSetters}
      ).flatten, Ident(objSym.termRef))
    }

    val readFunDef: DefDef = DefDef(readFunApplyMethod, { case List(List(input)) => Some(readFunBody(input.asExprOf[FieldInput])) })
    val fieldNameToNumberMapArgs: List[Expr[(String, Int)]] = sortedFields.map{ (f: FieldImpl) => '{ (${Literal(StringConstant(f.name)).asExprOf[String]}, ${Literal(IntConstant(f.number)).asExprOf[Int]}) } }.toList

    val body: List[Statement] = deserInfo.deserializerDefinitions(modSym) ++ List(
      ValDefQuotes(modSym.declaredField("fieldNameToNumberMap"), Some('{ new FieldNameToNumberLookup(${Varargs(fieldNameToNumberMapArgs)}: _*) }.asTerm)),
      DefDefQuotes(modSym.declaredMethod("defaultValue").head, { case _ => Some('{ null.asInstanceOf[T] }.asTerm) }),
      DefDefQuotes(modSym.declaredMethod("deserializeRaw").head, { case List(List(input)) => Some('{ ${input.asExprOf[RawInput]}.readRawObject($callReadFun) }.asTerm) }),
      DefDefQuotes(modSym.declaredMethod("deserializeNested").head, { case List(List(input)) => Some('{ ${input.asExprOf[NestedInput]}.readNestedObject($callReadFun) }.asTerm) }),
      ClassDef(readFunCls, List(TypeTree.of[Function1[FieldInput, T]]), List(readFunDef)),
      ValDef(modSym.declaredField("readFun"), Some(Apply(Select(New(TypeIdent(readFunCls)), readFunCls.primaryConstructor), List()))),
    )

    val (modValDef: ValDef, modClassDef: ClassDef) = ClassDef.module(modSym, List(TypeTree.of[Object], TypeTree.of[ObjectDeserializer[T]]), body)

    val res = Block(List(modValDef, modClassDef), Ref(modSym)).asExprOf[ObjectDeserializer[T]]
    log(s"SUCCESS GENERATING OBJECT DESERIALIZER FOR ${TypeRepr.of[T].show}")
    res
  }

//  /**
//   * Same as extractSingleTypeParamAsSeenFrom except returns an Option
//   *
//   * NOTE: This method returns None for 2 cases:
//   *  1. - The type T isn't <:< the baseType
//   *  2. - There isn't exactly 1 type arg
//   */
//  def getSingleTypeParamAsSeenFrom(tpe: Type, baseType: Type): Option[Type] = {
//    if (tpe <:< baseType) {
//      val args = typeArgsFor(tpe.baseType(baseType.typeSymbol))
//      if (args.size === 1) Some(args.head) else None
//    } else {
//      None
//    }
//  }
//
//  /**
//   * e.g. extractSingleTypeParamAsSeenFrom[Map[String,Int]](typeOf[TraversableOnce[_]]) => (String,Int)
//   */
//  def extractSingleTypeParamAsSeenFrom(tpe: Type, baseType: Type): Type = {
//    val List(arg) = extractTypeParamAsSeenFrom(tpe, baseType)
//    arg
//  }
//
//  def extractTypeParamAsSeenFrom(tpe: Type, baseType: Type): List[Type] = {
//    require(tpe <:< baseType, s"Expected: $tpe <:< $baseType")
//    typeArgsFor(tpe.baseType(baseType.typeSymbol))
//  }
//
//  def substituteGenericTypes(tpe: Type): Type = {
//    val realTypes: List[Type] = typeArgsFor(tpe)
//    val genericTypes: List[Symbol] = tpe.typeSymbol.asClass.typeParams
//    tpe.substituteTypes(genericTypes, realTypes)
//  }
//
//  /**
//   * Determine the return type of a method substituting generic parameters with real types
//   * if applicable.
//   */
//  def resolveType(objTpe: Type, tpe: Type): Type = {
//    val realTypes: List[Type] = typeArgsFor(objTpe)
//    val genericTypes: List[Symbol] = objTpe.typeSymbol.asClass.typeParams
//    require(realTypes.size === genericTypes.size, s"Real Types: $realTypes  Generic Types: $genericTypes")
//
//    val lookupMap: Map[Symbol, Type] = (genericTypes zip realTypes).toMap
//
//    lookupMap.getOrElse(tpe.normalize.typeSymbol, tpe.normalize)
//  }
//
//  /**
//   * Given a type and a method of that type return the default values for the parameters of the method
//   */
//  def defaultValuesForMethod(tpe: Type, method: MethodSymbol): List[Option[Tree]] = {
//    method.paramss.flatten.map{ _.asTerm }.zipWithIndex.map { case (term: TermSymbol, idx: Int) =>
//      if (term.isParamWithDefault) {
//        val defaultName: TermName = newTermName(s"${method.name.encoded}$$default$$${idx+1}")
//        val tree: Option[Tree] = getAccessorForMethod(tpe, defaultName) orElse getAccessorForMethod(companionType(tpe), defaultName)
//        if (tree.isEmpty) ctx.abort(ctx.enclosingPosition, s"Not sure how to access default value.  Tpe: $tpe  method: $method  defaultName: $defaultName")
//        tree
//      } else None
//    }
//  }
//
//  /**
//   * Given a class find the companion object
//   */
//  def companionType(tpe: Type): Type = tpe.typeSymbol.companionSymbol.asModule.moduleClass.asType.toType
//
//  /**
//   * Given a type and the name of a method return the tree that accesses that value
//   */
//  def getAccessorForMethod(tpe: Type, name: TermName): Option[Tree] = {
//    getNoArgsMethod(tpe, name).flatMap { sym: MethodSymbol =>
//      val select: Tree = if (tpe.typeSymbol.isModuleClass) q"${tpe.typeSymbol.companionSymbol}" else q"this"
//
//      sym.asMethod.paramss match {
//        case Nil       => Some(q"$select.$sym")
//        case List(Nil) => Some(q"$select.$sym()")
//        case _ => None
//      }
//    }
//  }
//

  /** Returns all constructors for a Type */
  private def constructors[A: Type]: List[Symbol] = {
    val tpe: TypeRepr = TypeRepr.of[A]
    val cls: Symbol = tpe.classSymbol.get

    // TODO: Do we need to check public vs private on the constructors?
    cls.declarations.filter{ _.isClassConstructor }
  }

  private def hasNoArgsConstructor[A: Type]: Boolean = getNoArgsConstructor[A].isDefined

  private def noArgsConstructor[A: Type]: Symbol = getNoArgsConstructor[A].getOrElse{ errorAndAbort(s"Could not find a no-args constructor for ${TypeRepr.of[A].show}") }

  private def getNoArgsConstructor[A: Type]: Option[Symbol] = {
    constructors[A].find{ (ctor: Symbol) =>
      ctor.paramSymss.filterNot{ _.forall{ _.isTypeDef } } match {
        case List() => true
        case List(List()) => true
        case _ => false
      }
    }
  }

  private def noArgsMethodOrField[T: Type](name: String): Symbol = {
    getNoArgsMethodOrField[T](name).getOrElse{ errorAndAbort(s"${TypeRepr.of[T].show} is missing a no-args method named $name") }
  }

  private def getNoArgsMethodOrField[T: Type](name: String): Option[Symbol] = {
    val cls: Symbol = Type.of[T].classSymbol
    cls.methodMember(name).find{ isNoArgsMethod } orElse cls.fieldMember(name).toOption
  }

  private def getSingleArgMethod[T: Type](name: String): Option[Symbol] = {
    Type.of[T].classSymbol.methodMember(name).find{ isSingleArgMethod }
  }

  private def isNoArgsMethod(method: Symbol): Boolean = {
    method.paramSymss match {
      case List()    => true
      case List(Nil) => true
      case _         => false
    }
  }

  private def isSingleArgMethod(method: Symbol): Boolean = {
    method.paramSymss match {
      case List(List(_)) => true
      case _             => false
    }
  }

  private def hasConstructorWithSignature[T: Type](params: Seq[TypeRepr]): Boolean = getConstructorWithSignature[T](params).isDefined
  private def constructorWithSignature[T: Type](params: Seq[TypeRepr]): Symbol = getConstructorWithSignature[T](params).getOrElse{ errorAndAbort(s"${TypeRepr.of[T]} is missing a constructor that takes params: $params") }

  private def getConstructorWithSignature[T: Type](params: Seq[TypeRepr]): Option[Symbol] = {
    constructors[T].find{ (ctor: Symbol) =>
      log(s"Constructor Params: ${ctor.paramSymss.flatten.filterNot{ _.isTypeDef }.map{ _.returnType.withConcreteTypes[T].show }}")

      ctor.paramSymss.filterNot{ _.forall{ _.isTypeDef } } match {
        case List() if params.size === 0 => true
        case List(Nil) if params.size === 0 => true
        case List(p) if p.size === params.size => (p.map{ _.returnType.withConcreteTypes[T] } zip params).forall{ case (ctorParam: TypeRepr, param: TypeRepr) => param <:< ctorParam }
        case _       => false
      }
    }
  }

  private def hasTransientAnnotation(sym: Symbol): Boolean = {
    // Note: scala.transient also covers the Java transient keyword/modifier
    val res: Boolean = hasAnnotation[scala.transient](sym) || hasAnnotation[java.beans.Transient](sym) || hasAnnotation[javax.xml.bind.annotation.XmlTransient](sym)

    if (res) log(s"hasTransientAnnotation($sym) - true  -  Annotations: ${sym.annotations}")

    res
  }

  /**
   * Note: The setter is optional if the type is a java.util.List in which case the getter is expected to
   *       initialize the list.
   */
  private case class JavaBeanField(name: String, getter: String, setter: String, tpe: TypeRepr)

  /**
   * strict - Require all fields to have matching getters/setters
   * allowMissingSetter - We allow the setter to be missing - this is for the
   *                      case when we expect there to be no setter (but this isn't enforced,
   *                      the caller must verify)
   * sortBottomUp       - See notes in sortFieldsBottomUp and getFieldsForType for info on the ordering
   */
  private def getJavaBeanFields[T: Type](strict: Boolean = true, allowMissingSetter: Boolean = false, sortBottomUp: Boolean = false): Vector[JavaBeanField] = {
    val tpe: TypeRepr = TypeRepr.of[T]
    val cls: Symbol = tpe.classSymbol.get

    log(s"getJavaBeanFields(${tpe.show}, strict: $strict)")

    // cls.fieldMembers is not sorted by declaration order which we need for the constructor param ordering
    //val declarationOrderFieldMembers: List[Symbol] = cls.declarations.filter{ _.isValDef }
    val declarationOrderFieldMembers: List[Symbol] = javaFields(tpe)

    val fields: List[Symbol] = if (sortBottomUp) sortFieldsBottomUp(declarationOrderFieldMembers) else declarationOrderFieldMembers

    var getters: Set[Symbol] = javaBeanGetters[T].toSet
    var setters: Set[Symbol] = javaBeanSetters[T].toSet

    val defs = Vector.newBuilder[JavaBeanField]

    log(s"  Fields: $fields")
    log(s"  Getters: $getters")
    log(s"  Setters: $setters")

    def findGetter(field: Symbol): Option[Symbol] = getters.find{ (m: Symbol) => Seq("get"+field.name.toLowerCase.trim, "is"+field.name.toLowerCase.trim).contains(m.name.toLowerCase.trim) }
    def findSetter(field: Symbol): Option[Symbol] = setters.find{ _.name.toLowerCase.trim === "set"+field.name.toLowerCase.trim }

    // We need to match up fields/getters/setters
    fields.filterNot{ (field: Symbol) =>
      val isTransient: Boolean = hasTransientAnnotation(field)

      // Remove any corresponding transient getters/setters
      if (isTransient) {
        log(s"Skipping transient field: $field")
        findGetter(field).foreach{ getters -= _ }
        findSetter(field).foreach{ setters -= _ }
      }

      isTransient
    }.map{ (field: Symbol) =>
      val tpe: TypeRepr = field.returnType
      val getter: Symbol = findGetter(field).getOrElse{ throw new StacklessIllegalArgumentException(s"Missing Getter for $field") }
      val setter: Option[Symbol] = findSetter(field)

      getters -= getter
      require(getter.returnType =:= tpe, s"Expected getter $getter to have same type as field $field (${tpe.show})")

      setter.foreach{ (s: Symbol) =>
        setters -= s
        require(s.paramSymss.head.head.returnType =:= tpe, s"Expected setter $s to take a single parameter with the same type as field $field (${tpe.show})")
      }

      val getterName: String = getter.name.trim

      val setterName: String = setter.map{ _.name.trim }.getOrElse{
        if (allowMissingSetter) {
          // We are making the assumption that we expect no setters so we
          // return null instead of first checking for the java.util.List case
          // below.
          null
        } else if (tpe <:< TypeRepr.of[java.util.List[_]]) {
          // This is okay - we allow the setter to be None if the getter returns a
          // live java.util.List for us to add items to (JAXB generates this pattern).
          // In this case we just use the getterName
          getterName
        } else {
          throw new StacklessIllegalArgumentException(s"Missing Setter for ${tpe.show} / $getter")
        }
      }

      if (hasTransientAnnotation(field) || hasTransientAnnotation(getter) || setter.exists{ hasTransientAnnotation } ) {
        log(s"Skipping Transient Field/Method:  $field  |  $getter  |  $setter")
        // Ignore the field
      } else {
        val jb: JavaBeanField = JavaBeanField(field.name.trim, getterName, setterName, tpe)
        log(s"Adding JavaBeanField: JavaBeanField(${jb.name}, ${jb.getter}, ${jb.setter}, ${jb.tpe.show}})")
        defs += jb
      }
    }

    // Remove any left over transient getters / setters
    getters.toVector.filter{ hasTransientAnnotation }.foreach{ getters -= _ }
    setters.toVector.filter{ hasTransientAnnotation }.foreach{ setters -= _ }

    if (strict) {
      require(getters.isEmpty, s"Unmatched Java Bean Getters: $getters")
      require(setters.isEmpty, s"Unmatched Java Bean Setters: $setters")
    }

    defs.result()
  }

  /**
   * See the notes below for getFieldsForType().  For that example this gives you:
   * Vector(baz, asd, foo, bar)
   *
   * The XJC Value Constructor plugin generates constructor arguments in this order.
   */
  private def sortFieldsBottomUp(fields: List[Symbol]): List[Symbol] = {
    // The fields are assumed to be in declaration order which is why this works
    val classOrder: List[Symbol] = fields.map{ _.owner }.distinct.reverse
    val grouped: Map[Symbol, Seq[Symbol]] = fields.groupBy{ _.owner }
    classOrder.flatMap{ grouped(_) }
  }

//  /**
//   * These come back "in the linearization order of their owners".  This means that if you have classes:
//   *    class App extends AppBase {
//   *      val foo: String = _
//   *      val bar: String = _
//   *    }
//   *
//   *    class AppBase {
//   *      val baz: String = _
//   *      val asd: String = _
//   *    }
//   *
//   *  We end up with Vector(foo, bar, baz, asd)
//   */
//  def getFieldsForType(tpe: Type): Vector[TermSymbol] = {
//    log(s"getFieldsForType($tpe)  => tpe.members.sorted: ${tpe.members.sorted}")
//
//    // Not sure if this is totally correct.  I don't really see a way to filter the variable/value symbols from a tpe.declarations
//    val res: Vector[TermSymbol] = tpe.members.sorted.filter{ d => d.isTerm && !d.isMethod }.map{ _.asTerm }.toVector
//
//    dedupeInheritedTermSymbols(res)
//  }
//
//  def getPublicMethodForType(tpe: Type): Vector[MethodSymbol] = getMethodsForType(tpe).filter{ _.isPublic }
//
//  def getMethodsForType(tpe: Type): Vector[MethodSymbol] = {
//    tpe.members.sorted.filter{ _.isTerm }.flatMap{ _.asTerm.alternatives }.filter{ _.isMethod }.map{ _.asMethod }.toVector
//  }
//
//  // Same as getMethodsForType but filters by name
//  def getMethodsForType(tpe: Type, name: TermName): Vector[MethodSymbol] = {
//    val sym: Symbol = tpe.member(name)
//
//    if (sym.isTerm) sym.asTerm.alternatives.filter{ _.isMethod }.map{ _.asMethod }.toVector
//    else Vector.empty
//  }

  private def javaFields(tpe: TypeRepr): List[Symbol] = {
    tpe.baseClasses.flatMap{ (cls: Symbol) =>
      cls.declarations.filter{ _.isValDef }
    }
  }

  private def javaBeanGetters[T: Type]: List[Symbol] = {
    val cls: Symbol = Type.of[T].classSymbol

    val all: List[Symbol] = cls.methodMembers.filter{ _.isPublic }.filter{ isNoArgsMethod }.filter{ (m: Symbol) =>
      val name: String = m.name
      val isGetter: Boolean = name.startsWith("get") || ((m.returnType =:= TypeRepr.of[Boolean]  || m.returnType =:= TypeRepr.of[java.lang.Boolean]) && name.startsWith("is"))
      isGetter && name != "getClass" && name != "isInstanceOf"
    }

    // This was needed for Scala 2. Hopefully we don't need for Scala 3
    //dedupeInheritedMethodSymbols(all)
    all
  }

  private def javaBeanSetters[T: Type]: List[Symbol] = {
    val cls: Symbol = Type.of[T].classSymbol

    val all: List[Symbol] = cls.methodMembers.filter{ _.isPublic }.filter{ isSingleArgMethod }.filter{ (m: Symbol) =>
      val name: String = m.name
      name.startsWith("set")
    }

    // Note: This was commented out in Scala 2
    // Since this dedupes based on the return type I don't think is actually what we want.
    // We will probably need to de-dupe based on the type of the single argument.  Since
    // all tests currently pass I'll punt this isuses until it actually becomes a problem.
    //dedupeInheritedMethodSymbols(all)

    all
  }

//  private def dedupeInheritedTypesUsing[T <: TermSymbol](symbols: Vector[T])(getType: T => Type): Vector[T] = {
//
//    // zipWithIndex to retain the ordering
//    val grouped: Vector[Seq[(T,Int)]] = symbols.zipWithIndex.groupBy{ case (sym, idx) => sym.name.decoded }.values.toVector
//
//    val res = grouped.map{ group: Seq[(T,Int)] =>
//      // We want the most specific return type in each group.  This handles
//      // the case of inheriting from an interface with a more specific return
//      // type.
//      group.sortWith{ case (a,b) => getType(a._1) <:< getType(b._1) }.head
//    }.sortBy{ case (symbol, idx) =>
//      // re-sort by the original index
//      idx
//    }.map{ case (symbol, idx) =>
//      symbol
//    }
//
//    res
//  }
//
//  // Note: existing code used the TermSymbol.typeSignature so I'm sticking with that here
//  private def dedupeInheritedTermSymbols(symbols: Vector[TermSymbol]): Vector[TermSymbol] = dedupeInheritedTypesUsing(symbols){ _.typeSignature }
//
//  // Note: existing code used the MethodSymbol.returnType so I'm sticking with that here.
//  private def dedupeInheritedMethodSymbols(symbols: Vector[MethodSymbol]): Vector[MethodSymbol] = dedupeInheritedTypesUsing(symbols){ _.returnType }
}