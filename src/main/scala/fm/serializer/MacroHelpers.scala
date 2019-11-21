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

import fm.common.Implicits._
import java.lang.{Iterable => JavaIterable}
import java.util.{Collection => JavaCollection}
import scala.collection.generic.{CanBuildFrom, Growable}
import scala.reflect.macros._
import scala.util.Try

abstract class MacroHelpers(isDebug: Boolean) { self =>
  val ctx: Context
  import ctx.universe.{Try => UniverseTry, _}

  private def log(s: Any): Unit = if (isDebug) println(s.toString)
  
  private case class ObjectSerializationInfo(objTpe: Type, fields: Vector[FieldImpl]) {
    private val sharedSerializers: Vector[(Type, TermName)] = {
      fields.filter{ _.serializer == null }.map{ _.tpe }.toSet.toVector.map{ tpe: Type =>
        // If the field type is the same as the object type leave the name empty so we reference this serializer
        val name: TermName = if (tpe =:= objTpe) nme.EMPTY else newTermName(ctx.fresh("shared_ser"))
        (tpe, name)
      }
    }

    val infos: Vector[FieldSerializationInfo] = fields.map{ new FieldSerializationInfo(objTpe, _, sharedSerializers) }
    
    // We filter out empty names (which will later be references to "this")
    def serializerDeclarations: Vector[ValDef] = sharedSerializers.filterNot{ case (_,name) => name == nme.EMPTY }.map{ case (tpe, name) =>      
      // Use a more specific type than fm.serializer.Serializer[$tpe] if we know it.
      // We can also use a non-lazy if the implicit is a non-macro
      getImplicit(appliedType(typeOf[Serializer[_]], List(tpe)), withMacrosDisabled = true) match {
        case Some(tree) => q"private[this] val $name: ${tree.tpe} = $tree"
        case None       => q"private[this] lazy val $name: fm.serializer.Serializer[$tpe] = implicitly[fm.serializer.Serializer[$tpe]]"
      }
    } ++ infos.flatMap{ _.serializerValDef }

    def writes: Vector[Tree] = infos.map{ field: FieldSerializationInfo =>
      val name: TermName = field.serializerTermName
      
      if (name == nme.EMPTY) q"serializeField(output, ${field.number}, ${field.name}, ${field.fieldAccessor})"
      else                   q"${name}.serializeField(output, ${field.number}, ${field.name}, ${field.fieldAccessor})"
      
    }
  }
  
  private case class ObjectDeserializationInfo(objTpe: Type, fields: Vector[FieldImpl]) {
    private val sharedDeserializers: Vector[(Type, TermName)] = {
      fields.filter{ _.deserializer == null }.map{ _.tpe }.toSet.toVector.map{ tpe: Type =>
        // If the field type is the same as the object type leave the name empty so we reference this serializer
        val name: TermName = if (tpe =:= objTpe) nme.EMPTY else newTermName(ctx.fresh("shared_deser"))
        (tpe, name)
      }
    }
    
    val infos: Vector[FieldDeserializationInfo] = fields.map{ new FieldDeserializationInfo(objTpe, _, sharedDeserializers) }
    
    def deserializerDeclarations: Vector[ValDef] = sharedDeserializers.filterNot{ case (_,name) => name == nme.EMPTY }.map{ case (tpe, name) =>
      getImplicit(appliedType(typeOf[Deserializer[_]], List(tpe)), withMacrosDisabled = true) match {
        case Some(tree) => q"private[this] val $name: ${tree.tpe} = $tree"
        case None       => q"private[this] lazy val $name: fm.serializer.Deserializer[$tpe] = implicitly[fm.serializer.Deserializer[$tpe]]"
      }
    } ++ infos.flatMap{ _.deserializerValDef }
    
    def readCases: Vector[CaseDef] = infos.map{ field: FieldDeserializationInfo =>
      val name: TermName = field.deserializerTermName
      val deser = if (name == nme.EMPTY) q"deserializeNested(input)" else q"${name}.deserializeNested(input)"
      
      cq"""${field.number} => 
        ${field.readVarName} = $deser
        ${field.isSetVarName} = true
      """
    }
    
    def isSetVars: Vector[ValDef] = infos.map{ _.isSetVarDef }
    def readVars: Vector[ValDef] = infos.map{ _.readVarDef }
    
    def setDefaultValuesForNonSetVariables: Vector[If] = infos.map{ f: FieldDeserializationInfo =>
      val deserializer = if (f.deserializerTermName == nme.EMPTY) q"null" else q"${f.deserializerTermName}"

      q"""if (!${f.isSetVarName}) {
            ${f.readVarName} = ${f.defaultValue}
            input.reportUnsetField(${f.number}, ${f.name}, ${f.hasUserDefinedDefaultValue}, ${deserializer})
        }"""
    }

    def ctorFields: Vector[FieldDeserializationInfo] = infos.filter{ _.field.constructorIdx >= 0 }.sortBy{ _.field.constructorIdx }

    def ctorParams: Vector[Ident] = ctorFields.map{ f: FieldDeserializationInfo => Ident(f.readVarName) }

    def hasMatchingConstructor: Boolean = hasNoArgsConstructor(objTpe) || hasConstructorWithSignature(objTpe, ctorFields.map{ _.tpe })

    def nonCtorSetters: Vector[Tree] = infos.filter{ _.field.setter != null }.map{ f: FieldDeserializationInfo =>
      
      val setterName: TermName = newTermName(f.field.setter)
      
      val tree: Tree = getSingleArgMethod(objTpe, setterName).map{ method: MethodSymbol =>
        //
        // This is the normal case where we have something like setName(...)
        //
        q"obj.${method}(${f.readVarName})"
      }.orElse { getNoArgsMethod(objTpe, setterName).map{ method: MethodSymbol =>
        //
        // This is the special case where we have a field.setter that is really a getter
        // that returns something like a java.util.List that has an addAll method that
        // we must use to set the value
        //
        require(method.returnType =:= f.tpe, s"Expected the return type of $setterName to be ${f.tpe}")
        require(getSingleArgMethod(method.returnType, newTermName("addAll")).isDefined, s"Expected ${method.returnType} to have a method called addAll")
        
        q"obj.${method}().addAll(${f.readVarName})"
      }}.getOrElse{ throw new IllegalArgumentException(s"Invalid Setter (${setterName}) for $objTpe") }
      
      tree
    }

    def toPrettyString(): String = {
      val sb: StringBuilder = new StringBuilder()

      sb += '\n'
      sb ++= "===================================================================================================================================="
      sb += '\n'
      sb ++= s"Type: $objTpe\n"
      sb += '\n'
      sb ++= s"ALL Fields:\n\n"
      fields.foreach { field: FieldImpl =>  sb ++= s"  $field\n" }
      sb += '\n'
      sb ++= s"Constructor Fields:\n\n"
      ctorFields.foreach { field: FieldDeserializationInfo =>  sb ++= s"  ${field.field}\n" }
      sb += '\n'
      sb ++= "Looking for Constructor:\n\n"
      sb ++= s"  List(${ctorFields.toList.map{ _.tpe }})\n"
      sb += '\n'
      sb ++= "Detected Constructors:\n\n"
      getMethodsForType(objTpe, nme.CONSTRUCTOR).foreach{ method: MethodSymbol => sb ++= s"  ${method.paramss.map{ _.map{ _.typeSignature }}}\n" }
      sb += '\n'
      sb ++= "(Note: Generated by ObjectDeserializationInfo.toPrettyString.  Modify this method to add additional information as needed.)\n"
      sb ++= "===================================================================================================================================="
      sb += '\n'
      sb.toString
    }
  }
  
  private case class FieldSerializationInfo(objTpe: Type, field: FieldImpl, sharedSerializers: Vector[(Type, TermName)]) {
    def number: Int = field.number
    def name: String = field.name
    def tpe: Type = field.tpe
    
    require(number > 0, "number must be > 0")
    require(null != name && name.length > 0, "name is empty")  
    require(null != tpe, "tpe is null")
    
    val fieldAccessor: Tree = {
      require(field.getter != null, s"FieldImpl.getter is null for FieldImpl: $field")
      val getter: TermName = newTermName(field.getter)
      val noArgs: Boolean = noArgsMethod(objTpe, getter).paramss.isEmpty
      if (noArgs) q"obj.${getter}" else q"obj.${getter}()"
    }

    // Use either the shared TermName or create a new one if we are using a custom serializer
    val serializerTermName: TermName = {
      if (field.serializer == null) sharedSerializers.find{ _._1 =:= field.tpe }.head._2 else newTermName(ctx.fresh(s"ser_${number}_${name}"))
    }
    
    // There is only a ValDef if we are not using a shared serializer
    val serializerValDef: Option[ValDef] = {
      if (field.serializer != null) {
        //Some(q"lazy val $serializerTermName: fm.serializer.Serializer[$tpe] = ${field.serializer}")
        Some(q"lazy val $serializerTermName: ${field.serializer.tpe} = ${field.serializer}")
      } else {
        None
      }
    }
  }
  
  private case class FieldDeserializationInfo(objTpe: Type, field: FieldImpl, sharedDeserializers: Seq[(Type, TermName)]) {
    def number: Int = field.number
    def name: String = field.name
    def tpe: Type = field.tpe
    
    // Use either the shared TermName or create a new one if we are using a custom deserializer
    val deserializerTermName: TermName = {
      if (field.deserializer == null) sharedDeserializers.find{ _._1 =:= field.tpe }.head._2 else newTermName(ctx.fresh(s"deser_${number}_${name}"))
    }
    
    // There is only a ValDef if we are not using a shared deserializer
    val deserializerValDef: Option[ValDef] = {
      if (field.deserializer != null) {
        //Some(q"lazy val $deserializerTermName: fm.serializer.Deserializer[$tpe] = ${field.deserializer}")
        Some(q"lazy val $deserializerTermName: ${field.deserializer.tpe} = ${field.deserializer}")
      } else {
        None
      }
    }
    
    val isSetVarName: TermName = newTermName(ctx.fresh(s"isset_${number}_${name}"))
    val isSetVarDef: ValDef = q"var $isSetVarName: Boolean = false"
    
    val readVarName: TermName = newTermName(ctx.fresh(s"value_${number}_${name}"))
    val readVarDef: ValDef = q"var $readVarName: $tpe = ${defaultValueForType(tpe)}"

    /** Does this field have a user-defined default value (e.g. foo: Int = 123) */
    def hasUserDefinedDefaultValue: Boolean = field.defaultValue =!= null

    /** The default (e.g. foo: Int = 123) or un-initialized value (e.g. 0 for Int) to use for this field  */
    def defaultValue: Tree = Option(field.defaultValue).getOrElse{
      if (deserializerTermName == nme.EMPTY) q"defaultValue" else q"${deserializerTermName}.defaultValue"
    }
  }

  private case class FieldDeserializerInfo(
    deserializerTermName: TermName, // The variable (actually a val) name of the serializer that we can reference when writing
    deserializerValDef: ValDef,     // The ValDef that defined the serializer for this field
    isSetVarName: TermName,         // This tracks whether or not we have read a value for this field
    isSetVarDef: ValDef,            // This is the ValDef for the isSetVarName
    readVarName: TermName,          // The name of the variable we will read the value into
    readVarDef: ValDef,             // The ValDef of the variable we read the value info
    defaultValue: Tree              // The default value to use for this field if a value isn't read
  )
  
  /** 
   * The companion class to Field
   * 
   * We take the arguments to the Field class (either as an Annotation or as "new Field(...)" expressions)
   * and generate a FieldImpl from them.  This means that for the Int and String fields you can only use 
   * literals that can be used a compile time.  The serializer/deserialize can be arbitrary trees.
   */
  case class FieldImpl(
    number: Int = -1,
    name: String = null,
    getter: String = null,
    setter: String = null,
    constructorIdx: Int = -1,
    serializer: Tree = null,
    deserializer: Tree = null,
    tpe: Type = null,
    defaultValue: Tree = null
  ) {
    def combine(other: FieldImpl): FieldImpl = try {
      
      copy(
        number = combine(number, other.number),
        name = combine(name, other.name),
        getter = combine(getter, other.getter),
        setter = combine(setter, other.setter),
        constructorIdx = combine(constructorIdx, other.constructorIdx),
        serializer = combine(serializer, other.serializer),
        deserializer = combine(deserializer, other.deserializer),
        tpe = combine(tpe, other.tpe)
      )
    } catch {
      case ex: Exception => ctx.abort(ctx.enclosingPosition, s"Could not Combine: ${ex.getMessage()} \nThis: $this\nOther: $other\nStack Trace:\n  ${ex.getStackTrace.mkString("\n  ")}")
    }
    
    private def combine(a: Int, b: Int): Int = {
      if (-1 == a) b
      else if (-1 == b) a
      else {
        require(a == b, s"Values don't match: $a != $b")
        a
      }
    }
    
    private def combine(a: String, b: String): String = combine(Option(a), Option(b))
    private def combine(a: Tree, b: Tree): Tree = combine(Option(a), Option(b))
    
    // Might not be totally accurate for Types (should use =:= for equality checking)
    // but I think we are okay with how we are using it
    private def combine(a: Type, b: Type): Type = combine(Option(a), Option(b))
    
    private def combine[T <: AnyRef](a: Option[T], b: Option[T]): T = {
      require(a == b || a.isDefined ^ b.isDefined, s"Values are different: $a != $b")
      (a orElse b).getOrElse{ null.asInstanceOf[T] }
    }
  }
  
  def cleanFieldImpls(fields: Seq[FieldImpl]): Vector[FieldImpl] = {
    val tmp: Vector[FieldImpl] = fields.toVector.sortBy{ _.number }
      
    tmp.foreach{ f: FieldImpl => require(f.number > 0, s"Number must be > 0: $f") }
    tmp.foreach{ f: FieldImpl => require(null != f.name && f.name.length > 0, s"Name must be non-blank: $f") }
    
    def requireUnique[T](label: String, vals: Seq[T]): Unit = {
      require(vals.size == vals.toSet.size, s"$label is not unique!  $tmp")
    }
    
    requireUnique("number", tmp.map{ _.number })
    requireUnique("name", tmp.map{ _.name })
    requireUnique("constructorIdx", tmp.map{ _.constructorIdx }.filterNot{ _ == -1 })
    
    tmp
  }
  
  def fillInType(objTpe: Type, fields: Vector[FieldImpl]): Vector[FieldImpl] = {
    fields.map{ f: FieldImpl =>
      if (f.tpe != null) f else {
        require(f.getter != null, "Getter is null, not sure how to determine type")
        val method: MethodSymbol = noArgsMethod(objTpe, newTermName(f.getter))
        require(method.paramss.isEmpty || method.paramss == List(List()), s"Getter should have an empty param list.  Tpe: ${objTpe}  Getter: ${f.getter}  Paramss: ${method.paramss}")
        
        val returnType: Type = resolveType(objTpe, method.returnType)
        require(!(returnType =:= typeOf[Unit]), s"Getter return type should not be Unit.  Tpe: ${objTpe}  Getter: ${f.getter}  Paramss: ${returnType}")
        f.copy(tpe = returnType)
      }
    }
  }
  
  /**
   * Given an Expression for a Field, extract out the parameters and turn them into a FieldImpl
   */
  def makeFieldImpl(expr: ctx.Expr[Field]): FieldImpl = makeFieldImpl(expr.tree)
  
  /**
   * Given a Tree for a Field, extract out the parameters and turn them into a FieldImpl
   */
  def makeFieldImpl(tree: Tree): FieldImpl = {
    val q"new fm.serializer.Field(..$args)" = tree
    makeFieldImpl(args)
  }

  def extractRenameFieldAnnotations[T: WeakTypeTag]: Map[String,String] = extractRenameFieldAnnotations(weakTypeOf[T])

  /**
   * Extract RenameField annotations and return a Map of the Old Name => New Name
   */
  def extractRenameFieldAnnotations(tpe: Type): Map[String,String] = {
    log(s"extractRenameFieldAnnotations($tpe)")

    val b = Vector.newBuilder[(String,String)]

    val annotationTpe: Type = typeOf[RenameField]

    // annotations on values & methods in the class
    for {
      member <- tpe.members.toVector
      // Seeing some weirdness with TermSymbols showing up with a space after them
      // (e.g. decoded: "pies " encoded: "pies\u0020") so I guess we need to filter on isMethod?
      if member.isMethod
      ann <- member.annotations
      if ann.tpe =:= annotationTpe
    } {
      log(s"""extractRenameFieldAnnotations($tpe) - Member: $member - "${member.name.decoded}" (${member.getClass}) - Annotations: ${member.annotations}""")

      val sym: MethodSymbol = member.asMethod

      val name: String = sym.name.decoded

      b += ((name, makeRenameFieldImpl(ann.scalaArgs)))
    }

    val isAbstract: Boolean = tpe.typeSymbol.asClass.isAbstractClass

    // annotations on constructor params (for non-abstract classes)
    if (!isAbstract) for {
      ctor <- tpe.member(nme.CONSTRUCTOR).asTerm.alternatives.toVector
      (param, idx) <- ctor.asMethod.paramss.flatten.zipWithIndex // TODO: Handle multiple parameter lists
      ann <- param.annotations
      if ann.tpe =:= annotationTpe
    } {
      log(s"extractRenameFieldAnnotations($tpe) - Constructor Param: $param - Annotations: ${param.annotations}")
      val name: String = param.name.decoded

      b += ((name, makeRenameFieldImpl(ann.scalaArgs)))

    }

    b.result.distinct.toUniqueHashMap
  }

  /**
   * Extract Field annotations for a type and convert them into FieldImpls
   */
  def extractFieldAnnotations[T: WeakTypeTag]: Seq[FieldImpl] = extractFieldAnnotations(weakTypeOf[T])
  
  def extractFieldAnnotations(tpe: Type): Seq[FieldImpl] = {
    log(s"extractFieldAnnotations($tpe)")
    
    val annotationTpe: Type = typeOf[Field]
    
    // annotations on values & methods in the class
    val fields: Vector[FieldImpl] = for {
      member <- tpe.members.toVector
      // Seeing some weirdness with TermSymbols showing up with a space after them
      // (e.g. decoded: "pies " encoded: "pies\u0020") so I guess we need to filter on isMethod?
      if member.isMethod
      ann <- member.annotations
      if ann.tpe =:= annotationTpe
    } yield {
      log(s"""extractFieldAnnotations($tpe) - Member: $member - "${member.name.decoded}" (${member.getClass}) - Annotations: ${member.annotations}""")
      
      val sym: MethodSymbol = member.asMethod
      val returnType: Type = resolveType(tpe, sym.returnType)
      
      val isGetter: Boolean = !(returnType =:= typeOf[Unit]) && sym.paramss.isEmpty
      
      val isSetter: Boolean = returnType =:= typeOf[Unit] && (sym.paramss match {
        case List(List(tpe)) => true
        case _ => false
      })
      
      require(isGetter || isSetter, s"Neither Getter nor Setter??  $sym")
      
      // This will probably work but not sure if it's 100% correct (vs using encoded)
      val name: String = sym.name.decoded
      
      val spec: FieldImpl = makeFieldImpl(ann.scalaArgs, name)
      
      val additionalInfo: FieldImpl = 
        if (isGetter) {
          FieldImpl(getter = name, tpe = resolveType(tpe, sym.returnType))
        } else if (isSetter) {
          val List(List(paramTpe)) = sym.paramss
          FieldImpl(setter = name, tpe = resolveType(tpe, paramTpe.typeSignature))
        } else throw new IllegalArgumentException(s"Both Getter AND Setter?  $sym")
      
      // Populate additional missing fields from FieldImpl
      val missingFields: FieldImpl = {
        val n: String = if (spec.name == null || spec.name == "") name else null
        FieldImpl(name = n)
      }
      
      spec combine additionalInfo combine missingFields
    }
    
    val isAbstract: Boolean = tpe.typeSymbol.asClass.isAbstractClass
    
    // annotations on constructor params (for non-abstract classes)
    val constructorParams: Vector[FieldImpl] = if (isAbstract) Vector.empty else for {
      ctor <- tpe.member(nme.CONSTRUCTOR).asTerm.alternatives.toVector
      (param, idx) <- ctor.asMethod.paramss.flatten.zipWithIndex // TODO: Handle multiple parameter lists
      ann <- param.annotations
      if ann.tpe =:= annotationTpe
    } yield {
      log(s"extractFieldAnnotations($tpe) - Constructor Param: $param - Annotations: ${param.annotations}")
      
      val name: String = param.name.decoded
      
      // If this param is a Val then we can also use it as a getter
      val getter: String = if (param.isTerm && param.asTerm.isVal) name else null
      
      // If the @Field annotation doesn't have a number give it a default
      val defaultNumber: Int = idx + 1
      
      val spec: FieldImpl = makeFieldImpl(ann.scalaArgs, name, defaultNumber)
      
      val additionalInfo: FieldImpl = FieldImpl(constructorIdx = idx, getter = getter, tpe = resolveType(tpe, param.typeSignature))
      
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
    combineFieldImplUsing(combineFieldImplUsing(all, _.number, (num: Int) => num == -1), _.name, (name: String) => name == null)
  }
  
  private def combineFieldImplUsing[T](all: Vector[FieldImpl], groupBy: FieldImpl => T, ignore: T => Boolean): Vector[FieldImpl] = {
    val (toCombine, ignored): (Vector[FieldImpl], Vector[FieldImpl]) = all.partition{ f: FieldImpl => !ignore(groupBy(f)) }
    
    val combined: Vector[FieldImpl] = toCombine.groupBy{ groupBy }.values.map{ _.reduceLeft{ (a,b) => a combine b } }.toVector
    combined ++ ignored
  }

  /**
   * Given the arguments for a RenameField (either from an expression creating a new instance of a Field or from an annotation)
   * return the name field
   */
  def makeRenameFieldImpl(args: Seq[Tree]): String = {
    args match {
      // The single Constructor for RenameField
      case Seq(string(name)) => name
    }
  }

  /**
   * Given the arguments for a Field (either from an expression creating a new instance of a Field or from an annotation)
   * create a FieldImpl
   */
  def makeFieldImpl(args: Seq[Tree], defaultName: String = null, defaultNumber: Int = -1): FieldImpl = {
    args match {
      // Full Constructor:
      case Seq(int(number), string(name), string(getter), string(setter), int(constructorIdx), ser, deser) => FieldImpl(number, name, getter, setter, constructorIdx, ser, deser)
      
      // Without Serializer Arg:
      case Seq(int(number), string(name), string(getter), int(constructorIdx)) => FieldImpl(number, name, getter, null, constructorIdx)
      case Seq(int(number), string(name), string(getter), string(setter)) => FieldImpl(number, name, getter, setter, -1)
      case Seq(int(number), string(name)) => FieldImpl(number, name, null, null, -1)
      case Seq(int(number)) => FieldImpl(number, defaultName, null, null, -1)
      case Seq(string(name)) => FieldImpl(defaultNumber, name, null, null, -1)
      
      // With Serializer Arg:
      // NOTE: The Tree matches for the serializer must come last since they will act as wildcard matches and conflict with some of the above patterns.
      case Seq(int(number), string(name), string(getter), int(constructorIdx), ser) => FieldImpl(number, name, getter, null, constructorIdx, ser, ser)
      case Seq(int(number), string(name), string(getter), string(setter), ser) => FieldImpl(number, name, getter, setter, -1, ser, ser)
      case Seq(int(number), string(name), ser) => FieldImpl(number, name, null, null, -1, ser, ser)
      case Seq(int(number), ser) => FieldImpl(number, defaultName, null, null, -1, ser, ser)
      case Seq(ser) => FieldImpl(defaultNumber, defaultName, null, null, -1, ser, ser)
      case Seq(string(name), ser) => FieldImpl(defaultNumber, name, null, null, -1, ser, ser)
      
      // Empty
      case Seq() => FieldImpl(defaultNumber, defaultName)
    }
  }
  
  /** For pattern matching an Int literal from a tree */
  object int    { def unapply(t: Tree): Option[Int]    = Try{ literal[Int](t)    }.toOption }
  
  /** For pattern matching a String literal from a tree */
  object string { def unapply(t: Tree): Option[String] = Try{ literal[String](t) }.toOption }
  
  /** Extract a Literal Value */
  private def literal[T](t: Tree): T = t match {
    case Literal(Constant(v)) => v.asInstanceOf[T]
  }
    
//  /**
//   * Lookup a Primitive given a type T.
//   * 
//   * This is shared for both Serializer and Deserializer
//   */
//  def findPrimitive[T: WeakTypeTag]: Option[Expr[Primitive[T]]] = {    
//    val tpe: Type = weakTypeOf[T]
//    
//    val expr: Expr[Primitive[_]] = 
//           if (tpe =:= typeOf[Boolean])     reify{ Primitive.boolean }
//      else if (tpe =:= typeOf[Float])       reify{ Primitive.float }
//      else if (tpe =:= typeOf[Double])      reify{ Primitive.double }
//      else if (tpe =:= typeOf[String])      reify{ Primitive.string }
//      else if (tpe =:= typeOf[Array[Byte]]) reify{ Primitive.byteArray }
//      else if (tpe =:= typeOf[Int])         reify{ Primitive.int }
//      else if (tpe =:= typeOf[Long])        reify{ Primitive.long }
//      else null
//    
//    Option(expr.asInstanceOf[Expr[Primitive[T]]])
//  }
  
  /**
   * Common types that I would like to define in CommonTypeImplicits but require the use
   * of a macro to create and I don't want to create a separate project to avoid the
   * separate compilation issue with macros.
   */
  def findCommonType[T: WeakTypeTag]: Option[Expr[SimpleSerializer[T]]] = {
    val tpe: Type = weakTypeOf[T]
    
    log(s"findCommonType[$tpe]")
    
    import java.math.{BigDecimal => JavaBigDecimal}
    
    val expr: Expr[SimpleSerializer[_]] = 
      if (tpe =:= typeOf[JavaBigDecimal]) {
        makeSimpleObjectSerializer(Vector(
          FieldImpl(number = 1, name = "unscaledVal", getter = "unscaledValue", constructorIdx = 0),
          FieldImpl(number = 2, name = "scale", getter = "scale", constructorIdx = 1)
        ))
      } else null
    
    Option(expr).map{ _.asInstanceOf[Expr[SimpleSerializer[T]]] } orElse findEnumSerializer[T]
  }
  
  def findEnumSerializer[T: WeakTypeTag]: Option[Expr[SimpleSerializer[T]]] = {
    val tpe: Type = weakTypeOf[T]
    
    log(s"findEnumSerializer[$tpe]")
    
    val tree: Tree = if (tpe <:< typeOf[java.lang.Enum[_]]) {
      // .valueOf is a java static method so we need a reference to the "companion" for the Java class
      // otherwise the {enumType}.valueOf() method doesn't work in quasiquotes
      val staticTpe: Type = companionType(tpe)
      //q"fm.serializer.Primitive.string.map[$tpe]((enum: $tpe) => enum.name(), (s: String) => $staticTpe.valueOf(s), null)"
      q"fm.serializer.Primitive.int.map[$tpe]((enum: $tpe) => enum.ordinal(), (idx: Int) => $staticTpe.values()(idx), null)"
    } else null
    
    Option(tree).map{ ctx.Expr[SimpleSerializer[T]](_) }
  }
  
  /**
   * Lookup an Option Serializer
   */
  def findOptionSerializer[T: WeakTypeTag]: Option[Expr[Serializer[T]]] = {
    val tpe: Type = weakTypeOf[T]
    
    log(s"findOptionSerializer($tpe)")
    
    val tree: Tree = if (tpe <:< typeOf[Option[_]]) {
      val arg: Type = extractSingleTypeParamAsSeenFrom(tpe, typeOf[Option[_]])
      
      // The .asInstanceOf is for when the type is Some[]
      q"fm.serializer.OptionSerializer[$arg]().asInstanceOf[fm.serializer.Serializer[$tpe]]"
    } else null
      
    Option(tree).map{ ctx.Expr[Serializer[T]](_) }
  }
  
  def findOptionDeserializer[T: WeakTypeTag]: Option[Expr[Deserializer[T]]] = {
    val tpe: Type = weakTypeOf[T]
    
    log(s"findOptionSerializer($tpe)")
    
    val tree: Tree = if (tpe <:< typeOf[Option[_]]) {
      val arg: Type = extractSingleTypeParamAsSeenFrom(tpe, typeOf[Option[_]])

      if (arg <:< typeOf[Boolean]) {
        q"fm.serializer.BooleanOptionDeserializer"
      } else if (arg <:< typeOf[Char]) {
        q"fm.serializer.CharOptionDeserializer"
      } else if (arg <:< typeOf[Int]) {
        q"fm.serializer.IntOptionDeserializer"
      } else if (arg <:< typeOf[Long]) {
        q"fm.serializer.LongOptionDeserializer"
      } else {
        q"fm.serializer.OptionDeserializer[$arg]()"
      }
    } else null
      
    Option(tree).map{ ctx.Expr[Deserializer[T]](_) }
  }

  /**
   * Lookup an AnyVal Serializer
   */
  def findAnyValSerializer[T: WeakTypeTag]: Option[Expr[Serializer[T]]] = {
    val tpe: Type = weakTypeOf[T]

    log(s"findAnyValSerializer($tpe)")

    val tree: Tree = if (tpe <:< typeOf[AnyVal]) {
      val name: TermName = newTermName(ctx.fresh("anyValSerializer"))

      // Since this is an AnyVal there should be a constructor that
      // only has a single val parameter. makeFieldImplsForCaseClass
      // should return that single param
      val fields: Vector[FieldImpl] = fillInType(tpe, cleanFieldImpls(makeFieldImplsForCaseClass(tpe)))

      val serInfo: ObjectSerializationInfo = ObjectSerializationInfo(tpe, fields)
      val field: FieldSerializationInfo = serInfo.infos.head

      log(s"field: $field")

      q"""
        implicit object $name extends fm.serializer.Serializer[$tpe] {
          ..${serInfo.serializerDeclarations}

          def serializeRaw(output: fm.serializer.RawOutput, obj: $tpe): Unit = ${field.serializerTermName}.serializeRaw(output, ${field.fieldAccessor})
          def serializeNested(output: fm.serializer.NestedOutput, obj: $tpe): Unit = ${field.serializerTermName}.serializeNested(output, ${field.fieldAccessor})
          def serializeField(output: fm.serializer.FieldOutput, number: Int, name: String, obj: $tpe): Unit = ${field.serializerTermName}.serializeField(output, number, name, ${field.fieldAccessor})
        }

        $name
       """
    } else null

    Option(tree).map{ ctx.Expr[Serializer[T]](_) }
  }

  def findAnyValDeserializer[T: WeakTypeTag]: Option[Expr[Deserializer[T]]] = {
    val tpe: Type = weakTypeOf[T]

    log(s"findAnyValDeserializer($tpe)")

    val tree: Tree = if (tpe <:< typeOf[AnyVal]) {
      val name: TermName = newTermName(ctx.fresh("anyValDeserializer"))

      // Since this is an AnyVal there should be a constructor that
      // only has a single val parameter. makeFieldImplsForCaseClass
      // should return that single param
      val fields: Vector[FieldImpl] = fillInType(tpe, cleanFieldImpls(makeFieldImplsForCaseClass(tpe)))

      val serInfo: ObjectDeserializationInfo = ObjectDeserializationInfo(tpe, fields)
      val field: FieldDeserializationInfo = serInfo.infos.head

      log(s"field: $field")

      q"""
        implicit object $name extends fm.serializer.Deserializer[$tpe] {
          ..${serInfo.deserializerDeclarations}
          def defaultValue: $tpe = make(${field.deserializerTermName}.defaultValue)
          def deserializeRaw(input: fm.serializer.RawInput): $tpe = make(${field.deserializerTermName}.deserializeRaw(input))
          def deserializeNested(input: fm.serializer.NestedInput): $tpe = make(${field.deserializerTermName}.deserializeNested(input))
          private def make(value: ${field.tpe}): $tpe = new ${tpe}(value)
        }

        $name
       """
    } else null

    Option(tree).map{ ctx.Expr[Deserializer[T]](_) }
  }
 
  /**
   * Lookup a Collection Serializer
   */
  def findCollectionSerializer[T: WeakTypeTag]: Option[Expr[Serializer[T]]] = {
    val tpe: Type = weakTypeOf[T]
    
    log(s"findCollectionSerializer($tpe)")
    
    Vector(
      checkCollectionSerializer[T](tpe, typeOf[TraversableOnce[(String,_)]], typeOf[StringMapSerializer[_,_]], true),
      checkCollectionSerializer[T](tpe, typeOf[TraversableOnce[_]], typeOf[TraversableOnceSerializer[_,_]], false),
      checkCollectionSerializer[T](tpe, typeOf[JavaIterable[_]], typeOf[JavaIterableSerializer[_,_]], false)
    ).flatten.headOption
  }
  
  private def checkCollectionSerializer[T](tpe: Type, colType: Type, serializerType: Type, useTupleValueAsElem: Boolean): Option[Expr[Serializer[T]]] = {
    log(s"checkCollection($tpe, $colType, $serializerType)")
    
    val tree: Tree = if (tpe <:< colType) {
      val typeParam: Type = extractSingleTypeParamAsSeenFrom(tpe, colType)
      
      val elem: Type = if (useTupleValueAsElem) {
        val List(_,valueTpe) = extractTypeParamAsSeenFrom(typeParam, typeOf[(_,_)])
        valueTpe
      } else typeParam
      
      val name: TermName = newTermName(ctx.fresh("colSerializer"))
      val proxyName: TermName = newTermName(ctx.fresh("colSerializerProxy"))
      
      log(s"  elem: $elem,  name: $name")
      
      // This is the serializer type without any type argument placeholders
      // e.g. TraversableOnceSerializer instead of TraversableOnceSerializer[_, _]
      val serTpe: Symbol = serializerType.typeConstructor.typeSymbol
      
      log(s"  serTpe: $serTpe")
      
      //log(s"implicit val $name: fm.serializer.Serializer[$tpe] = new ${serTpe}[$elem, $tpe](); $name")
      
      // We have to use a SerializerProxy here because StringMapSerializer/TraversableOnceSerializer/JavaIterableSerializer
      // all take implicit Serializer parameters that won't work if there is a nested type that is trying to reference
      // the implicit val we are trying to create.
      q"""
        implicit val $proxyName: fm.serializer.SerializerProxy[$tpe] = new fm.serializer.SerializerProxy[$tpe]
        val $name: fm.serializer.Serializer[$tpe] = new ${serTpe}[$elem, $tpe]()
        $proxyName.self = $name
        $name
      """
    } else null
    
    log(s"  Tree: $tree")
    
    if (null != tree) log(tree)
    
    Option(tree).map{ ctx.Expr[Serializer[T]](_) }
  }
  
  /**
   * Lookup a Collection Serializer
   */
  def findCollectionDeserializer[T: WeakTypeTag]: Option[Expr[Deserializer[T]]] = {
    val tpe: Type = weakTypeOf[T]
    
    log(s"findCollectionDeserializer($tpe)")

    val elemTpeOpt: Option[Type] = 
      getSingleTypeParamAsSeenFrom(tpe, typeOf[TraversableOnce[_]]) orElse
      getSingleTypeParamAsSeenFrom(tpe, typeOf[Growable[_]]) orElse
      getSingleTypeParamAsSeenFrom(tpe, typeOf[JavaIterable[_]]) orElse
      typeArgsFor(tpe).headOption
      
    if (elemTpeOpt.isEmpty) {
      log(s"elemTpe is empty for $tpe")
      return None
    }
    
    val elemTpe: Type = elemTpeOpt.get
    
    val tree: Tree = if (elemTpe <:< typeOf[(String,_)]) makeStringMapCollectionDeserializer(tpe, elemTpe) else makeNormalCollectionDeserializer(tpe, elemTpe)
        
    Option(tree).map{ ctx.Expr[Deserializer[T]](_) }
  }
  
  def makeNormalCollectionDeserializer(tpe: Type, elemTpe: Type): Tree = {
    val name: TermName = newTermName(ctx.fresh("colDeserializer"))
    val proxyName: TermName = newTermName(ctx.fresh("colDeserializerProxy"))
    
    val isTrait: Boolean = tpe.typeSymbol.asClass.isTrait

    val VectorTpe = appliedType(typeOf[Vector[_]], elemTpe)
    val IndexedSeqTpe = appliedType(typeOf[scala.collection.IndexedSeq[_]], elemTpe)
    val ImmutableIndexedSeqTpe = appliedType(typeOf[scala.collection.immutable.IndexedSeq[_]], elemTpe)

    val tree: Tree =
      if (tpe <:< VectorTpe) {
        // Note: tpe =:= VectorTpe doesn't work
        //println(s"DETECTED VECTOR - $tpe - new fm.serializer.VectorDeserializer[$elemTpe, $tpe]()")
        q"new fm.serializer.VectorDeserializer[$elemTpe, $tpe]()"
      } else if (tpe <:< typeOf[fm.common.ImmutableArray[_]]) {
        //println(s"DETECTED IMMUTABLE_ARRAY - $tpe - new fm.serializer.VectorDeserializer[$elemTpe, $tpe]()")
        q"new fm.serializer.ImmutableArrayDeserializer[$elemTpe, $tpe]()"
      } else if ((tpe <:< IndexedSeqTpe && IndexedSeqTpe <:< tpe) || (tpe <:< ImmutableIndexedSeqTpe && ImmutableIndexedSeqTpe <:< tpe)) {
        // Note: tpe =:= IndexedSeqTpe doesn't work and we want to make sure we only match scala.collection.IndexedSeq
        //       and NOT sublcasses (since scala.collection.mutable.IndexedSeq is also a subtype but won't work with Vector)
        //println(s"DETECTED INDEXED_SEQ - $tpe - new fm.serializer.VectorDeserializer[$elemTpe, $tpe]()")
        // Default to the VectorDeserializer for any other IndexedSeq type
        q"new fm.serializer.VectorDeserializer[$elemTpe, $tpe]()"
      } else if (hasImplicit(appliedType(typeOf[CanBuildFrom[_,_,_]], List(WildcardType, elemTpe, tpe)))) {
        //println(s"DETECTED CanBuildFrom - $tpe - new fm.serializer.CanBuildFromDeserializer[$elemTpe, $tpe]()")
        q"new fm.serializer.CanBuildFromDeserializer[$elemTpe, $tpe]()"
      } else if (tpe <:< typeOf[Growable[_]] && hasNoArgsConstructor(tpe)) {
        q"new fm.serializer.GrowableDeserializer[$elemTpe, $tpe](new $tpe)"
      } else if (tpe <:< typeOf[JavaCollection[_]]) {
        // TODO: make this more robust
        val newTpe: Tree = if (isTrait) q"new java.util.ArrayList[$elemTpe]()" else q"new $tpe"
        q"new fm.serializer.JavaCollectionDeserializer[$elemTpe, $tpe]($newTpe)"
      } else {
        null
      }
    
    if (tree == null) return null
    
    // See notes in checkCollectionSerializer for why we use this pattern
    q"""
      implicit val $proxyName: fm.serializer.DeserializerProxy[$tpe] = new fm.serializer.DeserializerProxy[$tpe]()
      val $name: fm.serializer.Deserializer[$tpe] = $tree
      $proxyName.self = $name
      $name
    """
  }
  
  def makeStringMapCollectionDeserializer(tpe: Type, elemTpe: Type): Tree = {
    require(elemTpe <:< typeOf[(String,_)], s"Expected elemTpe $elemTpe to be <:< (String,_)")
    
    val List(_,valueTpe) = extractTypeParamAsSeenFrom(elemTpe, typeOf[(_,_)])
    
    val name: TermName = newTermName(ctx.fresh("colDeserializer"))
    val proxyName: TermName = newTermName(ctx.fresh("colDeserializerProxy"))
    
    val isTrait: Boolean = tpe.typeSymbol.asClass.isTrait
       
    val tree: Tree = if (hasImplicit(appliedType(typeOf[CanBuildFrom[_,_,_]], List(WildcardType, elemTpe, tpe)))) {
      q"new fm.serializer.StringMapCanBuildFromDeserializer[$valueTpe, $tpe]()"
    } else if (tpe <:< typeOf[Growable[_]] && hasNoArgsConstructor(tpe)) {
      q"new fm.serializer.StringMapGrowableDeserializer[$valueTpe, $tpe](new $tpe)"
    } else if (tpe <:< typeOf[Vector[_]]) {
      q"fm.serializer.StringMapCanBuildFromDeserializer.forVector[$valueTpe, $tpe]()"
    } else if (tpe <:< typeOf[JavaCollection[_]]) {
      ???
      //val newTpe: Tree = if (isTrait) q"new java.util.ArrayList[$elemTpe]()" else q"new $tpe" 
      //q"implicit val $name: fm.serializer.Deserializer[$tpe] = new fm.serializer.JavaCollectionDeserializer[$elemTpe, $tpe]($newTpe); $name"
    } else {
      null
    }
    
    if (tree == null) return null
      
    // See notes in checkCollectionSerializer for why we use this pattern
    q"""
      implicit val $proxyName: fm.serializer.DeserializerProxy[$tpe] = new fm.serializer.DeserializerProxy[$tpe]()
      val $name: fm.serializer.Deserializer[$tpe] = $tree
      $proxyName.self = $name
      $name
    """
  }
  
  def hasImplicit(implicitTpe: Type, withMacrosDisabled: Boolean = false): Boolean = getImplicit(implicitTpe, withMacrosDisabled).isDefined
  
  def getImplicit(implicitTpe: Type, withMacrosDisabled: Boolean = false): Option[Tree] = {
    val t: Tree = ctx.inferImplicitValue(implicitTpe, silent = true, withMacrosDisabled = withMacrosDisabled, ctx.enclosingPosition)    
    if (t.isEmpty) None else Some(t)
  }

  /**
   * Is the given Type a primitive?
   * 
   * I'm sure there is a better way to do this...
   */
  private def isPrimitive(tpe: Type): Boolean = if (tpe <:< typeOf[AnyRef]) false else tpe.toString match {
    case "Boolean" => true
    case "Byte"    => true
    case "Short"   => true
    case "Char"    => true
    case "Int"     => true
    case "Long"    => true
    case "Float"   => true
    case "Double"  => true
    case _         => false
  }
  
  /**
   * Given a Type return a Tree that produces the default value (0/null/etc...) for that type
   */
  private def defaultValueForType(tpe: Type): Tree = if (tpe <:< typeOf[AnyRef]) q"null" else tpe.toString match {
    case "Unit"    => q"().asInstanceOf[$tpe]"
    case "Boolean" => q"false"
    case "Byte"    => q"(0: Byte)"
    case "Short"   => q"(0: Short)"
    case "Char"    => q"'\0'"
    case "Int"     => q"0"
    case "Long"    => q"0L"
    case "Float"   => q"0F"
    case "Double"  => q"0D"
    case _         => q"null.asInstanceOf[$tpe]"
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
  def makeFieldImplsForJavaBean(tpe: Type): Seq[FieldImpl] = {
    log(s"makeFieldImplsForJavaBean($tpe)")
    
    if (!hasNoArgsConstructor(tpe)) {
      log(s"makeFieldImplsForJavaBean($tpe) - No args constructor not found")
      return Nil
    }
    
    // Try to extract the java bean information from the type (will throw exceptions if it doesn't work)
    val beanFields: Vector[JavaBeanField] = try {
      getJavaBeanFields(tpe)
    } catch {
      case ex: IllegalArgumentException =>
        log(s"makeFieldImplsForJavaBean($tpe) => IllegalArgumentException: ${ex.getMessage}")
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
  def makeFieldImplsForJavaBeanImmutable(tpe: Type): Seq[FieldImpl] = {
    log(s"makeFieldImplsForJavaBeanImmutable($tpe)")
    
    // Try to extract the java bean information from the type (will throw exceptions if it doesn't work)
    val beanFields: Vector[JavaBeanField] = try {
      // The Value-Constructor plugin needs the fields sorted via sortBottomUp=true
      getJavaBeanFields(tpe, allowMissingSetter = true, sortBottomUp = true)
    } catch {
      case ex: IllegalArgumentException =>
        log(s"makeFieldImplsForJavaBeanImmutable($tpe) => IllegalArgumentException: ${ex.getMessage}")
        return Nil
    }
    
    log(s"makeFieldImplsForJavaBeanImmutable($tpe) - Bean Fields: $beanFields")
    
    if (beanFields.map{ _.setter }.map{ Option(_) }.exists{ _.isDefined }) {
      log(s"makeFieldImplsForJavaBeanImmutable($tpe) => Setter(s) defined, not Immutable Java Bean")
      return Nil
    }
    
    if (!hasConstructorWithSignature(tpe, beanFields.map{ _.tpe })) {
      log(s"makeFieldImplsForJavaBeanImmutable($tpe) - Constructor that takes (${beanFields.map{ _.tpe }}) not found")
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
  def makeFieldImplsForCaseClass(tpe: Type): Seq[FieldImpl] = {
    // This method doesn't support java classes
    if (tpe.typeSymbol.asClass.isJava) return Nil
    
    // This work on abstract classes
    if (tpe.typeSymbol.asClass.isAbstractClass) return Nil
    
    // We now support more than just case classes
    //if (!tpe.typeSymbol.asClass.isCaseClass) return Nil
    
    val primary: MethodSymbol = tpe.declaration(nme.CONSTRUCTOR).asTerm.alternatives.collectFirst {
      case ctor: MethodSymbol if ctor.isPrimaryConstructor => ctor
    }.headOption.getOrElse{ return Nil }
    
    // TODO: Handle multiple parameter lists
    require(primary.paramss.size <= 1, "Don't currently support multiple parameter lists")
    
    val args: List[Symbol] = primary.paramss.flatten
    val defaults: List[Option[Tree]] = defaultValuesForMethod(tpe, primary)
    
    //
    // We need to verify that there are no other vars or setters outside of the primary constructor
    //
    val ctorArgNames: Set[String] = args.map{ _.name.decoded }.toSet
    
    val varsOrSetters: Vector[MethodSymbol] = tpe.members.toVector.filter{ _.isMethod }.map{ _.asMethod }.filter{ m: MethodSymbol => m.isVar || m.isSetter }.filterNot { m: MethodSymbol =>
      val decodedName: String = m.name.decoded
      val name: String = if (decodedName.endsWith("_=")) decodedName.substring(0, decodedName.length-2) else decodedName
      ctorArgNames.contains(name)
    }
    
    // There are left over vars or setters so let's bail
    if (varsOrSetters.nonEmpty) return Nil
    
    (args zip defaults).zipWithIndex.map{ case ((arg: Symbol, default: Option[Tree]), idx: Int) =>
      val returnType: Type = resolveType(tpe, arg.typeSignature)
      
      FieldImpl(
        number = idx + 1,
        name = arg.name.decoded,
        getter = arg.name.decoded,
        setter = null,
        constructorIdx = idx,
        serializer = null,
        deserializer = null,
        tpe = returnType,
        defaultValue = default.orNull
      )
    }
  }
  
  def tryMakeObjectSerializer[T: WeakTypeTag](): Option[Expr[ObjectSerializer[T]]] = {
    tryMakeObjectSerializerOrDeserializer[T, ObjectSerializer[T]](makeObjectSerializer)
  }
  
  /**
   * This creates an ObjectSerializer for an interface/trait based on the field information of a concrete type.
   * 
   * e.g. You can have a "trait Foo { def name: String }" and a "case class FooImpl(name: String)".  The fields will be read
   *      from FooImpl but the serializer will be for Foo.  This means the Foo must have the same methods as FooImpl for it to work.
   */
  def tryMakeObjectSerializerForInterface[IFACE: WeakTypeTag, CONCRETE: WeakTypeTag](): Option[Expr[ObjectSerializer[IFACE]]] = {
    tryMakeObjectSerializerOrDeserializer[CONCRETE, ObjectSerializer[IFACE]]{ fields => makeObjectSerializer[IFACE](fields) }
  }
  
  def tryMakeObjectDeserializer[T: WeakTypeTag](): Option[Expr[ObjectDeserializer[T]]] = {
    tryMakeObjectSerializerOrDeserializer[T, ObjectDeserializer[T]](makeObjectDeserializer)
  }
  
  private def tryMakeObjectSerializerOrDeserializer[T: WeakTypeTag, RES](f: Seq[FieldImpl] => Expr[RES]): Option[Expr[RES]] = {
    val tpe: Type = weakTypeOf[T]
    log(s"tryMakeObjectSerializerOrDeserializer($tpe)")
    
    val fieldImpls: Seq[Seq[FieldImpl]] = Seq(
      extractFieldAnnotations(tpe),    // Try Annotations
      makeFieldImplsForCaseClass(tpe), // Try Auto-Generate for Case Classes
      makeFieldImplsForJavaBeanImmutable(tpe), // Try Immutable Java Bean
      makeFieldImplsForJavaBean(tpe)   // Try as a Java Bean
    )

    val renamedFields: Map[String,String] = extractRenameFieldAnnotations(tpe)

    log(s"Renamed Fields: $renamedFields")

    fieldImpls.find{ _.nonEmpty }.map{ fields: Seq[FieldImpl] =>
      // Apply any @RenameField annotation rules
      fields.map{ f: FieldImpl => f.copy(name = renamedFields.getOrElse(f.name, f.name))  }
    }.map{ f }
  }
  
  def makeSimpleObjectSerializer[T: WeakTypeTag](fields: Seq[FieldImpl]): Expr[SimpleObjectSerializer[T]] = {
    val tpe: Type = weakTypeOf[T]
    
    log(s"makeSimpleObjectSerializer[$tpe]($fields)")
    
    val serName: TermName = newTermName(ctx.fresh("ser"))
    val deserName: TermName = newTermName(ctx.fresh("deser"))
    val name: TermName = newTermName(ctx.fresh("simpleObjectSerializer"))
    
    val tree: Tree = q"""
      val $serName = ${makeObjectSerializer[T](fields).tree}
      val $deserName = ${makeObjectDeserializer[T](fields).tree}
      implicit val $name: fm.serializer.SimpleObjectSerializer[$tpe] = new fm.serializer.SimpleObjectSerializer[$tpe]()($serName, $deserName)
      $name
    """
    
    ctx.Expr[SimpleObjectSerializer[T]](tree)
  }
  
  /**
   * Attempt to create a SimpleSerializer for the type
   */
  def makeObjectSerializer[T: WeakTypeTag](fields: Seq[FieldImpl]): Expr[ObjectSerializer[T]] = {
    val tpe: Type = weakTypeOf[T]
    
    log(s"tryMakeObjectSerializer[$tpe]($fields)")
    
    val sortedFields: Vector[FieldImpl] = fillInType(tpe, cleanFieldImpls(fields))
    
    val serInfo: ObjectSerializationInfo = ObjectSerializationInfo(tpe, sortedFields)
    
    val name: TermName = newTermName(ctx.fresh("objectSerializer"))
    
    val tree: Tree = q"""
    implicit object $name extends fm.serializer.ObjectSerializer[$tpe] {
      ..${serInfo.serializerDeclarations}
    
      final def serializeRaw(output: fm.serializer.RawOutput, v: $tpe): Unit = output.writeRawObject(v){ writeFun }
      final def serializeNested(output: fm.serializer.NestedOutput, v: $tpe): Unit = output.writeNestedObject(v){ writeFun }
      final def serializeField(output: fm.serializer.FieldOutput, number: Int, name: String, v: $tpe): Unit = output.writeFieldObject(number, name, v){ writeFun }
    
      private[this] val writeFun: Function2[fm.serializer.FieldOutput, $tpe, Unit] = new Function2[fm.serializer.FieldOutput, $tpe, Unit] {
        def apply(output: fm.serializer.FieldOutput, obj: $tpe): Unit = {
          ..${serInfo.writes}
        }
      }
    }
    $name
    """
    
    ctx.Expr[ObjectSerializer[T]](tree)
  }
  
  def makeObjectDeserializer[T: WeakTypeTag](fields: Seq[FieldImpl]): Expr[ObjectDeserializer[T]] = {
    val tpe: Type = weakTypeOf[T]
    
    log(s"tryMakeObjectDeserializer[$tpe]($fields)")
    
    val sortedFields: Vector[FieldImpl] = fillInType(tpe, cleanFieldImpls(fields))

    val deserInfo: ObjectDeserializationInfo = ObjectDeserializationInfo(tpe, sortedFields)

    if (!deserInfo.hasMatchingConstructor) ctx.abort(ctx.enclosingPosition, s"Not sure how to construct ${tpe}.  Details:\n${deserInfo.toPrettyString}")

    val name: TermName = newTermName(ctx.fresh("objectDeserializer"))
        
    val readCases: Seq[CaseDef] = Vector(
      Vector(cq"0 => done = true"),
      deserInfo.readCases,
      Vector(cq"_ => input.skipUnknownField()")
    ).flatten
    
    val fieldNameToNumberMapArgs: Seq[Tree] = sortedFields.map{ f: FieldImpl => q"(${f.name}, ${f.number})" }
    
    val tree: Tree = q"""
    implicit object $name extends fm.serializer.ObjectDeserializer[$tpe] {
      ..${deserInfo.deserializerDeclarations}
    
      private[this] val fieldNameToNumberMap: scala.collection.immutable.Map[String, Int] = scala.collection.immutable.HashMap(..$fieldNameToNumberMapArgs)
    
      final def defaultValue: $tpe = null.asInstanceOf[$tpe]
      final def deserializeRaw(input: fm.serializer.RawInput): $tpe = input.readRawObject{ readFun }
      final def deserializeNested(input: fm.serializer.NestedInput): $tpe = input.readNestedObject{ readFun }
      
      private[this] val readFun: Function1[fm.serializer.FieldInput, $tpe] = new Function1[fm.serializer.FieldInput, $tpe] {
        def apply(input: fm.serializer.FieldInput): $tpe = {
          ..${deserInfo.isSetVars}
          ..${deserInfo.readVars}
          
          var done: Boolean = false
          
          while (!done) {
            val number: Int = input.readFieldNumber(fieldNameToNumberMap)
            (number: @scala.annotation.switch) match {
              case ..$readCases
            }
          }
              
          ..${deserInfo.setDefaultValuesForNonSetVariables}
              
          val obj: $tpe = new ${tpe}(..${deserInfo.ctorParams})
          ..${deserInfo.nonCtorSetters}
          obj
        }
      }
    }
    $name
    """
    
    ctx.Expr[ObjectDeserializer[T]](tree)
  }
  
  /**
   * Same as extractSingleTypeParamAsSeenFrom except returns an Option
   * 
   * NOTE: This method returns None for 2 cases:
   *  1. - The type T isn't <:< the baseType
   *  2. - There isn't exactly 1 type arg
   */
  def getSingleTypeParamAsSeenFrom(tpe: Type, baseType: Type): Option[Type] = {
    if (tpe <:< baseType) {
      val args = typeArgsFor(tpe.baseType(baseType.typeSymbol))
      if (args.size == 1) Some(args.head) else None
    } else {
      None
    }
  }
  
  /**
   * e.g. extractSingleTypeParamAsSeenFrom[Map[String,Int]](typeOf[TraversableOnce[_]]) => (String,Int)
   */
  def extractSingleTypeParamAsSeenFrom(tpe: Type, baseType: Type): Type = {
    val List(arg) = extractTypeParamAsSeenFrom(tpe, baseType)
    arg
  }
  
  def extractTypeParamAsSeenFrom(tpe: Type, baseType: Type): List[Type] = {
    require(tpe <:< baseType, s"Expected: $tpe <:< $baseType")
    typeArgsFor(tpe.baseType(baseType.typeSymbol))
  }
  
  def substituteGenericTypes(tpe: Type): Type = {
    val realTypes: List[Type] = typeArgsFor(tpe)
    val genericTypes: List[Symbol] = tpe.typeSymbol.asClass.typeParams
    tpe.substituteTypes(genericTypes, realTypes)
  }
  
  /**
   * Determine the return type of a method substituting generic parameters with real types
   * if applicable.
   */
  def resolveType(objTpe: Type, tpe: Type): Type = {
    val realTypes: List[Type] = typeArgsFor(objTpe)
    val genericTypes: List[Symbol] = objTpe.typeSymbol.asClass.typeParams
    require(realTypes.size == genericTypes.size, s"Real Types: $realTypes  Generic Types: $genericTypes")
    
    val lookupMap: Map[Symbol, Type] = (genericTypes zip realTypes).toMap
    
    lookupMap.getOrElse(tpe.normalize.typeSymbol, tpe.normalize)
  }

  /**
   * Determine the type arguments for a type.   
   * e.g.: 
   *  List[Int] => Int
   *  Map[String,Int] => String,Int
   */
  def typeArgsFor(tpe: Type): List[Type] = tpe match {
    case ref: TypeRef => ref.args.map{ _.normalize }
    case _ => Nil
  }
  
  /**
   * Given a type a a method of that type return the default values for the parameters of the method
   */
  def defaultValuesForMethod(tpe: Type, method: MethodSymbol): List[Option[Tree]] = {
    method.paramss.flatten.map{ _.asTerm }.zipWithIndex.map { case (term: TermSymbol, idx: Int) =>
      if (term.isParamWithDefault) {
        val defaultName: TermName = newTermName(s"${method.name.encoded}$$default$$${idx+1}")
        val tree: Option[Tree] = getAccessorForMethod(tpe, defaultName) orElse getAccessorForMethod(companionType(tpe), defaultName)
        if (tree.isEmpty) ctx.abort(ctx.enclosingPosition, s"Not sure how to access default value.  Tpe: $tpe  method: $method  defaultName: $defaultName")
        tree
      } else None
    }
  }
  
  /**
   * Given a class find the companion object
   */
  def companionType(tpe: Type): Type = tpe.typeSymbol.companionSymbol.asModule.moduleClass.asType.toType
  
  /**
   * Given a type and the name of a method return the tree that accesses that value
   */
  def getAccessorForMethod(tpe: Type, name: TermName): Option[Tree] = {
    getNoArgsMethod(tpe, name).flatMap { sym: MethodSymbol =>
      val select: Tree = if (tpe.typeSymbol.isModuleClass) q"${tpe.typeSymbol.companionSymbol}" else q"this"
  
      sym.asMethod.paramss match {
        case Nil       => Some(q"$select.$sym")
        case List(Nil) => Some(q"$select.$sym()")
        case _ => None
      }
    }
  }
  
  def hasNoArgsConstructor(tpe: Type): Boolean = getNoArgsConstructor(tpe).isDefined
  def noArgsConstructor(tpe: Type): MethodSymbol = noArgsMethod(tpe, nme.CONSTRUCTOR)
  def getNoArgsConstructor(tpe: Type): Option[MethodSymbol] = getNoArgsMethod(tpe, nme.CONSTRUCTOR)
  
  def noArgsMethod(tpe: Type, name: TermName): MethodSymbol = getNoArgsMethod(tpe, name).getOrElse{ ctx.abort(ctx.enclosingPosition, s"$tpe is missing a no-args method named $name") }
  
  def getNoArgsMethod(tpe: Type, name: TermName): Option[MethodSymbol] = getMethodsForType(tpe, name).find{ isNoArgsMethod }
  def getSingleArgMethod(tpe: Type, name: TermName): Option[MethodSymbol] = getMethodsForType(tpe, name).find{ isSingleArgMethod }
  
  def isNoArgsMethod(method: MethodSymbol): Boolean = {
    method.paramss match {
      case List()    => true
      case List(Nil) => true
      case _         => false
    }
  }
  
  def isSingleArgMethod(method: MethodSymbol): Boolean = {
    method.paramss match {
      case List(List(_)) => true
      case _             => false
    }
  }
  
  def hasConstructorWithSignature(tpe: Type, params: Seq[Type]): Boolean = getConstructorWithSignature(tpe, params).isDefined
  def constructorWithSignature(tpe: Type, params: Seq[Type]): MethodSymbol = getConstructorWithSignature(tpe, params).getOrElse{ ctx.abort(ctx.enclosingPosition, s"$tpe is missing a constructor that takes params: $params") }
  
  def getConstructorWithSignature(tpe: Type, params: Seq[Type]): Option[MethodSymbol] = getMethodsForType(tpe, nme.CONSTRUCTOR).find { method: MethodSymbol =>
    method.paramss match {
      case List(p) if p.size == params.size => (p.map{ _.typeSignature } zip params).forall{ case (ctorParam: Type, param: Type) => param <:< resolveType(tpe, ctorParam) }
      case _       => false
    }
  }
  
  private def ScalaTransientType   : Type = typeOf[scala.transient]
  private def JavaBeanTransientType: Type = typeOf[java.beans.Transient]
  private def XmlTransientType     : Type = typeOf[javax.xml.bind.annotation.XmlTransient]
  
  /**
   * We have to "initialize" the Symbol before some calls (e.g. sym.annotations)
   * will work.  Note:  In 2.10 we have to call sym.typeSignature but in 2.11 its sym.info
   * 
   * https://groups.google.com/forum/#!topic/scala-user/Ft_lUf5-ujE
   * 
   * Update: Seems like most Symbols are already initialized and the problem
   *         was the Java/Scala compile order.  But leaving this in here
   *         in case it becomes a problem in the future or under 2.11
   */
  private def initSymbol(sym: Symbol): Unit = sym.typeSignature
  
  //
  // HACK HACK HACK
  //
  // There doesn't appear to be any other way to check if a java field is marked transient
  // other than using java reflection which requires us to have a Class instance.  So
  // we create a throwaway ClassLoader to load the class.  If the Java class is part
  // of the project that is using the serializer macros then you *might* need this in your build.sbt:
  //
  //   compileOrder := CompileOrder.JavaThenScala
  //
  // MUST lazy load this since otherwise ctx won't be initialized.  We also only need the ClassLoader
  // if we are working with Java classes.
  private lazy val classLoader: ClassLoader = new java.net.URLClassLoader(ctx.classPath.toArray)
  
  /**
   * HACK to check for java transient field
   */
  private def isJavaTransient(sym: Symbol): Boolean = {
    if (!sym.isJava || !sym.isTerm || !sym.asTerm.isVar) return false

    val className: String = sym.owner.asClass.fullName
    
    log(s"isJavaTransient - Loading $className")
    
    val clazz: Class[_] = classLoader.loadClass(className)
    
    log(s"isJavaTransient - Checking field ${sym.name.decoded}")
    
    java.lang.reflect.Modifier.isTransient(clazz.getDeclaredField(sym.name.decoded).getModifiers())
  }
  
  def hasTransientAnnotation(sym: Symbol): Boolean = {
    log(s"hasTransientAnnotation($sym)  -  Annotations: ${sym.annotations}")
    initSymbol(sym)
    isJavaTransient(sym) || sym.annotations.map{ _.tpe }.exists{ ann: Type => ann =:= ScalaTransientType || ann =:= JavaBeanTransientType || ann =:= XmlTransientType }
  }

  /**
   * Note: The setter is optional if the type is a java.util.List in which case the getter is expected to
   *       initialize the list.
   */
  case class JavaBeanField(name: String, getter: String, setter: String, tpe: Type)
  
  /**
   * strict - Require all fields to have matching getters/setters
   * allowMissingSetter - We allow the setter to be missing - this is for the
   *                      case when we expect there to be no setter (but this isn't enforced, 
   *                      the caller must verify)
   * sortBottomUp       - See notes in sortFieldsBottomUp and getFieldsForType for info on the ordering
   */
  def getJavaBeanFields(tpe: Type, strict: Boolean = true, allowMissingSetter: Boolean = false, sortBottomUp: Boolean = false): Vector[JavaBeanField] = {
    log(s"getJavaBeanFields($tpe, strict: $strict)")
    
    val fields: Vector[TermSymbol] = if (sortBottomUp) sortFieldsBottomUp(getFieldsForType(tpe)) else getFieldsForType(tpe)
    
    var getters: Set[MethodSymbol] = javaBeanGetters(tpe).toSet
    var setters: Set[MethodSymbol] = javaBeanSetters(tpe).toSet
    
    val defs = Vector.newBuilder[JavaBeanField]
    
    log(s"  Fields: $fields")
    log(s"  Getters: $getters")
    log(s"  Setters: $setters")
    
    def findGetter(field: TermSymbol): Option[MethodSymbol] = getters.find{ m: MethodSymbol => Seq("get"+field.name.decoded.toLowerCase.trim, "is"+field.name.decoded.toLowerCase.trim).contains(m.name.decoded.toLowerCase.trim) }
    def findSetter(field: TermSymbol): Option[MethodSymbol] = setters.find{ _.name.decoded.toLowerCase.trim == "set"+field.name.decoded.toLowerCase.trim }
    
    // We need to match up fields/getters/setters
    fields.filterNot{ field: TermSymbol =>
      //
      // HACK TO GET THIS WORKING WITH THE JVM-SERIALIZERS BENCHMARK
      //
      //val isTransient: Boolean = hasTransientAnnotation(field) || field.name.decoded.trim == "hasBitrate"
      //
      
      val isTransient: Boolean = hasTransientAnnotation(field)
      
      // Remove any corresponding transient getters/setters
      if (isTransient) {
        log(s"Skipping transient field: $field")
        findGetter(field).foreach{ getters -= _ }
        findSetter(field).foreach{ setters -= _ }
      }
      
      isTransient
    }.map{ field: TermSymbol =>
      val tpe: Type = field.typeSignature
      val getter: MethodSymbol = findGetter(field).getOrElse{ throw new IllegalArgumentException(s"Missing Getter for $field") }
      val setter: Option[MethodSymbol] = findSetter(field)

      getters -= getter
      require(getter.returnType =:= tpe, s"Expected getter $getter to have same type as field $field ($tpe)")
      
      setter.foreach{ s: MethodSymbol =>
        setters -= s
        require(s.paramss.head.head.typeSignature =:= tpe, s"Expected setter $s to take a single parameter with the same type as field $field ($tpe)")
      }
      
      val getterName: String = getter.name.decoded.trim
      
      val setterName: String = setter.map{ _.name.decoded.trim }.getOrElse{
        if (allowMissingSetter) {
          // We are making the assumption that we expect no setters so we
          // return null instead of first checking for the java.util.List case
          // below.
          null
        } else if (tpe <:< typeOf[java.util.List[_]]) {
          // This is okay - we allow the setter to be None if the getter returns a 
          // live java.util.List for us to add items to (JAXB generates this pattern).
          // In this case we just use the getterName
          getterName
        } else {
          throw new IllegalArgumentException(s"Missing Setter for $tpe / $getter")
        }
      }
      
      if (hasTransientAnnotation(field) || hasTransientAnnotation(getter) || setter.exists{ hasTransientAnnotation } ) {
        log(s"Skipping Transient Field/Method:  $field  |  $getter  |  $setter")
        // Ignore the field
      } else {
        val jb = JavaBeanField(field.name.decoded.trim, getterName, setterName, tpe)
        log(s"Adding JavaBeanField: $jb")
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
    
    defs.result
  }
  
  /**
   * See the notes below for getFieldsForType().  For that example this gives you:
   * Vector(baz, asd, foo, bar)
   * 
   * The XJC Value Constructor plugin generates constructor arguments in this order.
   */
  def sortFieldsBottomUp(fields: Vector[TermSymbol]): Vector[TermSymbol] = {
    val classOrder: Vector[Symbol] = fields.map{ _.owner }.distinct.reverse
    val grouped: Map[Symbol, Seq[TermSymbol]] = fields.groupBy{ _.owner }
    classOrder.flatMap{ grouped(_) }
  }
  
  /**
   * These come back "in the linearization order of their owners".  This means that if you have classes:
   *    class App extends AppBase {
   *      val foo: String = _
   *      val bar: String = _
   *    }
   *    
   *    class AppBase {
   *      val baz: String = _
   *      val asd: String = _
   *    }
   *    
   *  We end up with Vector(foo, bar, baz, asd)
   */
  def getFieldsForType(tpe: Type): Vector[TermSymbol] = {
    log(s"getFieldsForType($tpe)  => tpe.members.sorted: ${tpe.members.sorted}")
    
    // Not sure if this is totally correct.  I don't really see a way to filter the variable/value symbols from a tpe.declarations
    val res: Vector[TermSymbol] = tpe.members.sorted.filter{ d => d.isTerm && !d.isMethod }.map{ _.asTerm }.toVector
    
    dedupeInheritedTermSymbols(res)
  }
  
  def getPublicMethodForType(tpe: Type): Vector[MethodSymbol] = getMethodsForType(tpe).filter{ _.isPublic }
  
  def getMethodsForType(tpe: Type): Vector[MethodSymbol] = {
    tpe.members.sorted.filter{ _.isTerm }.flatMap{ _.asTerm.alternatives }.filter{ _.isMethod }.map{ _.asMethod }.toVector
  }
  
  // Same as getMethodsForType but filters by name
  def getMethodsForType(tpe: Type, name: TermName): Vector[MethodSymbol] = {
    val sym: Symbol = tpe.member(name)
    
    if (sym.isTerm) sym.asTerm.alternatives.filter{ _.isMethod }.map{ _.asMethod }.toVector
    else Vector.empty
  }
  
  def javaBeanGetters(tpe: Type): Vector[MethodSymbol] = {
    val all: Vector[MethodSymbol] = getPublicMethodForType(tpe).filter{ isNoArgsMethod }.filter{ m: MethodSymbol =>
      val name: String = m.name.decoded
      val isGetter: Boolean = name.startsWith("get") || ((m.returnType =:= typeOf[Boolean]  || m.returnType =:= typeOf[java.lang.Boolean]) && name.startsWith("is"))
      
      isGetter && name != "getClass" && name != "isInstanceOf"
    }
    
    dedupeInheritedMethodSymbols(all)
  }
  
  def javaBeanSetters(tpe: Type): Vector[MethodSymbol] = {
    val all: Vector[MethodSymbol] = getPublicMethodForType(tpe).filter{ isSingleArgMethod }.filter{ m: MethodSymbol =>
      val name: String = m.name.decoded
      name.startsWith("set")
    }
    
    // Since this dedupes based on the return type I don't think is actually what we want.
    // We will probably need to de-dupe based on the type of the single argument.  Since
    // all tests currently pass I'll punt this isuses until it actually becomes a problem.
    //dedupeInheritedMethodSymbols(all)
    
    all
  }
  
  private def dedupeInheritedTypesUsing[T <: TermSymbol](symbols: Vector[T])(getType: T => Type): Vector[T] = {
    
    // zipWithIndex to retain the ordering
    val grouped: Vector[Seq[(T,Int)]] = symbols.zipWithIndex.groupBy{ case (sym, idx) => sym.name.decoded }.values.toVector
    
    val res = grouped.map{ group: Seq[(T,Int)] =>
      // We want the most specific return type in each group.  This handles
      // the case of inheriting from an interface with a more specific return
      // type.
      group.sortWith{ case (a,b) => getType(a._1) <:< getType(b._1) }.head
    }.sortBy{ case (symbol, idx) =>
      // re-sort by the original index
      idx
    }.map{ case (symbol, idx) =>
      symbol
    }
    
    res
  }
  
  // Note: existing code used the TermSymbol.typeSignature so I'm sticking with that here
  private def dedupeInheritedTermSymbols(symbols: Vector[TermSymbol]): Vector[TermSymbol] = dedupeInheritedTypesUsing(symbols){ _.typeSignature }

  // Note: existing code used the MethodSymbol.returnType so I'm sticking with that here.
  private def dedupeInheritedMethodSymbols(symbols: Vector[MethodSymbol]): Vector[MethodSymbol] = dedupeInheritedTypesUsing(symbols){ _.returnType }
}