/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

/** Utility methods for dealing with java reflection.  For the most
 *  part these are elaborate wrappers around the laborious java API.
 * 
 *  @author Paul Phillips
 *  @since 2.8
 */

package scala.reflect

import scala.util.NameTransformer.{ decode, encode }   // e.g. $plus$plus => ++
import scala.util.matching.Regex
import scala.util.control.Exception.{ catching, ignoring }
import scala.annotation.experimental
import java.lang.{ reflect => r }
import java.lang.{ Class => JClass }

object Utility
{
  trait ProperClassObject {
    def anyval2class[T <: AnyVal](x: T) = anyref2class(x.asInstanceOf[AnyRef])
    def anyref2class[T <: AnyRef](x: T) = x.getClass.asInstanceOf[JClass[T]]
  }
  
  object FromAny extends ProperClassObject {
    implicit def anyval2ReflectionWrapper[T <: AnyVal](x: T) = anyRefWrapper(x.asInstanceOf[AnyRef])
    implicit def anyref2ReflectionWrapper[T <: AnyRef](x: T) = anyRefWrapper[T](x)
  }
  object FromAnyRef extends ProperClassObject {
    implicit def anyRef2ReflectionWrapper[T <: AnyRef](x: T) = anyRefWrapper[T](x)
  }
  object FromClass {
    implicit def class2ReflectionWrapper[T](x: JClass[T]) = new SClass[T](x)
  }
  
  implicit def anyRefWrapper[T <: AnyRef](x: T): SClass[T] = new SClass(FromAnyRef.anyref2class(x))
    
  // implicit def classWrapper(x: JClass[_]): SClass[_] = new SClass(x)
  implicit def memberWrapper(member: r.Member): Member = member match {
    case x: r.Method              => new Method(x)
    case x: r.Constructor[_]      => new Constructor(x)
    case x: r.Field               => new Field(x)
  }  
  implicit def typeWrapper(rtype: r.Type): Type = rtype match {
    case x: r.ParameterizedType   => new ParameterizedType(x)
    case x: r.GenericArrayType    => new GenericArrayType(x)
    case x: r.WildcardType        => new WildcardType(x)
    case x: r.TypeVariable[_]     => new TypeVariable(x)
    case x: JClass[_]             => new SClass(x)
  }
  implicit def typesWrapper(types: Seq[r.Type]): Seq[Type] = types map typeWrapper
  
  final val sigRegex    = new Regex("""^([^(]+)\((.*?)\)$""", "method", "arglist")
  final val primitives  = List("boolean", "byte", "short", "int", "long", "float", "double", "char")
  
  def isScalaFunction(rt: r.Type): Boolean = rt match {
    case x: r.ParameterizedType => isScalaFunction(x.getRawType)
    case x: JClass[_]           => x.getCanonicalName startsWith "scala.Function"
    case _                      => false
  }
  
  def typeToString(rt: r.Type): String = rt.toScalaString
  
  def javaTermToScala(s: String): String = s match {
    case "void"                                 => "Unit"
    case _ if primitives contains s             => s.capitalize
    case _ if primitives contains s.capitalize  => "java.lang." + s.capitalize
    case _                                      => s
  }

  // Imbuing the java reflection hierarchy with a little more polymorphism
  sealed abstract class Reflected {
    def getName(): String
    // def getType: String
    
    def owner: Option[JClass[_]] = None
    def pkg: String = owner.map(_.getPackage.getName + ".") getOrElse ""
    override def toString() = toScalaString
    
    def dropPrefix(s: String, pre: String): String =
      if (s startsWith pre) s drop pre.length else s

    // string generation goes through here so we can output unqualified
    // names for types in the same package as the type being inspected
    final def toScalaString: String = {
      println("toScalaString: getName = " + getName)
      decode(getName)  
    }
    // final def typeToString(x: r.Type) = toScalaString
      // x.pkgRelativeString(pkg)
    final def typesToString(xs: List[r.Type]) = xs map typeToString mkString ", "
    final def typesToParamString(xs: List[r.Type]) =
      (xs map typeToString).zipWithIndex .
        map { case (t, i) => "p%d: %s".format(i + 1, t) } mkString ", "
    
    // final def pkgRelativeString(thePkg: String) = {
    //   val str = toScalaString
    //   val s = str.replaceAll("""(?!\.)""" + thePkg, "")
    //   
    //   if (s != str) s
    //   else if ((s startsWith "scala.") && s.lastIndexOf('.') == 5) s.substring("scala.".length)
    //   else dropPrefix(s, "java.lang.")
    // }
  }
  
  object Modifiers {
    import java.lang.reflect.{ Modifier => M }
    sealed abstract class Modifier(val name: String, val f: Int => Boolean) {
      def is(x: Modifiable) = f(x.getModifiers)
      def apply(x: Modifiable) = is(x)
    }
  
    case object Abstract extends Modifier("abstract", M isAbstract _)
    case object Final extends Modifier("final", M isFinal _)
    case object Private extends Modifier("private", M isPrivate _)
    case object Protected extends Modifier("protected", M isProtected _)
    case object Static extends Modifier("static", M isStatic _)
    case object Synchronized extends Modifier("synchronized", M isSynchronized _)
    case object Volatile extends Modifier("volatile", M isVolatile _)  
  }
  import Modifiers._
  val allModifiers = List(Abstract, Final, Private, Protected, Static, Synchronized, Volatile)
  
  /** Entities which can have modifiers. */
  trait Modifiable extends Reflected { 
    def getModifiers(): Int
    def getModifierString = allModifiers filter (_ is this) map (_.name) mkString " "
  }
  
  /** Entities which can accept type arguments - distinct from an entity
   *  which is the result of being parameterized, i.e. a ParameterizedType
   *  is not Parameterizable but Class, Constructor, and Method are.
   */
  trait Parameterizable extends Reflected {
    def paramTypes: List[r.Type]
    def tparams = typesToString(paramTypes)
    def paramString = if (paramTypes.isEmpty) "" else "[?]"   // "[%s]" format tparams

    def isScalaFunction = false
  }
  
  // From e.g. Function2[B,A,B]  to  (B, A) => B
  // XXX by name params
  trait PolyFunction extends Parameterizable {
    require(paramTypes.length > 0)
    
    val (args, List(res)) = paramTypes splitAt (paramTypes.length - 1)
    override def tparams = typesToString(args)
    
    override def isScalaFunction = true
    def functionString = "(%s) => %s".format(tparams, typeToString(res))
  }
  
  /*
   * Base Class:  Member
   * Traits:      Modifiable, Parameterizable
   * Subclasses:  Method, Constructor, Field
   */
    
  abstract class Member(ref: r.Member) extends Reflected with Modifiable  {
    val  descriptor: String
    def     retType: Option[r.Type]
    def formalTypes: List[r.Type]
    def    excTypes: List[r.Type]
    def paramString: String
    
    def getModifiers() = ref.getModifiers
    def getIdentifier = decode(ref.getName)
    override def owner = Some(ref.getDeclaringClass)
    def argsString: String = "(" + typesToParamString(formalTypes) + ")"
    def retString: String = retType.map(x => ": " + typeToString(x)) getOrElse ""
    def isSynthetic = ref.isSynthetic
    def getName = /* getModifierString + */ descriptor + " " + getIdentifier + paramString + argsString + retString
  }
  
  class Method(val ref: r.Method) extends Member(ref) with Parameterizable {
    val descriptor  = "def"
    def paramTypes  = ref.getTypeParameters.toList
    def retType     = Some(ref.getGenericReturnType)
    def formalTypes = ref.getGenericParameterTypes.toList
    def excTypes    = ref.getGenericExceptionTypes.toList
    
    def isStructurallyEqual(m: Method) = {
      val other = m.ref
      ref.getName == other.getName &&
      ref.getReturnType == other.getReturnType &&
      ref.getParameterTypes.toList == other.getParameterTypes.toList
    }
  }
  
  class Constructor[T](val ref: r.Constructor[T]) extends Member(ref) with Parameterizable {
    val descriptor  = "def"
    def paramTypes  = owner.get.getTypeParameters.toList
    def retType     = None
    def formalTypes = ref.getGenericParameterTypes.toList
    def excTypes    = ref.getGenericExceptionTypes.toList
    override def getIdentifier = "this"
  }
  
  class Field(val ref: r.Field) extends Member(ref) {
    val descriptor  = if (Final(this)) "val" else "var"
    def retType     = Some(ref.getGenericType)
    def formalTypes = Nil
    def excTypes    = Nil
    def paramString = ""
    
    override def argsString: String = ""
  }
  
  /*
   * Base Class:  Type
   * Traits:      Bounded, Parameterizable
   * Subclasses:  WildcardType, TypeVariable, GenericArrayType, ParameterizedType, SClass
   */
  
  sealed abstract class Type(ref: r.Type) extends Reflected {
    def getRawName = getName
    def isJavaLangObject = false
  }
  
  trait Bounded extends Type {
    def lower: Array[r.Type]
    def upper: Array[r.Type]
    def hasDefaultBounds = upper.length == 1 && upper(0).isJavaLangObject
    def hasNoLowerBounds = lower == null || lower.length == 0
    
    def boundsToString(ts: Array[r.Type], isUpper: Boolean): String = {
      if (ts == null || ts.length == 0) return ""
      if (isUpper && ts.length == 1 && ts(0).isJavaLangObject) return ""
      
      val boundsStr = if (isUpper) " <: " else " >: "
      val bounds: List[r.Type] = ts.toList
      
      if (bounds.isEmpty) "" else boundsStr + typesToString(bounds)
    }
  }
  
  
  class WildcardType(ref: r.WildcardType) extends Type(ref) with Bounded {
    override def getRawName = "_"
    def lower = ref.getLowerBounds
    def upper = ref.getUpperBounds
    def getName = (hasNoLowerBounds, hasDefaultBounds) match {
      case (true, true)     => "_"
      case (true, false)    => typeToString(upper(0))
      case (false, true)    => boundsToString(lower, false)
      case (false, false)   => boundsToString(upper, true) + ", " + boundsToString(lower, false)
    }
  }
  
  class TypeVariable(ref: r.TypeVariable[_]) extends Type(ref) with Bounded {
    def lower: Array[r.Type] = Array()
    def upper = ref.getBounds
    def getName = ref.getName + boundsToString(upper, true)
  }
  
  class GenericArrayType(ref: r.GenericArrayType) extends Type(ref) {
    def getName = "Array[" + typeToString(ref.getGenericComponentType) + "]"
  }
    
  class ParameterizedType(ref: r.ParameterizedType) extends Type(ref) with Parameterizable {
    def paramTypes: List[r.Type] = ref.getActualTypeArguments.toList
    // override def isScalaFunction = ref.getRawType.isScalaFunction
    override def getRawName = ref.getRawType.getRawName
    override def getName = getRawName + paramString
      // if (isScalaFunction) functionString
      // else getRawName + paramString
  }
  
  class SClass[T](val ref: JClass[T]) extends Type(ref) with Modifiable with Parameterizable {
    def getModifiers  = ref.getModifiers
    def paramTypes    = ref.getTypeParameters.toList    
    def methods       = ref.getDeclaredMethods.toList map { m => new Method(m) }
    def constructors  = ref.getDeclaredConstructors.toList map { c => new Constructor(c.asInstanceOf[r.Constructor[T]]) }
    def fields        = ref.getDeclaredFields.toList map { f => new Field(f) }
    def classes       = ref.getDeclaredClasses.toList map { c => new SClass(c) }
    def interfaces    = ref.getInterfaces.toList map { new SClass(_) }
    // getGenericInterfaces
    def declarations: List[Member] = (methods ::: fields) filterNot (x => Private(x)) //  (!_.isPrivate)
    
    def selfAndInterfaces = this :: interfaces
    
    override def pkg  = ref.getPackage.getName
    override def isJavaLangObject = ref == classOf[AnyRef]
    override def owner = if (ref.getDeclaringClass == null) Some(ref) else Some(ref.getDeclaringClass)
    
    // method signatures for all methods matching name (i.e. overloaded set)
    def signatures(name: String)  = methods.filter(_.getIdentifier == name).map(_.toScalaString)      
    def constructorSignatures     = constructors.map(_.toScalaString)   
    
    override def getName = getRawName + paramString
    override def getRawName = mkScalaString(ref.getName)
      // if (ref.getSimpleName endsWith "[]") mkArrayString(ref.getSimpleName)
      // else mkScalaString(ref.getName)
      
    private def mkArrayString(s: String): String =
      if (s endsWith "[]") "Array[" + mkArrayString(s.substring(0, s.length - 2)) + "]"
      else mkScalaString(s)
    
    private def mkScalaString(s: String): String =
      if (isJavaLangObject) "AnyRef" else javaTermToScala(s)
  }

  private object ObjectSizer
  {
    val SAMPLE_SIZE = 10000
    val SLEEP_INTERVAL = 100

    def sizeOf[T](clazz: JClass[T], size: Int = SAMPLE_SIZE): Option[Int] = {  
      val constructor = 
        catching(classOf[NoSuchMethodException]).opt(clazz.getConstructor())
        . map (_.asInstanceOf[r.Constructor[T]])  
        . getOrElse (return None)

      catching(classOf[Exception]) opt {
        val arr = new Array[AnyRef](size)
        var i = 0
        val start = getMemoryUse
        while (i < arr.size) {
          arr(i) = constructor.newInstance().asInstanceOf[AnyRef]
          i += 1
        }

        val end = getMemoryUse

        ((end - start) / size).toInt
      }
    }

    private def allMem = Runtime.getRuntime().totalMemory()
    private def freeMem = Runtime.getRuntime().freeMemory()

    private def getMemoryUse(): Long = {
      putOutTheGarbage()
      val totalMemory = allMem
      putOutTheGarbage()    
      (totalMemory - freeMem)
    }
    private def putOutTheGarbage() { collectGarbage() ; collectGarbage() }
    private def collectGarbage() {
      ignoring(classOf[InterruptedException]) {
        System.gc();
        Thread sleep SLEEP_INTERVAL
        System.runFinalization()
        Thread sleep SLEEP_INTERVAL
      }
    }
  }
  
  @experimental
  def sizeOf[T](clazz: JClass[T], size: Int = ObjectSizer.SAMPLE_SIZE): Option[Int] =
    ObjectSizer.sizeOf(clazz, size)
}
