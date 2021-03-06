/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package symtab

object Flags {

  // modifiers
  final val IMPLICIT      = 0x00000200   
  final val FINAL         = 0x00000020
  final val PRIVATE       = 0x00000004
  final val PROTECTED     = 0x00000001

  final val SEALED        = 0x00000400
  final val OVERRIDE      = 0x00000002
  final val CASE          = 0x00000800
  final val ABSTRACT      = 0x00000008    // abstract class, or used in conjunction
                                          // with abstract override.
                                          // Note difference to DEFERRED!

  final val DEFERRED      = 0x00000010    // was `abstract' for members | trait is virtual
  final val METHOD        = 0x00000040    // a method
  final val MODULE        = 0x00000100    // symbol is module or class implementing a module
  final val INTERFACE     = 0x00000080    // symbol is an interface (i.e. a trait which defines only abstract methods)


  final val MUTABLE       = 0x00001000    // symbol is a mutable variable.
  final val PARAM         = 0x00002000    // symbol is a (value or type) parameter to a method
  final val PACKAGE       = 0x00004000    // symbol is a java package
  // available: 0x00008000

  final val COVARIANT     = 0x00010000    // symbol is a covariant type variable
  final val CAPTURED      = 0x00010000    // variable is accessed from nested function.
                                          // Set by LambdaLift
  final val BYNAMEPARAM   = 0x00010000    // parameter is by name
  final val CONTRAVARIANT = 0x00020000    // symbol is a contravariant type variable
  final val LABEL         = 0x00020000    // method symbol is a label. Set by TailCall
  final val INCONSTRUCTOR = 0x00020000    // class symbol is defined in this/superclass
                                          // constructor.
  final val ABSOVERRIDE   = 0x00040000    // combination of abstract & override
  final val LOCAL         = 0x00080000    // symbol is local to current class (i.e. private[this] or protected[this]
                                          // pre: PRIVATE or PROTECTED are also set
  final val JAVA          = 0x00100000    // symbol was defined by a Java class
  final val SYNTHETIC     = 0x00200000    // symbol is compiler-generated
  final val STABLE        = 0x00400000    // functions that are assumed to be stable
                                          // (typically, access methods for valdefs)
                                          // or classes that do not contain abstract types.
  final val STATIC        = 0x00800000    // static field, method or class

  final val CASEACCESSOR  = 0x01000000    // symbol is a case parameter (or its accessor)
  final val TRAIT         = 0x02000000    // symbol is a trait
  final val DEFAULTPARAM  = 0x02000000    // the parameter has a default value
  final val BRIDGE        = 0x04000000    // function is a bridge method. Set by Erasure
  final val ACCESSOR      = 0x08000000    // a value or variable accessor (getter or setter)

  final val SUPERACCESSOR = 0x10000000    // a super accessor
  final val PARAMACCESSOR = 0x20000000    // for value definitions: is an access method
                                          // for a final val parameter
                                          // for parameters: is a val parameter
  final val MODULEVAR     = 0x40000000    // for variables: is the variable caching a module value
  final val SYNTHETICMETH = 0x40000000    // for methods: synthetic method, but without SYNTHETIC flag
  final val MONOMORPHIC   = 0x40000000    // for type symbols: does not have type parameters
  final val LAZY          = 0x80000000L   // symbol is a lazy val. can't have MUTABLE unless transformed by typer

  final val IS_ERROR      = 0x100000000L  // symbol is an error symbol
  final val OVERLOADED    = 0x200000000L  // symbol is overloaded
  final val LIFTED        = 0x400000000L  // class has been lifted out to package level
                                          // local value has been lifted out to class level
                                          // todo: make LIFTED = latePRIVATE?
  final val MIXEDIN       = 0x800000000L  // term member has been mixed in
  final val EXISTENTIAL   = 0x800000000L  // type is an existential parameter or skolem

  final val EXPANDEDNAME  = 0x1000000000L // name has been expanded with class suffix
  final val IMPLCLASS     = 0x2000000000L // symbol is an implementation class
  final val PRESUPER      = 0x2000000000L // value is evaluated before super call
  final val TRANS_FLAG    = 0x4000000000L // transient flag guaranteed to be reset
                                          // after each phase.

  final val LOCKED        = 0x8000000000L // temporary flag to catch cyclic dependencies
  final val SPECIALIZED   = 0x10000000000L// symbol is a generated specialized member
  final val DEFAULTINIT   = 0x20000000000L// symbol is a generated specialized member
  final val VBRIDGE       = 0x40000000000L// symbol is a varargs bridge

  final val InitialFlags  = 0x0001FFFFFFFFFFFFL // flags that are enabled from phase 1.
  final val LateFlags     = 0x00FE000000000000L // flags that override flags in 0x1FC.
  final val AntiFlags     = 0x7F00000000000000L // flags that cancel flags in 0x07F
  final val LateShift     = 47L
  final val AntiShift     = 56L

  // late flags (set by a transformer phase)
  final val latePRIVATE   = (PRIVATE: Long) << LateShift
  final val lateABSTRACT  = (ABSTRACT: Long) << LateShift
  final val lateDEFERRED  = (DEFERRED: Long) << LateShift
  final val lateINTERFACE = (INTERFACE: Long) << LateShift
  final val lateMODULE    = (MODULE: Long) << LateShift
  final val lateFINAL     = (FINAL: Long) << LateShift
  final val lateMETHOD    = (METHOD: Long) << LateShift
  
  final val notFINAL      = (FINAL: Long) << AntiShift
  final val notPRIVATE    = (PRIVATE: Long) << AntiShift
  final val notDEFERRED   = (DEFERRED: Long) << AntiShift
  final val notPROTECTED  = (PROTECTED: Long) << AntiShift
  final val notABSTRACT   = (ABSTRACT: Long) << AntiShift
  final val notOVERRIDE   = (OVERRIDE: Long) << AntiShift
  final val notMETHOD     = (METHOD: Long) << AntiShift

  // The flags from 0x001 to 0x800 are different in the raw flags
  // and in the pickled format.

  private final val IMPLICIT_PKL   = 0x00000001   
  private final val FINAL_PKL      = 0x00000002
  private final val PRIVATE_PKL    = 0x00000004
  private final val PROTECTED_PKL  = 0x00000008

  private final val SEALED_PKL     = 0x00000010
  private final val OVERRIDE_PKL   = 0x00000020
  private final val CASE_PKL       = 0x00000040
  private final val ABSTRACT_PKL   = 0x00000080

  private final val DEFERRED_PKL   = 0x00000100
  private final val METHOD_PKL     = 0x00000200
  private final val MODULE_PKL     = 0x00000400
  private final val INTERFACE_PKL  = 0x00000800

  private final val PKL_MASK       = 0x00000FFF


  private val r2p = {
    def rawFlagsToPickledAux(flags:Int) = {
      var pflags=0
      if ((flags & IMPLICIT )!=0) pflags|=IMPLICIT_PKL
      if ((flags & FINAL    )!=0) pflags|=FINAL_PKL
      if ((flags & PRIVATE  )!=0) pflags|=PRIVATE_PKL
      if ((flags & PROTECTED)!=0) pflags|=PROTECTED_PKL
      if ((flags & SEALED   )!=0) pflags|=SEALED_PKL
      if ((flags & OVERRIDE )!=0) pflags|=OVERRIDE_PKL
      if ((flags & CASE     )!=0) pflags|=CASE_PKL
      if ((flags & ABSTRACT )!=0) pflags|=ABSTRACT_PKL
      if ((flags & DEFERRED )!=0) pflags|=DEFERRED_PKL
      if ((flags & METHOD   )!=0) pflags|=METHOD_PKL
      if ((flags & MODULE   )!=0) pflags|=MODULE_PKL
      if ((flags & INTERFACE)!=0) pflags|=INTERFACE_PKL
      pflags
    }
    val v=new Array[Int](PKL_MASK+1)
    var i=0
    while (i<=PKL_MASK) {
      v(i)=rawFlagsToPickledAux(i)
      i+=1
    }
    v
  }

  private val p2r = {
    def pickledToRawFlagsAux(pflags:Int) = {
      var flags=0
      if ((pflags & IMPLICIT_PKL )!=0) flags|=IMPLICIT
      if ((pflags & FINAL_PKL    )!=0) flags|=FINAL
      if ((pflags & PRIVATE_PKL  )!=0) flags|=PRIVATE
      if ((pflags & PROTECTED_PKL)!=0) flags|=PROTECTED
      if ((pflags & SEALED_PKL   )!=0) flags|=SEALED
      if ((pflags & OVERRIDE_PKL )!=0) flags|=OVERRIDE
      if ((pflags & CASE_PKL     )!=0) flags|=CASE
      if ((pflags & ABSTRACT_PKL )!=0) flags|=ABSTRACT
      if ((pflags & DEFERRED_PKL )!=0) flags|=DEFERRED
      if ((pflags & METHOD_PKL   )!=0) flags|=METHOD
      if ((pflags & MODULE_PKL   )!=0) flags|=MODULE
      if ((pflags & INTERFACE_PKL)!=0) flags|=INTERFACE
      flags
    }
    val v=new Array[Int](PKL_MASK+1)
    var i=0
    while (i<=PKL_MASK) {
      v(i)=pickledToRawFlagsAux(i)
      i+=1
    }
    v
  }

  def rawFlagsToPickled(flags:Long):Long =
    (flags & ~PKL_MASK) | r2p(flags.toInt & PKL_MASK)

  def pickledToRawFlags(pflags:Long):Long =
    (pflags & ~PKL_MASK) | p2r(pflags.toInt & PKL_MASK)

  // List of the raw flags, in pickled order
  private val pickledListOrder = {
    def findBit(m:Long):Int = {
      var mask=m
      var i=0
      while (i <= 62) {
	if ((mask&1) == 1L) return i
	mask >>= 1
	i += 1
      }
      throw new FatalError("Internal error: mask is zero")
    }
    val v=new Array[Long](63)
    v(findBit(IMPLICIT_PKL ))=IMPLICIT
    v(findBit(FINAL_PKL    ))=FINAL
    v(findBit(PRIVATE_PKL  ))=PRIVATE
    v(findBit(PROTECTED_PKL))=PROTECTED
    v(findBit(SEALED_PKL   ))=SEALED
    v(findBit(OVERRIDE_PKL ))=OVERRIDE
    v(findBit(CASE_PKL     ))=CASE
    v(findBit(ABSTRACT_PKL ))=ABSTRACT
    v(findBit(DEFERRED_PKL ))=DEFERRED
    v(findBit(METHOD_PKL   ))=METHOD
    v(findBit(MODULE_PKL   ))=MODULE
    v(findBit(INTERFACE_PKL))=INTERFACE
    var i=findBit(PKL_MASK+1)
    while (i <= 62) {
      v(i)=1L << i
      i += 1
    }
    v.toList
  }

  // masks
  /** This flags can be set when class or module symbol is first created. */
  final val TopLevelCreationFlags: Long =
    MODULE | PACKAGE | FINAL | JAVA

  /** These modifiers can be set explicitly in source programs. */
  final val ExplicitFlags: Long =
    PRIVATE | PROTECTED | ABSTRACT | FINAL | SEALED |
    OVERRIDE | CASE | IMPLICIT | ABSOVERRIDE | LAZY

  /** These modifiers appear in TreePrinter output. */
  final val PrintableFlags: Long =
    ExplicitFlags | LOCAL | SYNTHETIC | STABLE | CASEACCESSOR |
    ACCESSOR | SUPERACCESSOR | PARAMACCESSOR | BRIDGE | STATIC | VBRIDGE

  /** The two bridge flags */
  final val BRIDGES = BRIDGE | VBRIDGE

  final val FieldFlags: Long =
    MUTABLE | CASEACCESSOR | PARAMACCESSOR | STATIC | FINAL | PRESUPER | LAZY

  final val AccessFlags: Long   = PRIVATE | PROTECTED
  final val VARIANCES     = COVARIANT | CONTRAVARIANT
  final val ConstrFlags: Long   = JAVA
  final val PickledFlags: Long  = 0xFFFFFFFFL

  /** Module flags inherited by their module-class */
  final val ModuleToClassFlags: Long = AccessFlags | MODULE | PACKAGE | CASE | SYNTHETIC | JAVA

  private def listToString(ss: List[String]): String =
    ss.filter("" !=).mkString("", " ", "")

  def flagsToString(flags: Long): String =
    listToString(for (mask <- pickledListOrder) yield flagToString(flags & mask))

  def flagsToString(flags: Long, privateWithin: String): String = {
    var f = flags
    val pw =
      if (privateWithin == "") {
        if ((flags & (PRIVATE | LOCAL)) == (PRIVATE | LOCAL).toLong) {
          f = f & ~(PRIVATE | LOCAL)
          "private[this]"
        } else if ((flags & (PROTECTED | LOCAL)) == (PROTECTED | LOCAL).toLong) {
          f = f & ~(PROTECTED | LOCAL)
          "protected[this]"
        } else {
          ""
        }
      } else if ((f & PROTECTED) != 0L) {
        f = f & ~PROTECTED
        "protected[" + privateWithin + "]"
      } else {
        "private[" + privateWithin + "]"
      }
    listToString(List(flagsToString(f), pw))
  }

  private def flagToString(flag: Long): String = {
    if (flag == IS_ERROR) "<is-error>"
    else if (flag == OVERLOADED  ) "<overloaded>"
    else if (flag == LIFTED      ) "<lifted>"
    else if (flag == MIXEDIN     ) "<mixedin/existential>"
    else if (flag == EXPANDEDNAME) "<expandedname>"
    else if (flag == IMPLCLASS   ) "<presuper/implclass>"
    else if (flag == TRANS_FLAG  ) "<trans-flag>"
    else if (flag == LOCKED      ) "<locked>"
    else if (flag == LAZY        ) "lazy"
    else flag.toInt match {
      case IMPLICIT      => "implicit"
      case FINAL         => "final"
      case PRIVATE       => "private"
      case PROTECTED     => "protected"

      case SEALED        => "sealed"
      case OVERRIDE      => "override"
      case CASE          => "case"
      case ABSTRACT      => "abstract"

      case DEFERRED      => "<deferred>"
      case METHOD        => "<method>"
      case MODULE        => "<module>"
      case INTERFACE     => "<interface>"

      case MUTABLE       => "<mutable>"
      case PARAM         => "<param>"
      case PACKAGE       => "<package>"

      case COVARIANT     => "<covariant/captured/byname>"
      case CONTRAVARIANT => "<contravariant/label/inconstr/defaultinit>"
      case ABSOVERRIDE   => "abstract override"
      case LOCAL         => "<local>"

      case JAVA          => "<java>"
      case SYNTHETIC     => "<synthetic>"
      case STABLE        => "<stable>"
      case STATIC        => "<static>"

      case CASEACCESSOR  => "<caseaccessor>"
      case TRAIT         => "<trait>"
      case BRIDGE        => "<bridge>"
      case ACCESSOR      => "<accessor>"

      case SUPERACCESSOR => "<superaccessor>"
      case PARAMACCESSOR => "<paramaccessor>"
      case VBRIDGE        => "<...bridge>"

      case _ => ""
    }
  }

  class Flag(mods: Long) {
    def isPrivate   = (mods & PRIVATE  ) != 0L
    def isProtected = (mods & PROTECTED) != 0L
    def isVariable  = (mods &   MUTABLE) != 0L
    def isPublic    = !isPrivate && !isProtected
  }
}
