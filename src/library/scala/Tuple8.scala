
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// generated by genprod on Tue Apr 22 16:48:01 CEST 2008  

package scala

/** Tuple8 is the canonical representation of a @see Product8 
 *  
 */
case class Tuple8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8](_1:T1, _2:T2, _3:T3, _4:T4, _5:T5, _6:T6, _7:T7, _8:T8) 
  extends Product8[T1, T2, T3, T4, T5, T6, T7, T8]  {

   override def toString() = {
     val sb = new StringBuilder
     sb.append('(').append(_1).append(',').append(_2).append(',').append(_3).append(',').append(_4).append(',').append(_5).append(',').append(_6).append(',').append(_7).append(',').append(_8).append(')')
     sb.toString
   }
  
}
