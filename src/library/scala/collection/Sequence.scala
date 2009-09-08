/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Sequence.scala 16092 2008-09-12 10:37:06Z nielsen $


package scala.collection

import mutable.ListBuffer
import generic._

/** Class <code>Sequence[A]</code> represents sequences of elements
 *  of type <code>A</code>.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait Sequence[+A] extends PartialFunction[Int, A] 
                      with Iterable[A] 
                      with TraversableClass[A, Sequence]
                      with SequenceTemplate[A, Sequence[A]] {
  override def companion: Companion[Sequence] = Sequence
}

object Sequence extends SequenceFactory[Sequence]
{
  implicit def builderFactory[A]: BuilderFactory[A, Sequence[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Sequence[A]] = immutable.Sequence.newBuilder[A]  
}

