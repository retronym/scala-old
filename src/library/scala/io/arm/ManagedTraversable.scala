/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io.arm

/**
 *  @version Adapted from scalax Aug/17/2009
 */

trait ManagedTraversable[+A] extends collection.Traversable[A] with ManagedHandle
{  
  val resource: ManagedResource[Handle]
  protected def iterator(v: Handle): Iterator[A]

  def foreach[U](f: A => U): Unit = resource flatMap iterator foreach f
}