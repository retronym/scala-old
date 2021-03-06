/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._
import annotation.tailrec

/** This extensible class may be used as a basis for implementing linked
 *  list. Type variable <code>A</code> refers to the element type of the
 *  list, type variable <code>This</code> is used to model self types of
 *  linked lists.
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait LinkedListLike[A, This <: Seq[A] with LinkedListLike[A, This]] extends SeqLike[A, This] { self =>
  
  var elem: A = _
  var next: This = _

  override def isEmpty = next eq this

  override def length: Int = if (isEmpty) 0 else next.length + 1

  override def head: A    = elem

  override def tail: This = {
    require(nonEmpty, "tail of empty list")
    next
  }

  /** Append linked list `that` at current position of this linked list
   *  @return the list after append (this is the list itself if nonempty,
   *  or list `that` if list this is empty. )
   */
  def append(that: This): This = {
    @tailrec
    def loop(x: This) {
      if (x.next.isEmpty) x.next = that
      else loop(x.next)
    }
    if (isEmpty) that
    else { loop(repr); repr }
  }

  /** Insert linked list `that` at current position of this linked list
   *  @pre this linked list is not empty
   */
  def insert(that: This): Unit = {
    require(nonEmpty, "insert into empty list")
    if (that.nonEmpty) {
      that.append(next)
      next = that
    }
  }

  override def drop(n: Int): This = {
    var i = 0
    var these: This = repr
    while (i < n && !these.isEmpty) {
      these = these.next.asInstanceOf[This] // !!! concrete overrides abstract problem
      i += 1
    }
    these
  }

  private def atLocation[T](n: Int)(f: This => T) = {
    val loc = drop(n)
    if (!loc.isEmpty) f(loc)
    else throw new IndexOutOfBoundsException(n.toString)
  }

  override def apply(n: Int): A   = atLocation(n)(_.elem)
  def update(n: Int, x: A): Unit  = atLocation(n)(_.elem = x)

  def get(n: Int): Option[A] = {
    val loc = drop(n)
    if (loc.nonEmpty) Some(loc.elem)
    else None
  }

  override def iterator: Iterator[A] = new Iterator[A] {
    var elems = self
    def hasNext = elems.nonEmpty
    def next = {
      val res = elems.elem
      elems = elems.next
      res
    }
  }

  override def foreach[B](f: A => B) {
    var these = this
    while (these.nonEmpty) {
      f(these.elem)
      these = these.next
    }
  }
}
