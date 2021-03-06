/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** This class implements double linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 */
@serializable @SerialVersionUID(-8144992287952814767L)
class DoubleLinkedList[A]() extends LinearSeq[A]
                            with GenericTraversableTemplate[A, DoubleLinkedList]
                            with DoubleLinkedListLike[A, DoubleLinkedList[A]] {
  next = this

  def this(elem: A, next: DoubleLinkedList[A]) {
    this()
    if (next != null) {
      this.elem = elem
      this.next = next
    }
  }

  override def companion: GenericCompanion[DoubleLinkedList] = DoubleLinkedList
}

object DoubleLinkedList extends SeqFactory[DoubleLinkedList] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, DoubleLinkedList[A]] = new GenericCanBuildFrom[A] //new CanBuildFrom[Coll, A, DoubleLinkedList[A]] { : Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, DoubleLinkedList[A]] =
    new Builder[A, DoubleLinkedList[A]] {
      var current: DoubleLinkedList[A] = _
      def +=(elem: A): this.type = {
        val tmp = new DoubleLinkedList(elem, null)
        if (current != null)
          current.insert(tmp)
        else
          current = tmp
        this
      }
      def clear() { current = null }
      def result() = current
    }
}
