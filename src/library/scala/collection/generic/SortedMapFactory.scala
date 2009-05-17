package scala.collection.generic

/** A template for companion objects of mutable.Map and subclasses thereof.
 */
abstract class SortedMapFactory[CC[A, B] <: SortedMap[A, B] with SortedMapTemplate[A, B, CC[A, B]]] {

  type Coll = CC[_, _]

  def newBuilder[A, B](implicit ord: Ordering[A]): Builder[(A, B), CC[A, B]]

  def empty[A, B](implicit ord: Ordering[A]): CC[A, B]

  def apply[A, B](elems: (A, B)*)(implicit ord: Ordering[A]): CC[A, B] = (newBuilder[A, B](ord) ++= elems).result

  class SortedMapBuilderFactory[A, B](implicit ord: Ordering[A]) extends BuilderFactory[(A, B), CC[A, B], Coll] {
    def apply(from: Coll) = newBuilder[A, B](ord)
  }
}