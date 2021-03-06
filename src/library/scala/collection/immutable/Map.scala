/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._

/**
 * @since 1
 */
trait Map[A, +B] extends Iterable[(A, B)] 
                    with scala.collection.Map[A, B] 
                    with MapLike[A, B, Map[A, B]] { self =>

  override def empty: Map[A, B] = Map.empty

  /** Add a key/value pair to this map. 
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new binding added to this map
   */
  override def updated [B1 >: B](key: A, value: B1): Map[A, B1]
  def + [B1 >: B](kv: (A, B1)): Map[A, B1]

  /** The same map with a given default function !!! todo: move to general maps? */
  def withDefault[B1 >: B](d: A => B1): Map[A, B1] = new Map.WithDefault[A, B1](this, d) 

  /** The same map with a given default value */
  def withDefaultValue[B1 >: B](d: B1): Map[A, B1] = new Map.WithDefault[A, B1](this, x => d)
}

/**
 * @since 1
 */
object Map extends ImmutableMapFactory[Map] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), Map[A, B]] = new MapCanBuildFrom[A, B]

  def empty[A, B]: Map[A, B] = new EmptyMap[A, B]

  class WithDefault[A, +B](underlying: Map[A, B], d: A => B) extends Map[A, B] {
    override def size = underlying.size
    def get(key: A) = underlying.get(key) orElse Some(default(key))
    def iterator = underlying.iterator
    override def empty = new WithDefault(underlying.empty, d)
    override def updated[B1 >: B](key: A, value: B1): WithDefault[A, B1] = new WithDefault[A, B1](underlying.updated[B1](key, value), d)
    def + [B1 >: B](kv: (A, B1)): WithDefault[A, B1] = updated(kv._1, kv._2)
    def - (key: A): WithDefault[A, B] = new WithDefault(underlying - key, d)
    override def default(key: A): B = d(key)
  }

  @serializable
  class EmptyMap[A, +B] extends Map[A, B] {
    override def size: Int = 0
    def get(key: A): Option[B] = None
    def iterator: Iterator[(A, B)] = Iterator.empty
    override def updated [B1 >: B] (key: A, value: B1): Map[A, B1] = new Map1(key, value)
    def + [B1 >: B](kv: (A, B1)): Map[A, B1] = updated(kv._1, kv._2)
    def - (key: A): Map[A, B] = this
  }
  
  @serializable
  class Map1[A, +B](key1: A, value1: B) extends Map[A, B] {
    override def size = 1
    def get(key: A): Option[B] = 
      if (key == key1) Some(value1) else None
    def iterator = Iterator((key1, value1))
    override def updated [B1 >: B] (key: A, value: B1): Map[A, B1] = 
      if (key == key1) new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    def + [B1 >: B](kv: (A, B1)): Map[A, B1] = updated(kv._1, kv._2)
    def - (key: A): Map[A, B] = 
      if (key == key1) empty else this
    override def foreach[U](f: ((A, B)) =>  U): Unit = {
      f((key1, value1))
    }
  }

  @serializable
  class Map2[A, +B](key1: A, value1: B, key2: A, value2: B) extends Map[A, B] {
    override def size = 2
    def get(key: A): Option[B] = 
      if (key == key1) Some(value1) 
      else if (key == key2) Some(value2)
      else None
    def iterator = Iterator((key1, value1), (key2, value2))
    override def updated [B1 >: B] (key: A, value: B1): Map[A, B1] = 
      if (key == key1) new Map2(key1, value, key2, value2)
      else if (key == key2) new Map2(key1, value1, key2, value)
      else new Map3(key1, value1, key2, value2, key, value)
    def + [B1 >: B](kv: (A, B1)): Map[A, B1] = updated(kv._1, kv._2)
    def - (key: A): Map[A, B] = 
      if (key == key1) new Map1(key2, value2)
      else if (key == key2) new Map1(key1, value1)
      else this
    override def foreach[U](f: ((A, B)) =>  U): Unit = {
      f((key1, value1)); f((key2, value2))
    }
  }

  @serializable
  class Map3[A, +B](key1: A, value1: B, key2: A, value2: B, key3: A, value3: B) extends Map[A, B] {
    override def size = 3
    def get(key: A): Option[B] = 
      if (key == key1) Some(value1) 
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else None
    def iterator = Iterator((key1, value1), (key2, value2), (key3, value3))
    override def updated [B1 >: B] (key: A, value: B1): Map[A, B1] = 
      if (key == key1)      new Map3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new Map3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new Map3(key1, value1, key2, value2, key3, value)
      else new Map4(key1, value1, key2, value2, key3, value3, key, value)
    def + [B1 >: B](kv: (A, B1)): Map[A, B1] = updated(kv._1, kv._2)
    def - (key: A): Map[A, B] = 
      if (key == key1)      new Map2(key2, value2, key3, value3)
      else if (key == key2) new Map2(key1, value1, key3, value3)
      else if (key == key3) new Map2(key1, value1, key2, value2)
      else this
    override def foreach[U](f: ((A, B)) =>  U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
  }
 
  @serializable
  class Map4[A, +B](key1: A, value1: B, key2: A, value2: B, key3: A, value3: B, key4: A, value4: B) extends Map[A, B] {
    override def size = 4
    def get(key: A): Option[B] = 
      if (key == key1) Some(value1) 
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else if (key == key4) Some(value4)
      else None
    def iterator = Iterator((key1, value1), (key2, value2), (key3, value3), (key4, value4))
    override def updated [B1 >: B] (key: A, value: B1): Map[A, B1] = 
      if (key == key1)      new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else new HashMap + ((key1, value1), (key2, value2), (key3, value3), (key4, value4), (key, value))
    def + [B1 >: B](kv: (A, B1)): Map[A, B1] = updated(kv._1, kv._2)
    def - (key: A): Map[A, B] = 
      if (key == key1)      new Map3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new Map3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new Map3(key1, value1, key2, value2, key3, value3)
      else this
    override def foreach[U](f: ((A, B)) =>  U): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
  }
}

