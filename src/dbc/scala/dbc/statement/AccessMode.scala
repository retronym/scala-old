/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package statement


abstract class AccessMode {
  def sqlString: String
}

object AccessMode {
  case object ReadOnly extends AccessMode {
    def sqlString = "READ ONLY"
  }
  case object ReadWrite extends AccessMode {
    def sqlString = "READ WRITE"
  }
}
