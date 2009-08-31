/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.net

import java.io.{ IOException }
import java.net.{ URL, MalformedURLException }
import java.net.{ InetAddress, Socket => JSocket }
import scala.util.control.Exception._

object Socket
{
  def apply(host: String, port: Int): Either[Throwable, Socket] =
    catching(classOf[IOException], classOf[SecurityException]) either new Socket(host, port)
}

class Socket(host: String, port: Int)
{
  
}