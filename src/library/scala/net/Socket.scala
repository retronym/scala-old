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

/** A very skeletal only-as-much-as-I-need Socket wrapper.
 *
 *  @author    Paul Phillips
 */
object Socket
{
  private val socketExceptions = List(classOf[IOException], classOf[SecurityException])
  
  class SocketBox(f: () => Socket) {
    def either: Either[Throwable, Socket] = catching(socketExceptions: _*) either f()
    def opt: Option[Socket] = catching(socketExceptions: _*) opt f()
  } 
  
  def apply(host: InetAddress, port: Int) = new SocketBox(() => new Socket(new JSocket(host, port)))
  def apply(host: String, port: Int) = new SocketBox(() => new Socket(new JSocket(host, port)))
}

class Socket(jsocket: JSocket) {
  def getOutputStream() = jsocket.getOutputStream()
  def getInputStream() = jsocket.getInputStream()
  def getPort() = jsocket.getPort()
  def close() = jsocket.close()
}