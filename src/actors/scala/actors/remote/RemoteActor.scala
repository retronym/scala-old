/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors
package remote


/** <p>
 *    This object provides methods for creating, registering, and
 *    selecting remotely accessible actors.
 *  </p>
 *  <p>
 *    A remote actor is typically created like this:
 *  </p><pre>
 *  actor {
 *    alive(9010)
 *    register('myName, self)
 *
 *    // behavior
 *  }
 *  </pre>
 *  <p>
 *    It can be accessed by an actor running on a (possibly)
 *    different node by selecting it in the following way:
 *  </p><pre>
 *  actor {
 *    // ...
 *    <b>val</b> c = select(Node("127.0.0.1", 9010), 'myName)
 *    c ! msg
 *    // ...
 *  }
 *  </pre>
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
object RemoteActor {

  private val kernels = new scala.collection.mutable.HashMap[Actor, NetKernel]

  /* If set to <code>null</code> (default), the default class loader
   * of <code>java.io.ObjectInputStream</code> is used for deserializing
   * objects sent as messages.
   */
  private var cl: ClassLoader = null

  def classLoader: ClassLoader = cl
  def classLoader_=(x: ClassLoader) { cl = x }

  /**
   * Makes <code>self</code> remotely accessible on TCP port
   * <code>port</code>.
   */
  def alive(port: Int): Unit = synchronized {
    createKernelOnPort(port)
  }

  def createKernelOnPort(port: Int): NetKernel = {
    val serv = TcpService(port, cl)
    val kern = serv.kernel
    val s = Actor.self
    kernels += Pair(s, kern)

    s.onTerminate {
      Debug.info("alive actor "+s+" terminated")
      // remove mapping for `s`
      kernels -= s
      // terminate `kern` when it does
      // not appear as value any more
      if (!kernels.valuesIterator.contains(kern)) {
        Debug.info("terminating "+kern)
        // terminate NetKernel
        kern.terminate()
      }
    }

    kern
  }

  /**
   * Registers <code>a</code> under <code>name</code> on this
   * node.
   */
  def register(name: Symbol, a: Actor): Unit = synchronized {
    val kernel = kernels.get(Actor.self) match {
      case None =>
        val serv = TcpService(TcpService.generatePort, cl)
        kernels += Pair(Actor.self, serv.kernel)
        serv.kernel
      case Some(k) =>
        k
    }
    kernel.register(name, a)
  }

  private def selfKernel = kernels.get(Actor.self) match {
    case None =>
      // establish remotely accessible
      // return path (sender)
      createKernelOnPort(TcpService.generatePort)
    case Some(k) =>
      k
  }

  /**
   * Returns (a proxy for) the actor registered under
   * <code>name</code> on <code>node</code>.
   */
  def select(node: Node, sym: Symbol): AbstractActor = synchronized {
    selfKernel.getOrCreateProxy(node, sym)
  }

  def someKernel: NetKernel =
    kernels.valuesIterator.next
}


/**
 * This class represents a machine node on a TCP network.
 *
 * @param address the host name, or <code>null</code> for the loopback address.
 * @param port    the port number.
 *
 * @author Philipp Haller
 */
case class Node(address: String, port: Int)
