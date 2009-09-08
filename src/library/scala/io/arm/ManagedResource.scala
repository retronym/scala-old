/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io.arm

trait ManagedHandle
{
  /** The type of resource being managed. */
  type Handle
} 

/** Provides automatic resource management, equivalent to C#'s using, or C++
 * RAII/RRID. Idiomatic usage would be as follows:
 *
 * <pre>
 * def fileReader(f : String) = ManagedResource(new FileReader(f))
 *
 * // Print the first character of a file
 * for(in &lt;- fileReader("/etc/passwd"))
 *     println(in.read().toChar)
 * </pre>
 *
 * @version Adapted from scalax Aug/17/2009
 */

abstract trait ManagedResourceImpl[+A] extends ManagedHandle
{
  /** Implement to acquire the managed resource.  Clients should not
   *  call this directly, as the returned resource will need to be
   *  disposed of manually.
   */
  protected def unsafeOpen(): Handle
  
  /** Implement to dispose of the managed resource.  Called automatically
   *  when the ManagedResource is used in a for comprehension.
   */
  protected def unsafeClose(v: Handle): Unit
   
  /** Implement to translate a Handle into the desired resource type. */
  protected def translate(v: Handle): A
  
  /** How to handle failure. */
  // protected def onFailure: PartialFunction[Throwable, () => Unit] =
  //   { case e: Exception => e.printStackTrace() }
} 
  
trait ManagedResource[+A] extends ManagedResourceImpl[A]
{
  /** Close ignoring all Throwables. */
  protected def unsafeCloseQuietly(v: Handle): Unit =
    try unsafeClose(v)
    catch { case e => e.printStackTrace() }
  
  /** Close ignoring only Exceptions. */
  protected def unsafeCloseIgnoringException(v : Handle): Unit =
    try unsafeClose(v)
    catch { case e: Exception => e.printStackTrace() }

  def foreach[T](f: A => T): Unit = acquireFor(f)
  def flatMap[B](f: A => B): B    = acquireFor(f)
  // def map[B](f: A => B): ManagedResource[B]

  /** Acquires the resource for the duration of the supplied function.
   *  If an exception is encountered closing the resource, it will try
   *  to close it again ignoring all exceptions, then propagate the
   *  original.
   */
  def acquireFor[B](f : A => B): B = {
    val v = unsafeOpen()
    var closeOK = false   // to make sure we don't try to close twice
    
    try {
      val r = f(translate(v))
      unsafeClose(v)
      closeOK = true
      r
    }
    finally if (!closeOK) unsafeCloseQuietly(v)
  }
}

/** The typical case of a ManagedResource, where the handle and resource
 *  are the same object.
 */
trait UntranslatedManagedResource[A] extends ManagedResource[A]
{
  type Handle = A
  final def translate(v: Handle) = v
}

object ManagedResource
{
  type Closeable = { def close(): Unit }
  
  /** Creates a ManagedResource for any type with a close method. Note that
   *  the opener argument is evaluated on demand, possibly more than once, so
   *  it must contain the code that actually acquires the resource. Clients
   *  are encouraged to write specialized methods to instantiate
   *  ManagedResources rather than relying on ad-hoc usage of this method.
   */
  def apply[A <: Closeable](opener: => A) =
    new UntranslatedManagedResource[A] {
      def unsafeOpen()      = opener
      def unsafeClose(r: A) = r.close()
    }
}
