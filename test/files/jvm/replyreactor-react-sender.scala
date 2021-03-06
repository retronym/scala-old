import scala.actors.ReplyReactor
import scala.actors.Actor._

object Test {

  val NUM = 2000

  def main(args: Array[String]) {
    var b: ReplyReactor = null

    val a = new ReplyReactor {
      def act() {
        var i = 0
        loopWhile (i < NUM) {
          i += 1
          react {
            case 'hello if sender == this => b ! 'fail
            case 'hello if sender == b => // do nothing
          }
        } andThen {
          b ! 'ok
        }
      }
    }
    a.start()

    b = new ReplyReactor {
      def act() {
        for (_ <- 0 until NUM)
          a ! 'hello
        react {
          case 'fail => println("FAIL")
          case 'ok   => println("OK")
          case other => println(other)
        }
      }
    }
    b.start()
  }

}
