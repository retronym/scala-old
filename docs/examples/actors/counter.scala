
import scala.actors.multi.Pid
import actors.distributed.RemoteActor
import actors.distributed.TCP
import actors.distributed.TcpNode
import actors.distributed.TcpService

abstract class CounterMessage
case class Incr() extends CounterMessage
case class Value(p: Pid) extends CounterMessage
case class Result(v: int) extends CounterMessage

class Counter extends RemoteActor {
  override def run(): unit =
    loop(0)

  def loop(value: int): unit = {
    Console.println("Value: " + value)
    receive {
      case Incr() =>
        loop(value + 1)
      case Value(p) =>
        p ! Result(value)
        loop(value)
      case other =>
        loop(value)
    }
  }
}

class CounterUser extends RemoteActor {
  override def run(): unit = {
    alive(TCP())

    spawn(TcpNode("127.0.0.1", 9090), "Counter")

    receive {
      case p: Pid =>
        // communicate with counter
        Console.println("" + node + ": Sending Incr() to remote Counter (" + p + ")...")
        p ! Incr()
        p ! Incr()
        p ! Value(self)
        receive {
          case Result(v) =>
            Console.println("Received result: " + v)
        }
    }
  }
}

object CounterTest {
  def main(args: Array[String]): unit = {
    val serv = new TcpService(9090)
    serv.start()

    val cu = new CounterUser
    cu.start()
  }
}
