def p(a: Any): Unit = println(a)

trait Params {
  val InverterDelay: Int = 2
  val OrGateDelay: Int = 5
  val AndGateDelay: Int = 3
}

class Wire extends Simulation {
  private var sigVal = false
  private var actions: List[Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean): Unit = {
    if(s != sigVal){
      sigVal = s
      actions foreach(_())
    }
  }

  def addAction(a: Action): Unit = {
    actions = a :: actions
    a()
  }
}

trait Simulation {
  type Action = () => Unit
  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()
  private var curTime = 0

  def currentTime: Int = curTime

  def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if first.time <= item.time =>
      first :: insert(rest, item)
    case _ =>
      item :: ag
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curTime = first.time
      first.action()
      loop()
    case Nil => ()
  }

  def run(): Unit = {
    afterDelay(0) {
      println(s"*** Simulation started at $currentTime ***")
    }
    loop()
  }
}

trait Gates extends Simulation {
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  def inverter(in: Wire, out: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = in.getSignal
      afterDelay(InverterDelay) { out setSignal !inputSig }
    }
    in addAction invertAction
  }

  def andGate(in1: Wire, in2: Wire, out: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) { out setSignal in1Sig & in2Sig }
    }
    in1 addAction orAction
    in2 addAction orAction
  }

  def orGate(in1: Wire, in2: Wire, out: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) { out setSignal in1Sig | in2Sig }
    }
    in1 addAction orAction
    in2 addAction orAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value=${wire.getSignal}")
    }
    wire addAction probeAction
  }
}

abstract class Circuits extends Gates {
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s, c1, c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }
}

p("START")
object sim extends Circuits with Params
import sim._
val in1, in2, sum, carry = new Wire
halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)

in1 setSignal true
run()
in2 setSignal true
run()
in1 setSignal false
run()

