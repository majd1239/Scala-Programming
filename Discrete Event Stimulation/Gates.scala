/**
  * Created by Admin on 7/28/17.
  */
abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  def inverter(input: Wire, output: Wire) =
  {
    def invertAction(): Unit = {

      val inputSig = input.getSignal

      afterDelay(InverterDelay) { output.setSignal(!inputSig)}
    }

    input addAction(invertAction)
  }

  def OrGate(w1: Wire, w2: Wire, output: Wire) = {

    def OrAction() = {

      val in1Sig = w1.getSignal
      val in2Sig = w2.getSignal

      afterDelay(OrGateDelay) { output.setSignal(in1Sig | in2Sig)}
    }

    w1 addAction(OrAction)
    w2 addAction(OrAction)
  }

  def AndGate(w1: Wire, w2: Wire, output: Wire) = {

    def andAction() = {

      val in1Sig = w1.getSignal
      val in2Sig = w2.getSignal

      afterDelay(AndGateDelay) { output.setSignal(in1Sig & in2Sig)}
    }

    w1 addAction(andAction)
    w2 addAction(andAction)
  }

  def probe(name: String, wire: Wire): Unit =
  {
    def probeAction(): Unit = println(s"$name $currentTime new-value =  ${wire.getSignal}")

    wire.addAction(probeAction)
  }


  class Wire {

    private var sigVal = false

    private var actions: List[Action] = List()

    def getSignal: Boolean = sigVal

    def setSignal(s: Boolean) : Unit =
    {
      if( s != sigVal )
      {
        sigVal = s
        actions foreach( action  =>  action() )
      }
    }

    def addAction(action : Action) : Unit =
    {
      actions = action :: actions
      action ()
    }

  }

}
