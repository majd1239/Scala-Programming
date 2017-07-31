/**
  * Created by Admin on 7/28/17.
  */
abstract class Circuits extends Gates {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {

    val d,e = new Wire

    OrGate(a, b, d)
    AndGate(a, b, c)
    inverter(c, e)
    AndGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, out: Wire)  : Unit =
  {
    val s,c1,c2 = new Wire

    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    OrGate(c1,c2,out)
  }

}
