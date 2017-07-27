/**
  * Created by Majed Takieddine on 6/12/17.
  */
import scala.io.Source
class Translator {

  private val words=Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt"
  ).getLines().toList filter (word => word forall (chr => chr.isLetter))

  private val mnemonics= Map('2' -> "ABC",'3'->"DEF",'4'->"GHI",'5'->"JKL",'6'->"MNO",'7'->"PQRS",'8'->"TUV",'9'->"WXYZ")

  private val charCode: Map[Char,Char] = for((key,value) <- mnemonics; letter <-value) yield letter ->key

  def Word_To_Num(word:String) = word.toUpperCase map charCode

  private def Num_To_Words(number: String): Set[List[String]] = {

    def WordsforNum : Map[String,Seq[String]] = words groupBy Word_To_Num withDefaultValue Seq()

    if (number.isEmpty) Set(List())
    else {
      for {
        digit <- 1 to number.length
        word <- WordsforNum(number take digit)
        rest <- Num_To_Words(number drop digit)
      } yield word :: rest
    }.toSet
  }

  def translate(number: String) = Num_To_Words(number) map(_ mkString " " )
}

object Main {
  def main(args: Array[String]): Unit = {

    var trans = new Translator()

    println (trans.translate("7225247386"))

  }
}
