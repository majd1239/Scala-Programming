package patmat

import common._

object Huffman {

  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(char, w) => w
    case Fork(left, right, chars, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, weight) => char :: Nil

    case Fork(left, right, char, weight) => char
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    
    def timesAcc(chars: List[Char], map: Map[Char, Int]): Map[Char, Int] = {
      chars match 
      {
        case Nil => map
        case c :: cs => 
        {
          val count = map.getOrElse(c, 0) + 1
          timesAcc(cs, map ++ Map(c -> count))
        }
      }
    }

    timesAcc(chars, Map()).toList // Convert the Map to a list of pairs.
  }


  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.map((pair: (Char, Int)) => Leaf(pair._1, pair._2)).sortBy(_.weight)
  }

  def singleton(trees: List[CodeTree]): Boolean = !(trees == Nil || trees.tail != Nil)

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    
    case first :: second :: tail => makeCodeTree(first, second) :: tail.sortWith(weight(_) < weight(_))
    case _ => trees
    
  }

  def until(singleton: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (singleton(trees)) trees

    else until(singleton, combine)(combine(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] =
  {
    
    def decodeChar(subTree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = {
      subTree match {
        
        case Leaf(char, _) => (char,bits)  
        case Fork(left, right, _, _) if (!bits.isEmpty) => decodeChar(if(bits.head==1) right else left,bits.tail) 
        case _ => throw new Exception("Not enough bits") 
        
      }
    }
    def decodeAcc(bits: List[Bit], acc: List[Char]): List[Char] = {
      if (bits.isEmpty) acc.reverse 
      else {
        val charBits = decodeChar(tree, bits)  
        decodeAcc(charBits._2,charBits._1::acc)
      }
    }
    decodeAcc(bits, List())
  }
  
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode,secret)
  
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
  {

    def lookup(tree:  CodeTree)(c: Char): List[Bit] = tree match {
      
      case Leaf(_, _) => List()
      case Fork(left, right, _, _) if chars(left).contains(c) => 0 :: lookup(left)(c)
      case Fork(left, right, _, _) => 1 :: lookup(right)(c)
    }

    text.flatMap(lookup(tree))
  }
  
  type CodeTable = List[(Char, List[Bit])]


  def codeBits(table: CodeTable)(char: Char): List[Bit] =
  {
    table.find(_._1 == char).get._2
  }


  def convert(tree: CodeTree): CodeTable =  
  {
    chars(tree).map(char => (char -> encode(tree)(List(char))))
  }
  
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b
  
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] =
  {
    val table = convert(tree)
    text.flatMap(codeBits(table)(_))
  }
  
}
