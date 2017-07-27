package forcomp


object Anagrams {

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = (w.toLowerCase groupBy identity map{case (x,s) =>(x,s.length)} toList) sorted

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(x => wordOccurrences(x))

  def wordAnagrams(word: Word): List[Word] =  dictionaryByOccurrences(wordOccurrences(word))

  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(List())
    case head :: tail => {
      for (o <- combinations(tail); x <- (0 to head._2))
        yield if(x==0) o else (head._1, x) :: o
    }
 
  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    x.map(a => y.find(_._1 == a._1) match {
      case Some(b) => (a._1, a._2 - b._2)
      case None => a
    }).filter(_._2 != 0)

  def sentenceAnagrams(sentence: Sentence): List[Sentence] =
  {
    def sentenceAnagramsInner(o: Occurrences): List[Sentence] =
      if (o.isEmpty) List(Nil)
      else
      {
        for (i <- combinations(o) if dictionaryByOccurrences.keySet(i);
             j <- dictionaryByOccurrences(i);
             s <- sentenceAnagramsInner(subtract(o, i))) yield {j :: s}
      }

    sentenceAnagramsInner(sentenceOccurrences(sentence))
  }
}
