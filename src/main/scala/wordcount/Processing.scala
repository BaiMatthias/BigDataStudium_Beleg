package wordcount
import common._
import mapreduce.BasicOperations

class Processing {

  /**********************************************************************************************
   *
   *                          Aufgabe 1
   *
   *********************************************************************************************
  */
  def getWords(line:String):List[String]={
    /*
     * Extracts all words in a line
     * 
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */

    line.replaceAll("[^a-zA-Z]", " ") // alle Buchstaben (gross und klein raus filtern
      .toLowerCase // toLowerCase --> alle Buchstaben in kleine
      .split(" ") // jedes Wort in eine Liste (nach Leerzeichen getrennt)
      .filter(p => p != "") // Alles raus filtern was nicht leer ist (so werden die doppelten Leerzeichen an den Stellen mit Sonderzeichen gefiltert)
      .toList // zur Rueckgabe in eine String-Liste
  }

  def getAllWords(l:List[(Int,String)]):List[String]={

    /*
     * Extracts all Words from a List containing tupels consisting
     * of a line number and a string
     * The Words should be in the same order as they occur in the source document
     * 
     * Hint: Use the flatMap function
     */

    l.flatMap( // das Ergebnis in die flatMap funktion aus der eine Strin-Liste hervorgeht
      x => getWords // String an getWords uebergeben (Sonderzeichen werden raus gefiltert)
      (x._2) // aus der Liste l [Int,String] den String(2)
    )
  }

  def countTheWords(l:List[String]):List[(String,Int)]={

    /*
     *  Gets a list of words and counts the occurences of the individual words
     */

    l.foldLeft( //links falten
      Map.empty[String, Int]){ //leere Map erstellen
      (count, word) => count + // Map fuellen und die Count Zahl erhohen
        (word -> (count.getOrElse(word, 0) + 1))} // get word +0 wenn Wort nicht vorhanden ansonsten +1 danach +1 da Wort ja einmal min. da
      .toList //map in liste [String, Int] umwandeln
  }

  /**********************************************************************************************
   *
   *                          Aufgabe 2
   *
   *********************************************************************************************
  */

  def mapReduce[S,B,R](mapFun:(S=>B),
      redFun:(R,B)=>R,
      base:R,
      l:List[S]):R = l.map(mapFun).foldLeft(base)(redFun)

  def countTheWordsMR(l:List[String]):List[(String,Int)]= {
     Map[String, Int]().empty.toList
  }


  /**********************************************************************************************
   *
   *                          Aufgabe 3
   *
   *********************************************************************************************
  */

    def getAllWordsWithIndex(l:List[(Int,String)]):List[(Int,String)]= {
      l.flatMap(indexAndWords /* Listenstruktur aufloesen */
       => getWords(indexAndWords._2) /* Woerter splitten und Sonderzeichen entfernen */
       .map((indexAndWords._1,_)) /* Woerter auf den Index mappen */
     
    )
    }

    /*
     * Extracts all Words from a List containing tupels consisting
     * of a line number and a string
     */


   def createInverseIndex(l:List[(Int,String)]):Map[String,List[Int]]= {
    l.foldLeft(Map.empty[String, List[Int]]){ /* Ganze Liste durchgehen */
       (inverseMap, wordTupel) => 
        inverseMap.updated(wordTupel._2, wordTupel._1 :: inverseMap.getOrElse(wordTupel._2, List())) /* Neue Map erstellen,
        * String dient hier nun als Key und Values wird eine Liste von Indizes angehaengt
        */
     }
   }

   def andConjunction(words:List[String], invInd:Map[String,List[Int]]):List[Int]={
       val lTemp =  words.foldLeft(List.empty[Int]){
       (wordList, wordSearched) =>
          wordList :::  invInd.getOrElse(wordSearched, List()).distinct
        
     }
     
     lTemp.filter( p => lTemp.count(_ == p) == words.length).distinct.reverse
   }

   def orConjunction(words:List[String], invInd:Map[String, List[Int]]):List[Int]={
     
     words.foldLeft(List.empty[Int]){
       (wordList, wordSearched) =>
          wordList :::  invInd.getOrElse(wordSearched, List()) /* Bilde eine Liste mit allen Indizes, die gefunden wurden */
        
     }.foldLeft(List.empty[Int]){
     	(l, ele) => if (l.contains(ele)) l else ele :: l /* Entferne doppelte Indizes */
     	}
   }
}


object Processing{

  def getData(filename:String):List[(Int,String)]={

    val url= getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    var c = -1
    val result= (for (row <- iter) yield {c=c+1;(c,row)}).toList
    src.close()
    result
  }
}