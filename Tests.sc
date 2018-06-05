import scala.io.Source
object Tests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet


val liast = List((1, "Das der weitergeht ist ein geiler Text"),(2, "Der hier weitergeht der"),(3, "Und endet hier"))
                                                  //> liast  : List[(Int, String)] = List((1,Das der weitergeht ist ein geiler Tex
                                                  //| t), (2,Der hier weitergeht der), (3,Und endet hier))
val list1 = List("der","weitergeht")              //> list1  : List[String] = List(der, weitergeht)
print("Hallo")                                    //> Hallo
    
    
    
   

  
 def getAllWordsWithIndex(l:List[(Int,String)]):List[(Int,String)]= {
   
    
   l.flatMap(indexAndWords => getWords(indexAndWords._2).map((indexAndWords._1,_))
     
    )

  }                                               //> getAllWordsWithIndex: (l: List[(Int, String)])List[(Int, String)]
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
  }                                               //> getWords: (line: String)List[String]



 


def createInverseIndex(l:List[(Int,String)]):Map[String,List[Int]]={

     l.foldLeft(Map.empty[String, List[Int]]){
       (inverseMap, wordTupel) =>
        inverseMap.updated(wordTupel._2, wordTupel._1::inverseMap.getOrElse(wordTupel._2, List()))
     }
     
   }                                              //> createInverseIndex: (l: List[(Int, String)])Map[String,List[Int]]
   def andConjunction(words:List[String], invInd:Map[String,List[Int]]):List[Int]={

    val lTemp =  words.foldLeft(List.empty[Int]){
       (wordList, wordSearched) =>
          wordList :::  invInd.getOrElse(wordSearched, List()).distinct
        
     }
      
     lTemp.filter( p => lTemp.count(_ == p) == words.length).distinct.reverse
    
      
   }                                              //> andConjunction: (words: List[String], invInd: Map[String,List[Int]])List[In
                                                  //| t]
   
   
    
    def getDocumentGroupedByCounts(filename:String, wordCount:Int):List[(Int,List[String])]= {
    val lWords = getWords(Source.fromFile(filename).getLines.mkString("\n"))
 	   
   lWords.grouped(wordCount).zipWithIndex.map{
      l => (l._2+1, l._1)
    }.toList
  
       
  }                                               //> getDocumentGroupedByCounts: (filename: String, wordCount: Int)List[(Int, Li
                                                  //| st[String])]
  def analyzeSentiments(l:List[(Int,List[String])]):List[(Int, Double, Double)]= {
   
   val mSenti = getData("AFINN-111.txt").map{ line =>
   val number = ("[-]?[0-9]".r.findFirstIn(line._2)).get
   val indexOfNumber = line._2.indexOf(number)
   val word = (line._2.subSequence(0, indexOfNumber -1)).toString().trim()
    (word, number.toInt)
      }.toMap
    
    l.foldLeft(List.empty[(Int,Double,Double)]){
      (nList, tTuple) =>  (tTuple._1, tTuple._2.map(mSenti.getOrElse(_,0)).sum.toDouble,   (tTuple._2.filter(p => mSenti.get(p).map{
        ele => println(ele)
        ele >= -5 && ele <= 5
      }.getOrElse(false)).length.toDouble / tTuple._2.length)) :: nList
    }
    
   
  }                                               //> analyzeSentiments: (l: List[(Int, List[String])])List[(Int, Double, Double)
                                                  //| ]
  
  
 
  def getData(filename:String):List[(Int,String)]={

    val url= getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    var c = -1
    val result= (for (row <- iter) yield {c=c+1;(c,row)}).toList
    src.close()
    result
  }                                               //> getData: (filename: String)List[(Int, String)]
  
  analyzeSentiments(getDocumentGroupedByCounts("C:/Users/MatzesPC/Desktop/TheoInf/Test.txt", 3))
                                                  //> -1
                                                  //| -2
                                                  //| res0: List[(Int, Double, Double)] = List((1,-3.0,0.6666666666666666))
   

}
 
                /* getDocumentGroupedByCounts("C:/Users/MatzesPC/Desktop/TheoInf/Test.txt", 3) */
   /*getDocumentGroupedByCounts("C:/Users/MatzesPC/Desktop/TheoInf/Test.txt", 3)*/
      
                                                  
                   
   
  