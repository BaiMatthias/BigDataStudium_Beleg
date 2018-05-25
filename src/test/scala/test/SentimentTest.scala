package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import wordcount._

/**
 * @author hendrik

 */

@RunWith(classOf[JUnitRunner])
class SentimentTest extends FunSuite {
  
  
  val sentiAnalyse= new Sentiments("AFINN-111.txt")
  
  test("Load Sentiments"){
    
    val sentAccident =sentiAnalyse.sentiments.getOrElse("accident",0)
     assert(sentAccident == -2)    
  }

  test("test getDocumentGroupedByCounts"){
    
    val data= sentiAnalyse.getDocumentGroupedByCounts("MobyDickShort.txt",10)
    val res= List((1,List("call", "me", "ishmael", "some", "years", "ago", "never", "mind", "how", "long")), 
        (2,List("precisely", "having", "little", "or", "no", "money", "in", "my", "purse", "and")), 
        (3,List("nothing", "particular", "to", "interest", "me", "on", "shore", "i", "thought", "i")), 
        (4,List("would", "sail", "about", "a", "little", "and", "see", "the", "watery", "part")),
        (5,List("of", "the", "world", "it", "is", "a", "way", "i", "have", "of")), 
        (6,List("driving", "off", "the", "spleen", "and", "regulating", "the", "circulation", "whenever", "i")),
        (7,List("find", "myself", "growing", "grim", "about", "the", "mouth", "whenever", "it", "is")), 
        (8,List("a", "damp", "drizzly", "november", "in", "my", "soul", "whenever", "i", "find")), 
        (9,List("myself", "involuntarily", "pausing", "before", "coffin", "warehouses", "and", "bringing", "up", "the")), 
        (10,List("rear", "of", "every", "funeral", "i", "meet", "and", "especially", "whenever", "my")), 
        (11,List("hypos", "get", "such", "an", "upper", "hand", "of", "me", "that", "it")), 
        (12,List("requires", "a", "strong", "moral", "principle", "to", "prevent", "me", "from", "deliberately")), 
        (13,List("stepping", "into", "the", "street", "and", "methodically", "knocking", "people", "s", "hats")), 
        (14,List("off", "then", "i", "account", "it", "high", "time", "to", "get", "to")), 
        (15,List("sea", "as", "soon", "as", "i", "can", "this", "is", "my", "substitute")),
        (16,List("for", "pistol", "and", "ball", "with", "a", "philosophical", "flourish", "cato", "throws")), 
        (17,List("himself", "upon", "his", "sword", "i", "quietly", "take", "to", "the", "ship")), 
        (18,List("there", "is", "nothing", "surprising", "in", "this", "if", "they", "but", "knew")),
        (19,List("it", "almost", "all", "men", "in", "their", "degree", "some", "time", "or")),
        (20,List("other", "cherish", "very", "nearly", "the", "same", "feelings", "towards", "the", "ocean")), 
        (21,List("with", "me")))
     assert(res===data)
    
  }
  
  test("test analyzeSentiments"){
    
    val res= List((1,0.42857142857142855,0.035), (2,1.75,0.02), (3,0.3333333333333333,0.03), 
        (4,1.2,0.025), (5,0.0,0.04), (6,0.9,0.05), (7,1.0,0.04), (8,-1.5,0.01), 
        (9,-0.8333333333333334,0.03), (10,1.1111111111111112,0.045), (11,2.1,0.05), 
        (12,2.5,0.043478260869565216))

    val epsilon= 0.01
    val data= sentiAnalyse.analyzeSentiments(sentiAnalyse.getDocumentGroupedByCounts("MobyDickC1.txt",200))
    val restest= data zip res map (X=> (Math.abs(X._1._2 - X._2._2), Math.abs(X._1._3 - X._2._3)))
    assert(restest.forall(X=>(X._1<epsilon) && (X._2 < epsilon)===true))

    //senti.createGraph(data.map(X=>(X._1,X._2)))
    
    
  }
  

}