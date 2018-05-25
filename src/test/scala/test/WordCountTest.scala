package test

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import wordcount.Processing

/**
 * @author hendrik

 */

@RunWith(classOf[JUnitRunner])
class WordCountTest extends FunSuite{
  
 trait teststrings{
   
     val s1="This 88 is! a,Test! The result !!!should be: 8 Words"
     val words_s1= List("a", "be", "is", "result", "should", "test", "the", "this", "words")
     val s2="This is another test. It contains a lot of words which are also in string 1."
     val words_s2= List("a", "also", "another", "are", "contains", "in", "is", "it", "lot", "of", "string", "test", "this", "which", "words")
     val test_list= List((0,s1),(1,""),(2,s2),(3,""))
     val wordlist=List("a", "a", "also", "another", "are", "be", "contains", "in", "is", "is", "it", "lot", "of", "result", "should", "string",
         "test", "test", "the", "this", "this", "which", "words", "words")
     val wordOccurences= List(("a",2), ("also",1), ("another",1), ("are",1), ("be",1), ("contains",1), ("in",1), ("is",2), 
           ("it",1), ("lot",1), ("of",1), ("result",1), ("should",1), ("string",1), ("test",2), ("the",1), ("this",2), ("which",1), ("words",2))
     val wordsWithIndex= List((0,"a"), (0,"be"), (0,"is"), (0,"result"), (0,"should"), (0,"test"), (0,"the"), (0,"this"), (0,"words"), (2,"a"), 
           (2,"also"), (2,"another"), (2,"are"), (2,"contains"), (2,"in"), (2,"is"), (2,"it"), (2,"lot"), (2,"of"), (2,"string"), (2,"test"), 
           (2,"this"), (2,"which"), (2,"words")) 
     val inverseIndex= Map("test" -> List(0, 2), "this" -> List(0, 2), "in" -> List(2), "are" -> List(2), "is" -> List(0, 2), "another" -> List(2), 
         "result" -> List(0), "it" -> List(2), "a" -> List(0, 2), "string" -> List(2), "also" -> List(2), "should" -> List(0), "lot" -> List(2), 
         "words" -> List(0, 2), "which" -> List(2), "be" -> List(0), "contains" -> List(2), "of" -> List(2), "the" -> List(0))
 
 } 
  
 val proc= new Processing()
 val data= Processing.getData("MobyDickC1.txt")
 test("Load File from Resources"){
    
     val d2=data.toArray
     assert((3,"little or no money in my purse, and nothing particular to interest me on")===((d2)(3)))   
 }
  
  test("Test Word Extraction 1"){
    
     new teststrings{
       val r= proc.getWords(s1)
       assert(r.length===9)
       assert(r.sorted===words_s1)
     }
  }
  
  test("Test Word Extraction 2"){
    
     new teststrings{
       val r= proc.getWords(s2)
       assert(r.length===15)
       assert(r.sorted===words_s2)
     }
  }
  
  test("get All Words Extraction"){
    
    new teststrings{
    
       val result= proc.getAllWords(test_list)
       assert (result.length===24)
       assert (result.sorted===wordlist)
    }
  }

  test("count Words"){
    
    new teststrings{
    
       val result= proc.countTheWords(proc.getAllWords(test_list))
       assert (result.length===19)
       assert (result.sorted===wordOccurences)
    }
  }
  
  test("count Words MapReduce"){
    
    new teststrings{
    
       val result= proc.countTheWordsMR(proc.getAllWords(test_list))
       assert (result.length===19)
       assert (result.sorted===wordOccurences)
    }
  }
  
  test("get All Words Extraction with Indizees"){
    
    new teststrings{
    
       val result= proc.getAllWordsWithIndex(test_list)
       assert (result.length===24)
       assert (result.sorted===wordsWithIndex)
    }
  }
  
  test("create Inverse Indizees"){
    
    new teststrings{
    
       val result= proc.createInverseIndex(proc.getAllWordsWithIndex(test_list))
       assert (result===inverseIndex)
    }
  }
  
  test("test and Conjunction 1"){
    
    new teststrings{
    
       val invInd= proc.createInverseIndex(proc.getAllWordsWithIndex(test_list))
       val result= proc.andConjunction(List("this","test","contains"), invInd)
       assert (result===List(2))
    }
  }  
  
    test("test and Conjunction 2"){
    
    new teststrings{
    
       val invInd= proc.createInverseIndex(proc.getAllWordsWithIndex(test_list))
       val result= proc.andConjunction(List("this","is","a"), invInd)
       assert (result===List(0,2))
    }
  }
  
  test("test and Conjunction 3"){
    
    new teststrings{
    
       val invInd= proc.createInverseIndex(proc.getAllWordsWithIndex(test_list))
       val result= proc.andConjunction(List("this","hello"), invInd)
       assert (result===List())
    }
  }
  
  test("test or Conjunction 1"){
    
    new teststrings{
    
       val invInd= proc.createInverseIndex(proc.getAllWordsWithIndex(test_list))
       val result= proc.orConjunction(List("hello","test"), invInd)
       assert (result===List(0,2))
    }
  }  
  
  test("test or Conjunction 2"){
    
    new teststrings{
    
       val invInd= proc.createInverseIndex(proc.getAllWordsWithIndex(test_list))
       val result= proc.orConjunction(List("hello","contains"), invInd)
       assert (result===List(2))
    }
  }
  
  test("test or Conjunction 3"){
    
    new teststrings{
    
       val invInd= proc.createInverseIndex(proc.getAllWordsWithIndex(test_list))
       val result= proc.orConjunction(List("hello","bang"), invInd)
       assert (result===List())
    }
  }
}

