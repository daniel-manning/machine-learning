package markovChains

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class MarkovChains {

  //val filePath = "/shakespeare_corpus.txt"
  val filePath = "/fiction_corpus.txt"
  val testPath = "/fiction_test.txt"

  val MARKOV_MAP: mutable.Map[Seq[String], mutable.Map[String, Int]] = new mutable.HashMap()
  val CHAIN_SIZE = 2

  def adjustProbabilities(sentence: String): Unit = {
    val segments = sentence.split(" ")
      //.+:("").:+("")
      .sliding(CHAIN_SIZE + 1)
      .filterNot(p => p.length < CHAIN_SIZE + 1)
      .toList

    //segments.foreach(s => println(s"segment: ${s.toList}"))

    for (segment <- segments) {
      val key = segment.take(CHAIN_SIZE)
      val probs = MARKOV_MAP.getOrElse(key, scala.collection.mutable.Map())
      probs(segment.last) = probs.getOrElse(segment.last, 0) + 1
      MARKOV_MAP(key) = probs
    }

    //MARKOV_MAP.foreach(f => if(!f._2.forall(m => m._2 < 2)) println(f))
  }

  def normalize(line: String): String = {
    line.stripLineEnd
      .toLowerCase
      //.filterNot("\\.-,\";:&" contains _)
  }

  Source.fromURL(getClass.getResource(filePath)).getLines()
      .map(normalize)
      .map(s => s.trim)
      .foreach(s => adjustProbabilities(s))

  val r = new Random()

  def nextWord(seed: Seq[String]): Option[String] = {
    val possible = MARKOV_MAP.getOrElse(seed, List())
    r.shuffle(possible.flatMap(pair => List.fill(pair._2)(pair._1))).toList match {
      case x :: _ => Some(x)
      case _      => None
    }
  }

  def nextSentence(): String = {
    val seed:Seq[String] = r.shuffle(MARKOV_MAP).head._1
    //println(s"looking for seed: $seed")
    //val seed = startWords(r.nextInt(startWords.size))
    val sentence: ArrayBuffer[String] = ArrayBuffer()
    sentence.appendAll(seed)
    while (!sentence.last.contains('.')) {
      //println(s"looking for next word: ${sentence.view(sentence.size - CHAIN_SIZE, sentence.size).toList}")
      val nextword = nextWord(sentence.view(sentence.size - CHAIN_SIZE, sentence.size))
      sentence.append(nextword.getOrElse("."))
      //println(s"nextword: $nextword")
    }
    sentence.view(1, sentence.size - 1).mkString(" ").capitalize
  }


  def runTestData():Unit = {
    println("Testing.....")
    val testData:List[(Int, Int)] = Source.fromURL(getClass.getResource(testPath)).getLines()
        .toList
      .map(normalize)
      .map(s => s.trim)
      .map(a => {
        val b = a.split(" ")
          .sliding(CHAIN_SIZE + 1)
          .filterNot(p => p.length < CHAIN_SIZE + 1)
          .map(_.toList)
          .toList
          .map {
            case List(x, y, z) =>  if(nextWord(Seq(x, y)).contains(z)){ 1 } else { 0 }
          }

        (b.sum, b.length)
      })

    val resultAccuracy = testData.foldRight((0, 0)){(a, b) => (a._1 + b._1, a._2 + b._2)}

    println(s"The Accuracy of our model against the test data is:  ${100*resultAccuracy._1.toDouble/resultAccuracy._2.toDouble} %")
    println(s"or ${resultAccuracy._1} correct predictions out of ${resultAccuracy._2} trials")
  }

}


object RunTrial extends MarkovChains with App {

  //loadData()
/*  println("****************************")
  println(MARKOV_MAP)
  println("****************************")*/
  val genText = (0 until 14).map(_ => nextSentence()).mkString("\n")

  println(genText)


  println("****************************")


  runTestData()

}