package tw.joecwu.contest.gcj2014.round1a

import grizzled.slf4j.Logging
import java.io.{BufferedReader, File, PrintWriter}
import scala.io.Source
import scala.Array
import scala.math._
import scalaz._
import Scalaz._

/**
 * Created by joewu on 4/25/14.
 */
object ProblemB extends Logging {
  case class Questions(q:Int, r:BigDecimal, t:BigDecimal)


  val output = new PrintWriter(new File("2014r1a_resultB.out"))

  def main(args : Array[String]) {
    info("Start Fighting!!!")
    val file = Source.fromURL(getClass().getResource("/2014/r1a/smallB.in"))

    val reader = file.bufferedReader()

    val total = reader.readLine().toInt

    info(s"Case Count: $total")

    var i = 0;
    val qs = {for( i <- 1 to total) yield parseQuestions(i, reader)}.toList

    qs.map{ q=>
      info(s"Q #${q.q} => ${q.r} ${q.t}")
      val result = solveQuestions(q)
      info(s"Case #${q.q}: ${result}")
      output.write(s"Case #${q.q}: ${result}\r\n")
    }

    file.close()
    output.close()
  }

  def parseQuestions(i: Int, reader: BufferedReader) = {

    val line = reader.readLine().split(' ')

    Questions(i,BigDecimal(line(0)),BigDecimal(line(1)))
  }

  def solveQuestions(q:Questions) : BigInt = {
    debug(s"r:${q.r} t:${q.t}")

    BigInt(0)
  }
}
