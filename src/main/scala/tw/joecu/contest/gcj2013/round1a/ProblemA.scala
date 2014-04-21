package tw.joecwu.contest.gcj2013.round1a

import grizzled.slf4j.Logging
import java.io.{BufferedReader, File, PrintWriter}
import scala.io.Source
import scala.Array
import scala.math._
/**
 * Created by joewu on 4/19/14.
 */
object ProblemA extends Logging {
  case class Questions(q:Int, r:BigDecimal, t:BigDecimal)


  val output = new PrintWriter(new File("2013r1a_resultA.out"))

  def main(args : Array[String]) {
    info("Start Fighting!!!")

    //    val file = Source.fromFile(args(0))
    val file = Source.fromURL(getClass().getResource("/2013/smallA.in"))

    val reader = file.bufferedReader()

    val total = reader.readLine().toInt

    info(s"Case Count: $total")

    var i = 0;
    val qs = {for( i <- 1 to total) yield parseQuestions(i, reader)}.toList

    qs.map{ q=>
      info(s"Q #${q.q} => ${q.r} ${q.t}")
      val result = solveQuestions(q)
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
    val base = (q.r+1).pow(2) - q.r.pow(2)
    debug(s"r:${q.r} t:${q.t} base=>$base")

    BigInt(math.ceil((((q.t / Pi) - base ) / 4).doubleValue).toLong+1)
  }
}
