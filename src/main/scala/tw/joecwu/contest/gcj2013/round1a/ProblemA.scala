package tw.joecwu.contest.gcj2013.round1a

import grizzled.slf4j.Logging
import java.io.{BufferedReader, File, PrintWriter}
import scala.io.Source
import scala.Array
import scala.math._
import scalaz._
import Scalaz._

/**
 * Created by joewu on 4/19/14.
 */
object ProblemA extends Logging {
  case class Questions(q:Int, r:BigDecimal, t:BigDecimal)


  val output = new PrintWriter(new File("2013r1a_resultA.out"))

  def main(args : Array[String]) {
    info("Start Fighting!!!")

    //    val file = Source.fromFile(args(0))
    val file = Source.fromURL(getClass().getResource("/2013/largeA.in"))

    val reader = file.bufferedReader()

    val total = reader.readLine().toInt

    info(s"Case Count: $total")

    var i = 0;
    val qs = {for( i <- 1 to total) yield parseQuestions(i, reader)}.toList

    qs.map{ q=>
      info(s"Q #${q.q} => ${q.r} ${q.t}")
      val result = solveQuestions(q)
      info(s"Case #${q.q}: ${result}")
      info(s"$result cycle needs paint:"+calNum(q.r,result))
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

    var left = BigInt(0)
    var right = BigInt(2000000000)
    while((right-left)>1){
      val mid = (right+left)/2
      val need = calNum(q.r,mid)
      (q.t>=need) ? (left=mid) | (right=mid)
      debug(s"Q(${q.q},${q.r},${q.t}) L:$left, R:$right, M:$mid => need:$need")
    }
    left
  }

  def calNum(r:BigDecimal, n:BigInt) = {
    val a = (r*2*BigDecimal(n))+triangularNum(BigDecimal(n)*2-1)
    debug("calNum:"+a.doubleValue().toString)
    a
  }

  def triangularNum(n:BigDecimal) : BigDecimal = {
    (n.pow(2)+n)/2
  }
}


object BigDecimalMath {
  implicit def toBigDecimal(decimal: String): BigDecimal = BigDecimal(decimal)

  def sqrt(x: BigDecimal): BigDecimal = {
    val maxIterations = x.mc.getPrecision + 1

    val guessSteam: Stream[BigDecimal] = newtonRaphsonApproximations(x).take(maxIterations)
    val exactMatch: Option[Stream[BigDecimal]] = guessSteam.sliding(2).find(a => a(0) == a(1))
    val root: Stream[BigDecimal] = exactMatch.getOrElse(Stream(guessSteam.last))

    root(0)
  }

  /**
   * A sequence of BigDecimals the tend towards the square root of toSqrt.
   * Using the Newton Raphson Approximations http://en.wikipedia.org/wiki/Newton's_method
   * @param toSqrt the value to find the root of
   * @param guess the first guess to iterate over (typically toSqrt/2)
   * @return
   */
  private[this] def newtonRaphsonApproximations(toSqrt: BigDecimal, guess: BigDecimal): Stream[BigDecimal] =
    Stream.cons(guess, newtonRaphsonApproximations(toSqrt, ((toSqrt / guess) + guess) / 2))

  private[this] def newtonRaphsonApproximations(toSqrt: BigDecimal): Stream[BigDecimal] =
    newtonRaphsonApproximations(toSqrt, toSqrt / 2)

}