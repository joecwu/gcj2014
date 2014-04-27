package tw.joecwu.contest.gcj2014.qualification

import grizzled.slf4j.Logging
import java.io.{BufferedReader, File, PrintWriter}
import scala.io.Source
import java.text.DecimalFormat

/**
 * Created by joewu on 4/12/14.
 */
object ProblemB extends Logging {
  val formatter = new DecimalFormat("0.0000000")
  case class Questions(q:Int, c:Double, f:Double, x:Double)


  val output = new PrintWriter(new File("2014q_resultB.out"))

  def main(args : Array[String]) {
    info("Start Fighting!!!")

    //    val file = Source.fromFile(args(0))
    val file = Source.fromURL(getClass().getResource("/2014/q/largeB.in"))

    val reader = file.bufferedReader()

    val total = reader.readLine().toInt

    info(s"Case Count: $total")

    var i = 0;
    val qs = {for( i <- 1 to total) yield parseQuestions(i, reader)}.toList
    qs.map{ solveQuestions(_) }.foreach{ r =>
      debug(s"Case #${r._1}: ${formatter.format(r._2)}")
      output.write(s"Case #${r._1}: ${formatter.format(r._2)}\r\n")
    }


    file.close()
    output.close()
  }

  def parseQuestions(i: Int, reader: BufferedReader) = {
    val line = reader.readLine().split(' ')
    Questions(i,line(0).toDouble,line(1).toDouble,line(2).toDouble)
  }

  def solveQuestions(q:Questions) : (Int,Double) = {
    val rate : Double = 2.0
    (q.q, calTime(rate,q.c,q.f,q.x))
  }

  def calTime(rate: Double, c:Double, f:Double, x:Double) : Double = {
    if(x<=c)
      return x/rate
    val remain = x-c
    val currentCost = (x-remain)/rate
    //check buy or not
    val costA = remain/rate
    val costB = x/(rate+f)
    if(costA<costB)
      return currentCost+costA
    else
      return currentCost+calTime(rate+f,c,f,x)
  }

}
