package tw.joecwu.contest.gcj2014.qualification

import grizzled.slf4j.Logging
import scala.io.Source
import java.io.{File, PrintWriter, BufferedReader}

/**
 * Created by joewu on 4/12/14.
 * Magic Trick
 */
object ProblemA extends Logging {
  case class Questions(q:Int, line1:String,line2:String)


  val output = new PrintWriter(new File("resultA.out"))

  def main(args : Array[String]) {
    info("Start Fighting!!!")

//    val file = Source.fromFile(args(0))
    val file = Source.fromURL(getClass().getResource("/smallA.in"))

    val reader = file.bufferedReader()

    val total = reader.readLine().toInt

    info(s"Case Count: $total")

    var i = 0;
    val qs = {for( i <- 1 to total) yield parseQuestions(i, reader)}.toList

    file.close()
    output.close()
  }

  def parseQuestions(i: Int, reader: BufferedReader) = {

      val firstNumber = reader.readLine().toInt
      val l = 0;
      val f = {
        Array(
          reader.readLine(),
          reader.readLine(),
          reader.readLine(),
          reader.readLine()
        )
      }
      val line1 = f(firstNumber-1)

      val secondNumber = reader.readLine().toInt
      val s = {
        Array(
          reader.readLine(),
          reader.readLine(),
          reader.readLine(),
          reader.readLine()
        )
      }
      val line2 = s(secondNumber-1)

      val result = check(line1, line2)

      info(s"Case #$i: $result")
      output.write(s"Case #$i: $result\r\n")
      Questions(i,line1,line2)
  }

  def check(line1:String, line2:String) = {
    val l1 = line1.split(' ').map{_.toInt}.toList
    val l2 = line2.split(' ').map{_.toInt}.toList
    val mResult = l1.filter(x => l2.contains(x))
    val mSize = mResult.length
    if(mSize > 1){
      "Bad magician!"
    }else if (mSize == 1){
      mResult(0).toString()
    }else{
      "Volunteer cheated!"
    }
  }
}
