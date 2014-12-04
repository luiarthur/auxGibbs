import scala.collection.immutable.Vector.empty // vectors
import scala.io.Source                         // input/output
import org.apache.commons.math3.special.Gamma._
import breeze.stats.distributions.{NegativeBinomial,Binomial,Gaussian}
import java.io.File
import scala.math.{log,pow,Pi,exp}
import util.Random

object crp{ //Chinese Restaurant Process
  
  val n = 100 // The number of people and maximum number of tables
  val a = 1.0   // concentration parameter
  
  var table = Vector.fill(1)(1.0) // Vector: number of people at each table
  var customer = Array.fill(10)(1) // Array: table number of each customer

  val ran = new Random()
  def runif():Double = ran.nextDouble

  def genTable(n: Int){
    val r = runif 
    var c = 0
    var min = 0.0
    var max = table(0)/(a+table.sum-1.0)
    var found = false

    while (!found)  {
      if (((min < r) & (r <= max) ) || (c+1 == table.length)) {found=true}
      else {
        c += 1
        val temp = min
        min = max
        max = temp + table(c)/(a+table.sum-1) 
      }
    }

    if (c+1 == table.length) {table = table :+ 1.0}
    else {table = table updated (c, table(c) + 1.0)}
  }

  //val n =10
  def main(args: Array[String]){
    for (k <- 1 to (n-1)){
      //println("A")
      genTable(k) 
    }
  }
  table.foreach(t => println(t))
}
