import scala.collection.immutable.Vector.empty
import org.apache.commons.math3.special.Gamma._
import breeze.stats.distributions.{Gaussian,Binomial}
import scala.io.Source
import java.io.File
import scala.math.{log,pow,Pi,exp,sqrt}
import util.Random

object algorithm8{


  val runif = new Random()

  
  // Write Vetor to File
  def writeToFile(file1: String, file2: String, Y: Array[Double], C: Array[Int], T: Array[Double]) {
    var pw = new java.io.PrintWriter(new File(file1))
    val N = Y.size
    pw.write("y"+" "+"c"+" \n")
    Array.tabulate(N)(i => i).foreach(i => pw.write(Y(i)+" "+ C(i)+"\n"))
    pw.close()
    
    pw = new java.io.PrintWriter(new File(file2))
    pw.write("Theta \n")
    T.foreach(i => pw.write(i+"\n"))
    pw.close()
  }
  

  def relabel(x: Array[Int]): Array[Int] = {
    val uniq = x.distinct
    var y = Array.fill(x.size)(0)
    for (i <- 0 to (uniq.size-1)){
      for (j <- 0 to (x.size-1)){
        if (x(j) == uniq(i)){ y(j) = i+1 } 
      }
    }
    y
  }

  
  def sample(v: Array[Int], p: Array[Double]): Int = {
    val psum = p.sum
    val P = Array(0.0) ++ 
            Array.tabulate(p.size)(i => 1.0 * p.take(i+1).sum / psum )
    val r = runif.nextDouble
    var i = 1
    while( P(i) < r ){
      i = i + 1
    }
    v(i-1)
  }


  def dnorm(x: Double, m: Double, s: Double): Double = {
    1/sqrt(2*Pi*s*s) * exp(-(x-m)*(x-m)/(s*s*2))
  }

  
  def main(args:Array[String]) {
    val G = new Gaussian(0,1)

    //val Y1 = new Gaussian(-.5,.1)
    //val Y2 = new Gaussian(.5,.1)

    val Y1 = new Gaussian(G.draw,.1)
    val Y2 = new Gaussian(G.draw,.1)

    val Y = Array.fill(100)(Y1.draw) ++ Array.fill(100)(Y2.draw)
    val y = Y.sorted
    val n = y.size
    //val y = Array(-1.48,-1.4,-1.16,-1.08,-1.02,.14,.51,.53,.78)


    var c = Array.fill(n)(1)
    var phi = Array.fill(n)(G.draw)


    for ( i <- 0 to (n-1) ){
      var ct = c.take(i)++c.drop(i+1)
      val k = ct.distinct.size
      val h = k + 1
      ct = relabel(ct)

      if (ct.contains(c(i))){
        phi(h) = G.draw
      }

      val w = Array.fill(h)(1)
      for ( t <- 0 to (k-1) ){
        w(t) = ct.count( q => q == t )
      }

      val samp = Array.tabulate(h)(i => i+1)
      val prob = Array.fill(h)(0.0)
      for (j <- 0 to (h-1)){
        prob(j) = w(j) * dnorm(y(i),phi(j),.1)
      }

      c(i) = sample(samp,prob)
      c = ct.take(i) ++ Array(c(i)) ++ ct.drop(i)
    }

    val newPhi = phi.take(c.distinct.size) // take the first c.distinct.size phis
    //c.foreach(i => println(i))
    //newPhi.foreach(i => println(i))
    val K = newPhi.size

    // Metrolopis Sampling: Draw new Phi's
    val N = 100000; val cs = .3; val burn = N / 10
    val results = Array.fill(K)(Array.fill(N)(0.0)) 
    // results(a)(b) refers to the ath draw of the bth theta

    def mh(k: Int /*kth theta*/): Array[Double] = {
      def lik(phi: Double): Double = {
        def which(c: Array[Int], k: Int): Vector[Int] = {
          var out = Vector.fill(0)(0)
          for(i <- 0 to (c.length-1)){
            if (c(i) == k) {out = out :+ i } 
          }
          out
        }
        val z = which(c,k)
        val yi = Array.tabulate(z.size)(i => y(z(i)))
        val x = Array.tabulate(yi.size)(i => pow((yi(i)-phi),2)).sum 
        -.5 * (100 * x + pow(phi,2))
      }

      var cnt  = 0
      var out = Array.fill(N+burn)(0.0) 
      for (i <- 1 to (N+burn-1)){
        out(i) = out(i-1)
        val cand = G.draw * cs + out(i) //rnorm(1,out[i],cs)
        if (log(runif.nextDouble) < lik(cand) - lik(out(i))){
          out(i) = cand
          if (i >= burn) {cnt += 1} 
        }
      }
      
      println("Acceptance Rate "+k+": "+cnt*1.0/N)
      out.drop(burn)
    } //End of Metropolis

    for(k <- 1 to K){results(k-1) = mh(k)}
    val theta = Array.tabulate(K)(k => results(k).sum/N)

    // Printing Results to Terminal:
    //println("y"+" "+"c")
    //Array.tabulate(200)(i => i).foreach(i => println(y(i)+" "+c(i)))
    //theta.foreach(i => println(i+" "+666))

    writeToFile("yc.txt","theta.txt",y,c,theta)

  } //end of main
} // end of object


