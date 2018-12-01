package probability

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class MyHmc(l: Int, eps: Double) extends Sampler {
  def sample(density: DensityFunction,
             warmupIterations: Int,
             iterations: Int,
             keepEvery: Int)(implicit rng: RNG): List[Array[Double]] = {

    val nVars = density.nVars

    val pos: Array[Double] => Double = {
      input => density.update(input)
      density.density
    }

    val gradient: Array[Double] => Array[Double] = {
      input => density.update(input)
      0.until(nVars).
        map{i => density.gradient(i)}.toArray
    }

    def initialiseTheta: Array[Double] =
      Array.fill(nVars)(rng.standardNormal)

    def samplePhi: Array[Double] =
      Array.fill(nVars)(rng.standardNormal)

    def step(theta: Array[Double]) = {
      val phi = samplePhi
      val (t, p) = Hmc.leapfrogs(eps, gradient, l, theta, phi)
      // println(s"proposed theta ${t.mkString(", ")}")

      // determine if the proposed parameter is accepted
      val a = Hmc.logAcceptance(t, p, theta, phi, pos)
      val u = rng.standardUniform
      // println(s"log-acceptance $a")
      // println(s"log(u) = ${math.log(u)}")
      if (math.log(u) < a) {
        // println("Accepted!")
        t
      } else {
        theta
      }

    }

    val init = initialiseTheta
    println(s"Initial value ${init.mkString(", ")}")
    Stream.iterate(init)(step).
      zipWithIndex.
      filter { case (_, i) => i % keepEvery == 0 }.
      map(_._1).
      take(iterations).
      toList

    // val buf = new ListBuffer[Array[Double]]
    // var i = 0
    // var current = initialiseTheta
    // while (i < iterations) {
    //   current = step(current)

    //   if (i % keepEvery == 0) {
    //     buf += current
    //   }

    //   i += 1
    // }
    // buf.toList
  }
}

object Hmc {
  def phiHalfStep(
    eps: Double,
    theta: Array[Double],
    phi: Array[Double],
    gradient: Array[Double] => Array[Double]) =
    phi.zip(gradient(theta)).
      map { case (p, dt) =>  p + dt * 0.5 * eps }

  def thetaStep(
    eps: Double,
    theta: Array[Double],
    phi: Array[Double]) =
    theta.zip(phi).map { case (t, p) => t + eps * p }

  /**
    * Perform a leapfrog full step with the momentum
    * @param theta the current value of the parameters
    * @param phi the current value of the momentum
    * @param eps the step size
    * @return a tuple containing the updated value of the parameters and
    * momentum
    */
  def leapfrog(
    eps: Double,
    gradient: Array[Double] => Array[Double],
    theta: Array[Double],
    phi: Array[Double]) = {
    val p1 = phiHalfStep(eps, theta, phi, gradient)
    val t1 = thetaStep(eps, theta, p1)
    val p2 = phiHalfStep(eps, t1, p1, gradient)

    (t1, p2)
  }

  /**
    * Perform l leapfrog steps
    */
  @tailrec
  def leapfrogs(
    eps: Double,
    gradient: Array[Double] => Array[Double],
    l: Int,
    theta: Array[Double],
    phi: Array[Double]): (Array[Double], Array[Double]) = {
    if (l == 0) {
      (theta, phi)
    } else {
      val (t, p) = leapfrog(eps, gradient, theta, phi)
      leapfrogs(eps, gradient, l-1, t, p)
    }
  }

  // E_k = 0.5 p^2
  def kinetic(phi: Array[Double]): Double = {
    var k = 0.0
    var i = 0
    while (i < phi.size) {
      val p = phi(i)
      k += (p * p)
      i += 1
    }
    k * 0.5
  }

  def logDensity(
    theta: Array[Double],
    phi: Array[Double],
    pos: Array[Double] => Double) =
    pos(theta) - kinetic(phi)

  /**
    * Calculate the log-acceptance rate
    */
  def logAcceptance(
    propTheta: Array[Double],
    propPhi: Array[Double],
    theta: Array[Double],
    phi: Array[Double],
    pos: Array[Double] => Double) = {
    val a = logDensity(propTheta, propPhi, pos) - logDensity(theta, phi, pos)
    if (a.isNaN) { Math.log(0.0) } else { a.min(0.0) }
  }
    
}
