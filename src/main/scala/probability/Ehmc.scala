package probability

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import cats._
import cats.implicits._
import scala.collection.mutable.ListBuffer
import math._
import scala.annotation.tailrec

case object Ehmc extends Sampler {
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

    /**
      * Sample the value of the momentum from a standard multivariate normal
      * distribution
      * @return an array of standard normal random variables
      */
    def samplePhi: Array[Double] =
      Array.fill(nVars)(rng.standardNormal)

    def longestBatch(
      theta: Array[Double],
      phi: Array[Double],
      eps: Double,
      L: Int) = {

      def loop(thetaOut: Array[Double], phiOut: Array[Double],
               t: Array[Double], p: Array[Double], l: Int
      ): (Array[Double], Array[Double], Int) = {
        if (Nuts.dot(Nuts.minus(t, theta), p) >= 0) {
          val (t1, p1) = Hmc.leapfrogs(eps, gradient, l, t, p)
          if (l == L)
            loop(t1, p1, t1, p1, l + 1)
          else
            loop(thetaOut, phiOut, t1, p1, l + 1)
        } else {
          (thetaOut, phiOut, l)
        }
      }

      loop(theta, phi, theta, phi, 1)
    }

    def longestBatchStep(
      eps: Double,
      l0: Int)(thetal: (Array[Double], Int)): (Array[Double], Int) = {

      val phi = samplePhi
      val theta = thetal._1
      val (t, p, l) = longestBatch(theta, phi, eps, l0)
      val (propTheta, propPhi) = if (l < l0) {
        Hmc.leapfrogs(eps, gradient, l0 - l, t, p)
      } else {
        (t, p)
      }
      val u = scala.util.Random.nextDouble()
      val a = Hmc.logAcceptance(propTheta, propPhi, theta, phi, pos)
      if (log(u) < a) {
        (propTheta, l)
      } else {
        (theta, l)
      }
    }

    def discreteUniform(min: Int, max: Int)(implicit rng: RNG) = {
      math.floor(rng.standardUniform * (max - min) + min).toInt
    }

    def empiricalLongestBatch(
      eps: Double,
      l0: Int,
      k: Int = 200)(theta: Array[Double]): Vector[Int] = {

      Vector.iterate((theta, l0), k)(longestBatchStep(eps, l0)).
        map(_._2)
    }

    def step(
      l0: Int,
      mu: Double,
      empiricalL: Vector[Int],
      delta: Double = 0.65)(s: DualAverageState) = {

      // sample the momentum variable
      val phi = samplePhi

      // use current step size
      val eps = exp(s.logeps)
      // if (s.iter == warmupIterations)
      //   println(s"Step size at end of warmup is $eps")

      // draw the number of leapfrog steps from the empirical dist
      val k = empiricalL.size
      val i = discreteUniform(0, k)
      val l = empiricalL(i)

      // perform l leapfrog steps
      val (t, p) = Hmc.leapfrogs(eps, gradient, l, s.theta, phi)

      // calculate acceptance
      val a = Hmc.logAcceptance(t, p, s.theta, phi, pos)

      // tune step size during warmup iterations
      val (hm1, logeps1, logepsbar1) = if (s.iter < warmupIterations) {
        val acceptProb = min(1.0, exp(a))
        DualAverage.updateEps(s.iter, mu, delta, acceptProb)(s.hm, s.logeps, s.logepsbar)
      } else {
        (s.hm, s.logeps, s.logepsbar)
      }

      // val (hm1, logeps1, logepsbar1) = (s.hm, s.logeps, s.logepsbar)

      // determine if the proposed parameter is accepted
      val u = rng.standardUniform
      if (log(u) < a) {
        DualAverageState(s.iter + 1, t, s.accepted + 1,
                         logeps1, logepsbar1, hm1)
      } else {
        s.copy(iter = s.iter + 1, logeps = logeps1,
               logepsbar = logepsbar1, hm = hm1)
      }
    }

    var i = 0
    val initTheta = initialiseTheta
    val eps0 = Nuts.findReasonableEpsilon(initTheta, samplePhi, pos, gradient, pos)
    var current = DualAverageState(1, initTheta, 0, log(eps0), 0.0, 0.0)
    // println(s"Initial step size $eps0")
    val l0 = 100
    var empiricalL = Vector(l0)
    val buf = new ListBuffer[Array[Double]]

    while (i < iterations) {
      // if (i % 1000 == 0)
      //   println(s"Iteration $i")

      // once step size tuning has completed, compute
      // empirical distribution
      if (i == warmupIterations) {
        // println("Determining empirical distribution")
        empiricalL = empiricalLongestBatch(exp(current.logeps), l0)(current.theta)
      }

      current = step(l0, log(10 * eps0), empiricalL)(current)

      if (i % keepEvery == 0) {
        buf += current.theta
      }

      i += 1
    }
    buf.toList
  }
}
