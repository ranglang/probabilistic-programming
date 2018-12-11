package probability

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import cats._
import cats.implicits._
import kantan.csv._
import kantan.csv.ops._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import math._

/**
  * No U-turn sampler with dual averaging, HMC with selection of step size
  * epsilon and number of leapfrog steps l in an adaptation phase
  */
case class Nuts(maxTreeDepth: Int) extends Sampler {
  import Nuts._

  def sample(density: DensityFunction,
             warmupIterations: Int,
             iterations: Int,
             keepEvery: Int)(implicit rng: RNG): List[Array[Double]] = {

    val delta: Double = 0.65 // target acceptance rate
    val deltamax: Double = 1000 // if pos(theta) - log(u) < deltamax stop

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

    // E_k = 0.5 p^2
    def kinetic(array: Array[Double]): Double = {
      var k = 0.0
      var i = 0
      while (i < nVars) {
        val p = array(i)
        k += (p * p)
        i += 1
      }
      k * 0.5
    }

    def logDensity(
      theta: Array[Double],
      phi: Array[Double]) =
      pos(theta) - kinetic(phi)

    /**
      * Calculate the log-acceptance rate
      */
    def logAcceptance(
      propTheta: Array[Double],
      propPhi: Array[Double],
      theta: Array[Double],
      phi: Array[Double]) = {

      // check if any parameters are NaN
      val thetaNans = propTheta.map(_.isNaN).
        foldLeft(false)((f, a) => a || f)

      if (thetaNans) {
        -1e99
      } else {
        val ap = logDensity(propTheta, propPhi) -
          logDensity(theta, phi)

        if (ap.isNaN) {
          -1e99
        } else {
          ap
        }
      }
    }

    def initialiseTheta: Array[Double] =
      Array.fill(nVars)(rng.standardNormal)

    /**
      * Sample the value of the momentum from a standard multivariate normal
      * distribution
      * @return an array of standard normal random variables
      */
    def samplePhi: Array[Double] = {
      Array.fill(nVars)(rng.standardNormal)
    }

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
      gradient: Array[Double] => Array[Double])(
      theta: Array[Double],
      phi: Array[Double]) = {
      val p1 = phi.zip(gradient(theta)).
        map { case (p, dt) =>  p + dt * 0.5 * eps }
      val t1 = theta.zip(p1).map { case (t, p) => t + eps * p }
      val p2 = p1.zip(gradient(t1)).
        map { case (p, dt) =>  p + dt * 0.5 * eps }

      (t1, p2)
    }

    /**
      * Update the value of epsilon during an adaptation phase
      * @param m the iteration number
      * @param mu default set to log(10 * eps0)
      * @param acceptProb the acceptance probability from the previous time step
      */
    def updateEps(
      m:          Int,
      mu:         Double,
      acceptProb: Double,
      nAccept:    Int,
      k:          Double = 0.75,
      gamma:      Double = 0.05,
      t0:         Double = 10
    )(hm0:        Double,
      logeps0:    Double,
      logepsbar0: Double): (Double, Double, Double) = {

      val md = m.toDouble
      val ra = 1 / (md + t0)
      val hm = (1 - ra) * hm0 + ra * (delta - acceptProb / nAccept.toDouble)
      val logeps1 = mu - (sqrt(md) / gamma) * hm
      val power = pow(md, -k)
      val logepsbar1 = power * logeps1 + (1.0 - power) * logepsbar0

      (hm0, logeps1, logepsbar1)
    }

    case class TreeState(
      thetaM: Array[Double],
      phiM: Array[Double],
      thetaP: Array[Double],
      phiP: Array[Double],
      theta1: Array[Double],
      n:      Int,
      s:      Boolean,
      acceptProb: Double,
      nAccept: Int)

    def updateS(
      s: Boolean,
      thetaP: Array[Double],
      thetaM: Array[Double],
      phiP: Array[Double],
      phiM: Array[Double]) = {

      val tptm = minus(thetaP, thetaM)
      val i1 = dot(tptm, phiM) >= 0
      val i2 = dot(tptm, phiP) >= 0

      s && i1 && i2
    }

    def buildTree(
      u:      Double,
      v:      Int,
      j:      Int,
      eps:    Double,
      theta0: Array[Double],
      phi0:   Array[Double])(
      theta:  Array[Double],
      phi:    Array[Double]): TreeState = {

      if (j == 0) {
        val (t1, p1) = leapfrog(eps * v, gradient)(theta, phi)
        val a1 = logDensity(t1, p1)
        val n = if (a1 >= log(u)) 1 else 0
        val s = deltamax + a1 > log(u)
        val a = logAcceptance(t1, p1, theta0, phi0)

        TreeState(t1, p1, t1, p1, t1, n, s, a, 1)
      } else {
        val bt = buildTree(u, v, j - 1, eps, theta0, phi0) _
        val st1 = bt(theta, phi)
        var thetaMinus = st1.thetaM
        var phiMinus = st1.phiM
        var thetaPlus = st1.thetaP
        var phiPlus = st1.phiP

        // implicitly build left and right trees
        val st = if (st1.s) {
          val st2 = if (v == -1) {
            val st = bt(thetaMinus, phiMinus)
            thetaMinus = st.thetaM
            phiMinus = st.phiM
            st
          } else {
            val st = bt(thetaPlus, phiPlus)
            thetaPlus = st.thetaP
            phiPlus = st.phiP
            st
          }
          val u = scala.util.Random.nextDouble()
          val p = st2.n.toDouble / math.max(st1.n.toDouble + st2.n.toDouble, 1.0)
          val newTheta = if (u < p) {
            st2.theta1
          } else {
            st1.theta1
          }

          val a = st1.acceptProb + st2.acceptProb
          val nA = st1.nAccept + st2.nAccept
          val newS = updateS(st2.s && st1.s, thetaPlus, thetaMinus, phiPlus, phiMinus)
          st2.copy(theta1 = newTheta, acceptProb = a, nAccept = nA,
                   s = newS, n = st1.n + st2.n)
        } else {
          st1
        }

        st
      }
    }

    def sampleDirection: Int = {
      val u = scala.util.Random.nextDouble()
      if (u < 0.5) -1 else 1
    }

    var accepted = 0

    def loopTrees(
      u: Double,
      eps: Double,
      j: Int,
      phi0: Array[Double],
      theta: Array[Double]) = { st: TreeState =>

      def loop(current: TreeState, j: Int): TreeState = {
        if (j > maxTreeDepth) {
          println("Exceeded max tree depth")
          current
        } else if (current.s) {
          val vj = sampleDirection
          val bt = buildTree(u, vj, j, eps, theta, phi0) _
          val st1 = if (vj == -1) {
            bt(current.thetaM, current.phiM)
          } else {
            bt(current.thetaP, current.phiP)
          }
          val u1 = scala.util.Random.nextDouble()
          val p = st1.n.toDouble / current.n.toDouble
          val newTheta = if (st1.s && u1 < p) {
            accepted += 1
            st1.theta1
          } else {
            current.theta1
          }
          val newState = st1.copy(theta1 = newTheta, n = current.n + st1.n,
                   s = updateS(st1.s, st1.thetaP, st1.thetaM, st1.phiP, st1.phiM))

          loop(newState, j + 1)
        } else {
          current
        }
      }

      loop(st, j)
    }

    case class DualAverageState(
      iter:      Int,
      theta:     Array[Double],
      logeps:    Double,
      logepsbar: Double,
      hm:        Double)

    /**
      * A single step of NUTS
      * @param s the current state
      */
    def step(mu: Double)(s: DualAverageState): DualAverageState = {
      val phi = samplePhi
      val u = scala.util.Random.nextDouble() * exp(logDensity(s.theta, phi))
      val eps = exp(s.logeps)
      val initst = TreeState(s.theta, phi, s.theta, phi, s.theta, 1, true, 0.0, 0)
      val st = loopTrees(u, eps, 0, phi, s.theta)(initst)
      val (hm1, logeps1, logepsbar1) = if (s.iter < warmupIterations) {
        updateEps(s.iter, mu, min(1.0, exp(st.acceptProb)),
                  st.nAccept)(s.hm, s.logeps, s.logepsbar)
      } else {
        (s.hm, s.logepsbar, s.logepsbar)
      }
      DualAverageState(s.iter + 1, st.theta1, logeps1, logepsbar1, hm1)
    }

    val init = initialiseTheta
    val eps0 = findReasonableEpsilon(init, samplePhi, pos, gradient)
    println(s"initial step size $eps0")
    val initState = DualAverageState(1, init, log(eps0), 0.0, 0.0)
    val mu = log(10 * eps0)

    val buf = new ListBuffer[Array[Double]]
    var i = 0
    var current = initState
    while (i < iterations) {
      current = step(mu)(current)

      if (i % keepEvery == 0) {
        buf += current.theta
      }

      i += 1
    }
    buf.toList
  }
}

object Nuts {
    /**
      * Initialise the value of epsilon
      */
    def findReasonableEpsilon(
      theta: Array[Double],
      phi: Array[Double],
      pos: Array[Double] => Double,
      gradient: Array[Double] => Array[Double]): Double = {

      val eps = 1.0
      val (initTheta, initPhi) = Hmc.leapfrog(eps, gradient, theta, phi)
      def prop(propTheta: Array[Double], propPhi: Array[Double]) =
        Hmc.logAcceptance(propTheta, propPhi, theta, phi, pos)
      val i = prop(initTheta, initPhi) > log(0.5)
      val a = if (i) 1.0 else -1.0

      def loop(thetaP: Array[Double], phiP: Array[Double],
               curEps: Double, count: Int): Double = {

        if (a * prop(thetaP, phiP) > -a * log(2.0) && count < 100) {
          val (propTheta, propPhi) = Hmc.leapfrog(curEps, gradient, theta, phi)
          loop(propTheta, propPhi, pow(2.0, a) * curEps, count + 1)
        } else if (count > 100) {
          println("Could not find reasonable epsilon in 100 steps")
          curEps
        } else {
          curEps
        }
      }

      loop(initTheta, initPhi, eps, 0)
    }

  def dot(xs: Array[Double], ys: Array[Double]): Double =
    (xs zip ys).map { case (x, y) => x * y }.sum

  def minus(xs: Array[Double], ys: Array[Double]) =
    (xs zip ys).map { case (x, y) => x - y }
}
