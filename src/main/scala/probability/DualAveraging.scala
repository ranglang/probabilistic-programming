package probability

import breeze.stats.distributions._
import math._

/**
  * TODO: How to handle proposals of NaN
  */
case class DualAverageState(
  iter:      Int,
  theta:     Array[Double],
  accepted:  Int,
  logeps:    Double,
  logepsbar: Double,
  hm:        Double)

/**
  * Hamiltonian Monte Carlo with dual averaging for selecting
  * optimum step size
  * @param lambda target simulation length
  * @param delta target acceptance rate
  */
object DualAverage {

  /**
    * Update the value of epsilon during an adaptation phase
    * @param m the iteration number
    * @param mu default set to log(10 * eps0)
    * @param acceptProb the acceptance probability from the previous time step
    */
  def updateEps(
    m:     Int,
    mu:    Double,
    delta: Double,
    acceptProb: Double,
    k:     Double = 0.75,
    gamma: Double = 0.05,
    t0:    Double = 10.0
  )(hm0: Double,
    logeps0: Double,
    logepsbar0: Double): (Double, Double, Double) = {

    val md = m.toDouble
    val ra = 1 / (md + t0)
    val hm = (1 - ra) * hm0 + ra * (delta - acceptProb)
    val logeps1 = mu - ((sqrt(md) * hm) / gamma)
    val power = pow(md, -k)
    val logepsbar1 = power * logeps1 + (1.0 - power) * logepsbar0

    (hm, logeps1, logepsbar1)
  }
}
