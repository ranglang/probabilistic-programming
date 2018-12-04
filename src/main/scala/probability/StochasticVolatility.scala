package probability

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import com.stripe.rainier.cats._
import java.nio.file.Paths
import cats.implicits._
import kantan.csv._
import kantan.csv.ops._

object SV extends App {
  implicit val rng = ScalaRNG(500)

  val rawData = Paths.get("data/sv_ou_sims.csv")
  val reader = rawData.asCsvReader[(Double, Double, Double)](rfc.withHeader)
  val ys: Vector[(Double, Double)] = reader.
    collect {
      case Right(a) => (a._1, a._2)
    }.
    toVector

  val prior = for {
    phi1 <- Beta(5.0, 2.0).param
    phi = 2 * phi1 - 1
    mu <- Normal(0.0, 2.0).param
    sigma <- LogNormal(2.0, 2.0).param
    x0 <- Normal(mu, sigma * sigma / (1 - phi * phi)).param
    t0 = 0.0
  } yield (t0, phi, mu, sigma, x0)

  def ouStepReal(phi: Real, mu: Real, sigma: Real, x0: Real, dt: Double) = {
    val mean = mu + (-1.0 * phi * dt).exp * (x0 - mu)
    val variance = sigma.pow(2) * (1 - (-2 * phi * dt).exp) / (2*phi)
    Normal(mean, variance.pow(0.5))
  }

  def step(st: (Double, Real, Real, Real, Real),
           y: (Double, Double)) = {
    val (t, phi, mu, sigma, x0) = st
    val dt = y._1 - t

    for {
      x1 <- ouStepReal(phi, mu, sigma, x0, dt).param
      _ <- Normal(0.0, (x1 * 0.5).exp).fit(y._2)
    } yield (t + dt, phi, mu, sigma, x1)
  }

  val fullModel = prior flatMap (p => ys.foldM(p)(step))

  val model = for {
    ps <- fullModel
  } yield
      Map("phi" -> ps._2, "mu" -> ps._3, "sigma" -> ps._4)

  val thin = 20

  // val metropolis = model.sample(Metropolis, 5000, 10000 * thin, thin)
  val hmc = model.sample(HMC(5), 5000, 10000 * thin, thin)
  // val nuts = model.sample(Nuts(10), 5000, 10000 * thin, thin)
  val ehmc = model.sample(Ehmc, 5000, 10000 * thin, thin)

  val headers = rfc.withHeader("phi", "mu", "sigma")
  // val outmh = new java.io.File("data/metropolis_sv.csv")
  val outhmc = new java.io.File("data/hmc_sv.csv")
  // val outnuts = new java.io.File("data/nuts_sv.csv")
  val outehmc = new java.io.File("data/ehmc_sv.csv")

  // outmh.writeCsv(metropolis.map(_.values), headers)
  outhmc.writeCsv(hmc.map(_.values), headers)
  // outnuts.writeCsv(nuts.map(_.values), headers)
  outehmc.writeCsv(ehmc.map(_.values), headers)
}
