package probability

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import com.stripe.rainier.cats._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.exp
import breeze.stats.distributions.{Gaussian, Poisson => BreezePoisson}
import cats._
import cats.implicits._
import kantan.csv._
import kantan.csv.ops._

object Lm extends App {
  implicit val rng = ScalaRNG(2)

  val n = 1000
  val x = Vector.fill(n)(rng.standardNormal)
  val x1 = Vector.fill(n)(rng.standardNormal)

  val b0 = 4.0
  val b1 = -1.5
  val b2 = 2.5

  val y = x.zip(x1) map { case (xi1, xi2) =>
    b0 + b1 * xi1 + b2 * xi2 + rng.standardNormal * 0.5}

  val data = List(Vector.fill(n)(1.0), x, x1).transpose zip y
  data take 10 foreach println

  // write model for linear regression
  val model = for {
    b0 <- Normal(0.0, 5.0).param
    b1 <- Normal(0.0, 5.0).param
    b2 <- Normal(0.0, 5.0).param
    sigma <- Gamma(2.0, 10.0).param
    _ <- Predictor.fromDoubleVector { x =>
      {
        val mean = b0 + b1 * x.head + b2 * x(1)
        Normal(mean, sigma)
      }
    }
    .fit(data)
  } yield Map("b0" -> b0, "b1" -> b1, "b2" -> b2, "sigma" -> sigma)

  val thin = 20

  val metropolis = model.sample(Metropolis, 5000, 10000 * thin, thin)
  val hmc = model.sample(HMC(5), 5000, 10000 * thin, thin)
  val nuts = model.sample(Nuts(10), 5000, 10000 * thin, thin)
  val ehmc = model.sample(Ehmc, 5000, 10000 * thin, thin)

  val headers = rfc.withHeader("beta0", "beta1", "beta2", "sigma")
  val outmh = new java.io.File("data/metropolis_lm.csv")
  val outhmc = new java.io.File("data/hmc_lm.csv")
  val outnuts = new java.io.File("data/nuts_lm.csv")
  val outehmc = new java.io.File("data/ehmc_lm.csv")

  outmh.writeCsv(metropolis.map(_.values), headers)
  outhmc.writeCsv(hmc.map(_.values), headers)
  outnuts.writeCsv(nuts.map(_.values), headers)
  outehmc.writeCsv(ehmc.map(_.values), headers)
}

