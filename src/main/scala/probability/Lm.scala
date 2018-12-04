package probability

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import breeze.linalg.{DenseMatrix, DenseVector, csvwrite, csvread}
import breeze.numerics.exp
import breeze.stats.distributions.{Gaussian, Poisson => BreezePoisson}
import cats.implicits._
import kantan.csv._
import kantan.csv.ops._

object Lm extends App {
  val n = 1000
  val p = 2
  val x = Vector.tabulate(p + 1) { j => Vector.tabulate(n)(i =>
    if (j == 0) 1.0 else if (j == 1) Gaussian(0.0, 1.0).draw else Gaussian(0.0, 1.0).draw )}

  val a = DenseVector(4.0, -1.5, 1.5)

  val xm = new DenseMatrix(n, p + 1, x.flatten.toArray)
  val mean: DenseVector[Double] = (a.t * xm.t).t
  val y = mean map (xi => Gaussian(xi, 0.5).draw)

  csvwrite(new java.io.File("lm_sims.csv"),
           DenseMatrix.horzcat(xm, y.toDenseMatrix.t))

  implicit val rng = ScalaRNG(2)

  // write model for linear regression
  val model = for {
    b0 <- Normal(0.0, 5.0).param
    b1 <- Normal(0.0, 5.0).param
    b2 <- Normal(0.0, 5.0).param
    sigma <- Gamma(2.0, 2.0).param
    _ <- Predictor.fromDoubleVector { xs =>
      {
        val mean = b0 + b1 * xs(2) + b2 * xs(3)
        Normal(mean, sigma)
      }
    }
    .fit(x zip y.data.toVector)
  } yield Map("b0" -> b0, "b1" -> b1, "b2" -> b2, "sigma" -> sigma)

  val thin = 20

  val metropolis = model.sample(Metropolis, 5000, 10000 * thin, thin)
  val hmc = model.sample(HMC(5), 5000, 10000 * thin, thin)
  val nuts = model.sample(Nuts(10), 5000, 10000 * thin, thin)
  val ehmc = model.sample(Ehmc, 5000, 10000 * thin, thin)

  val headers = rfc.withHeader("beta0", "beta1", "beta2", "sigma")
  val outmh = new java.io.File("metropolis_lm.csv")
  val outhmc = new java.io.File("hmc_lm.csv")
  val outnuts = new java.io.File("nuts_lm.csv")
  val outehmc = new java.io.File("ehmc_lm.csv")

  outmh.writeCsv(metropolis.map(_.values), headers)
  outhmc.writeCsv(hmc.map(_.values), headers)
  outnuts.writeCsv(nuts.map(_.values), headers)
  outehmc.writeCsv(ehmc.map(_.values), headers)
}

