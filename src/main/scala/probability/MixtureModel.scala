package probability

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import com.stripe.rainier.cats._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.exp
import breeze.stats.distributions.{Gaussian, Multinomial, Rand}
import cats._
import cats.implicits._
import kantan.csv._
import kantan.csv.ops._

object MixtureModel extends App {
  implicit val rng = ScalaRNG(2)
  // simulate data from a mixture model

  val theta = DenseVector(0.3, 0.2, 0.5)
  val mu = Vector(-2.0, 1.0, 3.0)
  val n = 100

  def sim(theta: DenseVector[Double], sigma: Double,
          mu: Vector[Double]): Rand[Vector[Double]] = for {
    i <- Multinomial(theta)
    y <- Gaussian(mu(i), sigma)
  } yield Vector(y, i.toDouble, mu(i))

  val sims = sim(theta, 0.5, mu).sample(n)

  val head = rfc.withHeader("y", "index", "mu")
  val out = new java.io.File("data/mixture_model.csv")
  out.writeCsv(sims, head)

  def softmax(alpha: Seq[Real]) = {
    val total = alpha.map(_.exp).reduce(_ + _)
    alpha.map(a => a.exp / total)
  }

  val ys = sims.map(_.head)

  val model = for {
    theta1 <- Beta(2.0, 5.0).param
    theta2 <- Beta(2.0, 5.0).param
    alphas = Seq(theta1.log, theta2.log, Real.zero)
    // thetas = Seq(Real(0.3), Real(0.2), Real(0.5))
    thetas = softmax(alphas)
    mu1 <- Normal(0.0, 5.0).param
    mu2 <- Normal(0.0, 5.0).param
    mu3 <- Normal(0.0, 5.0).param
    mus = Seq(mu1, mu2, mu3)
    sigma <- Gamma(2.0, 10.0).param
    components: Map[Continuous, Real] = mus.zip(thetas).map {
      case (m: Real, t: Real) => (Normal(m, sigma) -> t) }.toMap
    _ <- Mixture(components).fit(ys)
  } yield Map("theta1" -> thetas.head, "theta2" -> thetas(1),
              "theta3" -> thetas(2),
                "mu1" -> mu1, "mu2" -> mu2, "mu3" -> mu3, "sigma" -> sigma)

  val thin = 20
  val ehmc = model.sample(HMC(10), 5000, 1000 * thin, thin)
  val headers = rfc.withHeader("theta1", "theta2", "theta3", "mu1", "mu2", "mu3", "sigma")
  val outehmc = new java.io.File("data/ehmc_mixture.csv")

  // Maps don't preserve order so we have to do this
  val params: List[Vector[Double]] = ehmc.map(x => Vector(x.get("theta1"), x.get("theta2"), x.get("theta3"), x.get("mu1"), x.get("mu2"), x.get("mu3"), x.get("sigma")).flatten)
  outehmc.writeCsv(params, headers)
}
