package probability

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import com.stripe.rainier.cats._
import java.nio.file.Paths
import cats.implicits._
import kantan.csv._
import kantan.csv.ops._
import breeze.stats.distributions.{Poisson => BreezePoisson}

object SimulatePoisson extends App {
  case class Parameters(phi: Double, mu: Double, sigma: Double)

  def ouStep(p: Parameters)(x: Double, dt: Double) = {
    val mean = p.mu + math.exp(-p.phi * dt) * (x - p.mu)
    val variance = (math.pow(p.sigma, 2) * (1 - math.exp(-2*p.phi*dt))) / (2*p.phi)
    mean + math.sqrt(variance) * scala.util.Random.nextGaussian()
  }

  def simulate(p: Parameters,
               ts: Stream[Double]): Stream[(Double, Double, Int)] = {
    val x0 = scala.util.Random.nextGaussian()
    val y0 = BreezePoisson(math.exp(x0)).draw
    val t0 = ts.head
    ts.tail.scanLeft((t0, x0, y0)){ case ((t0, x, _), t) =>
      val dt = t - t0
      val x1 = ouStep(p)(x, dt)
      val y = BreezePoisson(math.exp(x1)).draw
      (t, x1, y)
    }
  }

  val p = Parameters(0.4, 2.0, 0.5)
  val ts = Stream.iterate(0.0)(t => t + scala.util.Random.nextDouble() * 3.0)
  val ys = simulate(p, ts).take(1000).toVector

  val out = new java.io.File("data/poisson.csv")
  val headers = rfc.withHeader("time", "x", "y")

  out.writeCsv(ys, headers)
}

object PoissonPomp extends App {
  implicit val rng = ScalaRNG(500)

  val rawData = Paths.get("data/poisson.csv")
  val reader = rawData.asCsvReader[(Double, Double, Int)](rfc.withHeader)
  val ys: Vector[(Double, Int)] = reader.
    collect {
      case Right(a) => (a._1, a._3)
    }.
    toVector.
    tail

  val prior = for {
    phi <- Beta(2.0, 8.0).param
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
           y: (Double, Int)) = {
    val (t, phi, mu, sigma, x0) = st
    val dt = y._1 - t

    for {
      x1 <- ouStepReal(phi, mu, sigma, x0, dt).param
      lambda = x1.exp
      _ <- Poisson(lambda).fit(y._2)
    } yield (t + dt, phi, mu, sigma, x1)
  }

  val fullModel = prior flatMap (p => ys.foldM(p)(step))

  val model = for {
    ps <- fullModel
  } yield
      Map("phi" -> ps._2, "mu" -> ps._3, "sigma" -> ps._4)

  val thin = 20

  val iters = model.sample(Ehmc, 10000, 1000 * thin, thin)

  val out = new java.io.File("hmc_poisson.csv")
  val headers = rfc.withHeader("phi", "mu", "sigma")

  out.writeCsv(iters.map(_.values), headers)
}
