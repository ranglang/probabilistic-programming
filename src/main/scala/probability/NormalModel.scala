package probability

import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import kantan.csv._
import kantan.csv.ops._

object NormalModel extends App {
  implicit val rng = ScalaRNG(200)

  val data = Seq.fill(100)(4.0 + 0.75 * scala.util.Random.nextGaussian())

  val model: RandomVariable[Map[String, Real]] = for {
    mu <- Normal(0.0, 0.5).param
    sigma <- Gamma(2.0, 0.5).param
    _ <- Normal(mu, sigma).fit(data)
  } yield Map("mu" -> mu, "sigma" -> sigma)

  val iters = model.sample(Ehmc, 1000, 10000, 1)

  val out = new java.io.File("ehmc_normal.csv")
  val headers = rfc.withHeader("mu", "sigma")

  out.writeCsv(iters.map(_.values), headers)
}
