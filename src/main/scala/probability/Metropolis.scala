package probability

import cats._
import cats.implicits._
import breeze.numerics.{exp, log}
import kantan.csv._
import kantan.csv.ops._
import com.stripe.rainier.core._
import com.stripe.rainier.compute._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import com.stripe.rainier.sampler._

case object Metropolis extends Sampler {
  def sample(density: DensityFunction,
             warmupIterations: Int,
             iterations: Int,
             keepEvery: Int)(implicit rng: RNG): List[Array[Double]] = {

    val nVars = density.nVars

    val init: Array[Double] =
      Array.fill(nVars)(rng.standardNormal)

    val pos: Array[Double] => Double = {
      input => density.update(input)
      density.density
    }

    val prop: Array[Double] => Array[Double] = ps => {
      ps map (p => p + scala.util.Random.nextGaussian() * 0.05)
    }

    def step: Array[Double] => Array[Double] = { p: Array[Double] =>
      val ps = prop(p)
      val a = pos(ps) - pos(p)
      val u = rng.standardNormal

      if (log(u) < a) ps else p
    }

    val buf = new ListBuffer[Array[Double]]
    var i = 0
    var current = init
    while (i < iterations) {
      current = step(current)
      if (i % keepEvery == 0)
        buf += current
      i += 1
    }
    buf.toList
  }
}

object MetropolisFitting extends App {
  implicit val rng = ScalaRNG(2)

  val model = for {
    p <- Beta(3, 3).param
    _ <- Binomial(p, 100).fit(60)
  } yield p

  val iters = model.sample(Metropolis, 1000, 10000, 5)

  val out = new java.io.File("metropolis_coin.csv")
  val headers = rfc.withHeader("p")

  out.writeCsv(iters, headers)
}
