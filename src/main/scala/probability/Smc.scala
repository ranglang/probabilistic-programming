package probability

import cats._
import cats.implicits._
import breeze.stats.distributions.{Multinomial, Gaussian}
import breeze.linalg.{DenseVector, linspace}
import kantan.csv._
import kantan.csv.ops._

class Smc[A](val cloud: Vector[A]) { self =>
  val n = 1000

  def condition(ll: A => Double): Smc[A] = {
    val ws = cloud.map(ll).map(math.exp)
    val indices = Smc.resample(ws)
    println(s"Current state size ${ws.size}")
    new Smc[A](indices map (cloud(_)))
  }

  def flatMap[B](f: A => Smc[B]): Smc[B] = {
    val sms = Smc.thin(1000, self.cloud).flatMap(a => f(a).cloud)
    new Smc[B](sms)
  }

  def pure(a: A): Smc[A] =
    Smc(10, a)
}

object Smc {
  def apply[A](n: Int, a: => A): Smc[A] =
    new Smc[A](Vector.fill(n)(a))


  def thin[A](n: Int, v: Vector[A]): Vector[A] = {
    v.zipWithIndex.
      filter { case (_, i) => i % n == 0 }.
      map(_._1)
  }

  implicit val smcMonad = new Monad[Smc] {
    def flatMap[A, B](fa: Smc[A])(f: A => Smc[B]): Smc[B] = fa.flatMap(f)

    def pure[A](a: A): Smc[A] = Smc(1000, a)

    def tailRecM[A, B](a: A)(f: A => Smc[Either[A,B]]): Smc[B] = ???
    //   f(a).cloud.sequence match {
    //     case Right(b) => Smc(b)
    //     case Left(b) => tailRecM(b)(f)
    //   }
    // }
  }

  def resample(w: Vector[Double]): Vector[Int] = {
    val n = w.size
    Multinomial(DenseVector(w.toArray)).
      sample(n).
      toVector
  }

  def normal(n: Int)(mu: Double, sigma: Double): Smc[Double] =
    Smc(n, Dist.normal(mu, sigma).draw)
}

object Filter extends App {
  case class Parameters(phi: Double, mu: Double, sigma: Double)
  val p = Parameters(0.8, 1.0, 0.5)
  val arStep = (a0: Double) =>
  p.mu + p.phi * (a0 - p.mu) + p.sigma * scala.util.Random.nextGaussian()
  val state = Stream.iterate(p.mu)(arStep)
  val ar1 = state.
    map(a0 => Gaussian(a0, 0.5).draw).
    zipWithIndex.
    map { case (y, t) => (t, y) }.
    take(1000).
    toVector

  def kernel(d: Smc[Double], y: (Int, Double)): Smc[Double] = {
    val x = d flatMap (a => Smc.normal(10)(p.mu + p.phi * (a - p.mu), p.sigma))
    x condition (a => Gaussian(a, 0.5).logPdf(y._2))
  }

  def pFilter(p: Parameters,
              ys: Vector[(Int, Double)]): Vector[Smc[Double]] = {

    val init = Smc.normal(100)(p.mu, p.sigma / math.sqrt((1 - p.phi * p.phi)))
    ys.scanLeft(init)(kernel)
  }

  val filtered = pFilter(p, ar1)
  val meanState = filtered.
    map(a => breeze.stats.mean(a.cloud))

  def intervals(p: Double, x: Vector[Double]): (Double, Double) = {
    val n = x.size
    val upper = math.floor(p * n).toInt
    val lower = n - upper
    val ord = x.sorted
    (ord(lower), ord(upper))
  }

  val out = new java.io.File("smc_output.csv")
  val headers = rfc.withHeader("time", "state", "observation", "mean", "upper", "lower")
  val ts = Vector.range(1, 100)
  val intervals: Vector[(Double, Double)] = filtered.
    map(a => intervals(0.95, a.cloud))
  val data = (ar1 zip state zip meanState.tail zip intervals).
    map { case ((((t, y), x), f), (l, u)) => (t, y, x, f, l, u) }

  out.writeCsv(data, headers)
}
