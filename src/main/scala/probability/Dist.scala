package probability

import cats._, cats.implicits._
import breeze.stats.distributions.{Beta => BreezeBeta}
import scala.annotation.tailrec
import breeze.numerics.exp

trait Dist[A] { self =>
  private val N = 10000

  def draw(): A

  def sample(n: Int): Vector[A] =
    Vector.fill(n)(draw())

  def condition(c: A => Boolean): Dist[A] = new Dist[A] {
    @tailrec
    override def draw = {
      val a = self.draw
      if (c(a)) a else this.draw
    }
  }

  def probabilityOf(pred: A => Boolean, samples: Int = N): Double =
    self.sample(samples).filter(pred).size.toDouble / samples
}

object Dist {
  def always[A](a: A): Dist[A] = new Dist[A] {
    def draw = a
  }

  implicit val distMonad = new Monad[Dist] {
    def pure[A](a: A) = Dist.always(a)

    override def map[A, B](d: Dist[A])(f: A => B): Dist[B] =  new Dist[B] {
      override def draw = f(d.draw)
    }

    def flatMap[A, B](d: Dist[A])(f: A => Dist[B]): Dist[B] = new Dist[B] {
      override def draw = f(d.draw).draw
    }

    def tailRecM[A, B](a: A)(f: A => Dist[Either[A, B]]) = {
      f(a).draw match {
        case Right(b) => Dist.always(b)
        case Left(b) => tailRecM(b)(f)
      }
    }
  }

  def beta(a: Double, b: Double): Dist[Double] = new Dist[Double] {
    val beta = new BreezeBeta(a, b)
    def draw = beta.draw
  }

  def uniform(min: Double, max: Double): Dist[Double] = new Dist[Double] {
    def draw = scala.util.Random.nextDouble() * (max - min) + min
  }

  def bernoulli(p: Double): Dist[Boolean] =
    uniform(0, 1).map(_ < p)

  def binomial(n: Int, p: Double): Dist[Int] =
    Applicative[Dist].replicateA(n, bernoulli(p)).
      map(_.filter(x => x).size)

  def normal(mu: Double, sigma: Double): Dist[Double] = new Dist[Double] {
    def draw = scala.util.Random.nextGaussian() * sigma + mu
  }
}

object Model {
  def main(args: Array[String]): Unit = {
    val a = 1
    val b = 1
    val n = 10
    val h = 6

    val program: Dist[(Double, Int)] = for {
      p <- Dist.beta(a, b)
      heads <- Dist.binomial(n, p)
    } yield (p, heads)

    val program1: Dist[(Double, Int)] = Dist.beta(a, b).flatMap(p =>
      Dist.binomial(n, p).map(y => (p, y)))

    val N = 10000
    val ps = program.condition(_._2 == h)
    ps.sample(10).foreach(println)
  }
}
