// package probability

// import breeze.stats.distributions.{Rand, RandBasis}
// import breeze.linalg.DenseVector
// import cats._
// import cats.free.Free
// import breeze.plot._

// object Probability {
//   sealed trait DistF[A]
//   case class FlatMap[A, B](d: Dist[A], f: A => Dist[B]) extends DistF[B]
//   case class Pure[A](a: A) extends DistF[A]
//   case class Condition[A](d: Dist[A], ll: A => Double) extends DistF[A]
//   case class Draw[F[_], A](fa: F[A], r: RandBasis) extends DistF[A]

//   // A free monad
//   type Dist[A] = Free[DistF, A]

//   // lift the functions into the free monad
//   def flatMap[A, B](d: Dist[A], f: A => Dist[B]): Dist[B] =
//     Free.liftF(FlatMap(d, f))
//   def pure[A](a: A): Dist[A] =
//     Free.liftF(Pure(a))
//   def condition[A](d: Dist[A], ll: A => Double): Dist[A] =
//     Free.liftF(Condition(d, ll))
//   def draw[F[_], A](fa: F[A], r: RandBasis): Dist[A] =
//     Free.liftF(Draw(fa, r))

//   def map[A, B](d: Dist[A], f: A => B): Dist[B] =
//     flatMap(d, (a: A) => pure(f(a)))

//   def uniform: Dist[Double] =
//     draw(Rand.uniform, Rand)

//   def bernoulli(p: Double): Dist[Boolean] =
//     uniform.map(_ < p)

//   // We can derive the binomial distribution from the bernoulli distribution
//   def binomial(n: Int, p: Double): Dist[Int] = {
//     Monad[Dist].replicateA(n, bernoulli(p)).
//       map(_.filter(c => c).size)
//   }

//   // an interpreter for the program, using the rand probability monad
//   def sample: (DistF ~> Rand) = new (DistF ~> Rand) {

//     def apply[A](fa: DistF[A]): Rand[A] = fa match {
//       case FlatMap(d, f) => f(apply(d))
//       case Pure(a) => Rand.always(a)
//       case d: Draw[f, a] =>
//         val fa: f[a] = d.fa
//         implicit val sampleable: Sampleable[f] = d.sampleable
//         fa.sample(randomness)
//       }
//     }

//   val program: Dist[Int] = for {
//     p <- beta(1, 1)
//     ys <- binomial(10, p)
//   } yield ys

//   def main(args: Array[String]) = {
//     println("This is how many heads out of ten coin flips")
//     println(program.foldMap(sample).draw)
//   }
// }
