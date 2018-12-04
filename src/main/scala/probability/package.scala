import breeze.stats.distributions._
import breeze.stats.distributions.Rand._
import cats.Monad
import com.stripe.rainier.compute._
import com.stripe.rainier.core._
import com.stripe.rainier.sampler._
import scala.annotation.tailrec

package object probability {
  type LogLikelihood = Double

  // define rand as a cats monad
  implicit val randMonad = new Monad[Rand] {
    def flatMap[A, B](fa: Rand[A])(f: A => Rand[B]): Rand[B] = {
      fa.flatMap(f)
    }

    def pure[A](a: A): Rand[A] = always(a)

    def tailRecM[A, B](a: A)(f: A => Rand[Either[A, B]]): Rand[B] = {
      f(a).draw match {
        case Right(b) => always(b)
        case Left(b) => tailRecM(b)(f)
      }
    }
  }
}
