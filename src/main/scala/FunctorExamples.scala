import cats.Functor
import cats.implicits._

object FunctorExamples {

  def tupleIt[F[_], T](v: F[T])(implicit F: Functor[F]): F[(T,T)] = {
    F.map(v)(x => (x,x))
  }

  // using context bound
  def tupleIt2[F[_] : Functor, T](v: F[T]): F[(T,T)] = {
    implicitly[Functor[F]].map(v)(x => (x,x))
  }

  // using extension methods
  def tupleIt3[F[_], T](v: F[T])(implicit F: Functor[F]): F[(T,T)] = {
    v.map(x => (x,x))
  }

}
