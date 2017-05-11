import cats.Functor

object FunctorExamples {

  def tupleIt[F[_], T](v: F[T])(implicit F: Functor[F]): F[(T,T)] = {
    F.map(v)(x => (x,x))
  }

  def tupleIt2[F[_] : Functor, T](v: F[T]): F[(T,T)] = {
    implicitly[Functor[F]].map(v)(x => (x,x))
  }

}
