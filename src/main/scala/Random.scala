object RandomExamples {

  /*
   *  creating random instances out of nothing
   *  map: Random[A] (A => B)
   *  flatMap: Random[A], A => Random[B], Random[B]
   *  pure A: Random[A]
   *
   *  // elimination form
   *  run: Random[A] scala.util.Random => A
   *  int: Random[Int]
   *  double: Random[Double]
   */

  sealed trait Random[A] {

    import Random._

    // reification (turning behaviour into data)
    def map[B](f: A => B): Random[B] = Map(this, f)
    def flatMap[B](f: A => Random[B]): Random[B] = FlatMap(this, f)
    def run(rng: util.Random): A = {

      // method introduced because Double.type was inferred rather than Random[Double]...
      // this is problem with generalized algebraic data types
      def generate[B](random: Random[B]): B =
        random match {
          case Map(s, f) => f(s.run(rng))
          case FlatMap(s, f) => f(s.run(rng)).run(rng)
          case Int => rng.nextInt()
          case Double => rng.nextDouble()
          case Pure(a) => a
        }
      generate(this)
    }

  }

  object Random {
    def int: Random[Int] = Int
    def double: Random[Double] = Double
    def pure[A](a: A): Random[A] = Pure(a)

    // composition
    def boolean: Random[Boolean] = int.map(_ % 0 == 0)

    def points = ??? // use applicative for it

    final case class Map[A, B](source: Random[A], f: A => B) extends Random[B]
    final case class FlatMap[A, B](source: Random[A], f: A => Random[B])
        extends Random[B]
    final case object Int extends Random[Int]
    final case object Double extends Random[Double]
    final case class Pure[A](a: A) extends Random[A]
  }
}

/*
 *  as Free monad
 *  type Random[A] = Free[RandomOp, A]
 *
 *  RandomOp would be Int, Double from above
 */
