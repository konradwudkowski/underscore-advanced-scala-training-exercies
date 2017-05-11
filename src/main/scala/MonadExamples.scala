import cats.Monad

object MonadExamples {

  // Define monad for Function1[A,B] or A => B

  type MyFunction[T] = String => T
  object MyFunction1 {
    def flatMap[A, B](fa: MyFunction[A])(f: A => MyFunction[B]): MyFunction[B] =
      {

//        (input: String) => {
//        val a = fa.apply(input)
//        val fb = f(a)
//        fb.apply(input)
//      }

        (input: String) => f(fa(input))(input)
      }

    def pure[A](a: A): MyFunction[A] = _ => a
  }

}

object Main extends App {

  import MonadExamples.MyFunction1._

  val f = flatMap( (i: String) => i.length)( (x: Int) => (i: String) => if(x % 2 == 0) s"yeah! $i" else "nah" )

  assert( f("hello") == "nah" )

  assert( f("hello!") == "yeah! hello!" )

}

