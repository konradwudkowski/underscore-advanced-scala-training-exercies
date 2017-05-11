import org.scalatest.{Matchers, WordSpec}
import cats.implicits._

class FunctorExamplesSpec extends WordSpec with Matchers {

  import FunctorExamples._

  "tupleIt" should {
    "return tupled values for any Functor" in {
      tupleIt(Option(1)) shouldBe Some((1,1))
    }
  }

  "tupleIt2 (using context bound)"  should {
    "return tupled values for any Functor" in {
      tupleIt2(Option(1)) shouldBe Some((1,1))
    }
  }

}
