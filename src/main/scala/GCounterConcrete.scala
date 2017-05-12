import cats.Monoid
import cats.kernel.CommutativeMonoid
import cats.implicits._

trait IdempotentCommutativeMonoid[A] extends CommutativeMonoid[A]

case class GCounterConcrete[Elt](counters: Map[Int, Elt]) {
  def increment(id: Int, amount: Elt)(
      implicit m: CommutativeMonoid[Elt]): GCounterConcrete[Elt] = {
    val current = counters.get(id).getOrElse(m.empty)
    val updated = m.combine(current, amount)

    GCounterConcrete(counters + (id -> updated))
  }

  def total(implicit m: CommutativeMonoid[Elt]): Elt = {
    m.combineAll(counters.values)
  }

  def merge(other: GCounterConcrete[Elt])(
      implicit m: IdempotentCommutativeMonoid[Elt]): GCounterConcrete[Elt] =
    GCounterConcrete(counters |+| other.counters)
}

// in a typeclass you explicitly pass what would be the `this` parameter
trait GCounter[F[_], Elt] {
  def increment(f: F[Elt])(id: Int, amount: Elt): F[Elt]

  def total(f: F[Elt]): Elt

  def merge(f1: F[Elt], f2: F[Elt]): F[Elt]
}

object GCounter {

  type IntMap[A] = Map[Int, A]

  implicit def mapIcmInstance[Elt](
      implicit m: IdempotentCommutativeMonoid[Elt])
    : IdempotentCommutativeMonoid[IntMap[Elt]] = {
    val mapInstance = implicitly[Monoid[IntMap[Elt]]]
    new IdempotentCommutativeMonoid[IntMap[Elt]] {
      def empty: IntMap[Elt] = mapInstance.empty

      def combine(x: IntMap[Elt], y: IntMap[Elt]): IntMap[Elt] =
        mapInstance.combine(x, y)
    }

  }

  /*
   *  How to add dependencies on Monoids?
   *  1. to add to type class definition: trait GCounter[F[_], Elt : IdempotentCommutativeMonoid : CommutativeMonoid]
   *  2. add dependencies to def that defines instance as implicit params (below)
   */

  def mapGCounterInstance[Elt](implicit cm: CommutativeMonoid[Elt],
                               icm: IdempotentCommutativeMonoid[Elt]) =
    new GCounter[IntMap, Elt] {

      def increment(f: IntMap[Elt])(id: Int, amount: Elt): IntMap[Elt] = {
        val current = f.get(id).getOrElse(cm.empty)
        val updated = cm.combine(current, amount)

        f + (id -> updated)
      }

      def total(f: IntMap[Elt]): Elt =
        cm.combineAll(f.values)

      def merge(f1: IntMap[Elt], f2: IntMap[Elt]): IntMap[Elt] =
        implicitly[IdempotentCommutativeMonoid[IntMap[Elt]]].combine(f1, f2)
    }

}
