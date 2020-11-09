import scala.language.higherKinds

package object utils {

  sealed trait HList extends Product with Serializable

  final case class ::[H, T <: HList](head: H, tail: T) extends HList
  sealed trait HNil extends HList
  case object HNil extends HNil {
    def ::[H](t: H): H :: HNil = utils.::(t, this)
  }

  sealed trait ArgBuilder extends Product with Serializable {
    type F[Res]
    def apply[Res]: F[Res] => Res
  }

  final case class add[H, T <: ArgBuilder](head: H, tail: T) extends ArgBuilder {
    type F[Res] = H => (tail.type)#F[Res]

    def apply[Res]: F[Res] => Res = f => tail.apply(f(head))
  }

  sealed trait ArgNil extends ArgBuilder{
  }

  case object ArgNil extends ArgNil {
    def add[H](t: H): H add ArgNil = utils.add(t, ArgNil)

    type F[Res] = Res

    def apply[Res]: F[Res] => Res = identity
  }

  implicit class Utils[T <: HList](h: T) {

    def ::[H](t: H): H :: T = utils.::(t, h)
  }

  implicit class Utils2[T <: ArgBuilder](h: T) {

    def add[H](t: H): H add T = utils.add(t, h)
  }
  //  trait Ops {
  //  }
}
