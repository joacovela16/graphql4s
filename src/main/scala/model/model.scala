
import monix.reactive.Observable

import scala.reflect.ClassTag

package object model {
  type TargetFormat = String
  type QueryParams = Map[String, String]
  type Body = Option[String]



  final case class Issue(code: String, message: String)







}
