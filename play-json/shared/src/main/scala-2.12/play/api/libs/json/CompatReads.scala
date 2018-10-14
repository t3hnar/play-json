package play.api.libs.json

import scala.language.higherKinds

import scala.collection.generic

private[json] trait CompatReads { _: LowPriorityDefaultReads =>
  /**
   * Generic deserializer for collections types.
   */
  implicit def traversableReads[F[_], A](implicit bf: generic.CanBuildFrom[F[_], A, F[A]], ra: Reads[A]): Reads[F[A]] = new Reads[F[A]] {
    def reads(json: JsValue): JsResult[F[A]] = json match {
      case JsArray(ts) => {
        type Errors = Seq[(JsPath, Seq[JsonValidationError])]
        def locate(e: Errors, idx: Int) = e.map { case (p, valerr) => (JsPath(idx)) ++ p -> valerr }

        ts.iterator.zipWithIndex.foldLeft(Right(Vector.empty): Either[Errors, Vector[A]]) {
          case (acc, (elt, idx)) => (acc, ra.reads(elt)) match {
            case (Right(vs), JsSuccess(v, _)) => Right(vs :+ v)
            case (Right(_), JsError(e)) => Left(locate(e, idx))
            case (Left(e), JsSuccess(_, _)) => Left(e)
            case (Left(e1), JsError(e2)) => Left(e1 ++ locate(e2, idx))
          }
        }.fold(JsError.apply, { res =>
          val builder = bf()
          builder.sizeHint(res)
          builder ++= res
          JsSuccess(builder.result())
        })
      }

      case _ => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsarray"))))
    }
  }

}
