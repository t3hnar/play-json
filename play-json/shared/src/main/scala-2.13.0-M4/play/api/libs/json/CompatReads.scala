/*
 * Copyright (C) 2009-2018 Lightbend Inc. <https://www.lightbend.com>
 */

package play.api.libs.json

import scala.language.higherKinds

import scala.collection.Factory
import scala.collection.{ IndexedSeq, Seq }

private[json] trait CompatReads { _: LowPriorityDefaultReads =>
  /**
   * Generic deserializer for collections types.
   */
  implicit def traversableReads[F[_], A](implicit f: Factory[A, F[A]], ra: Reads[A]): Reads[F[A]] = new Reads[F[A]] {
    def reads(json: JsValue): JsResult[F[A]] = json match {
      case JsArray(ts) => {
        type Errors = Seq[(JsPath, Seq[JsonValidationError])]

        def locate(e: Errors, idx: Int) = e.map {
          case (p, valerr) => (JsPath(idx)) ++ p -> valerr
        }

        val builder = f.newBuilder
        builder.sizeHint(ts.size)

        @annotation.tailrec
        def go(idx: Int, js: IndexedSeq[JsValue]): JsResult[F[A]] =
          js.headOption match {
            case Some(elt) => ra.reads(elt) match {
              case JsSuccess(v, _) => {
                builder += v
                go(idx + 1, js.tail)
              }

              case JsError(e) => JsError(locate(e, idx))
            }

            case _ => JsSuccess(builder.result())
          }

        go(0, ts)
      }

      case _ => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsarray"))))
    }
  }

}
