package com.prosapient

import scala.collection.mutable

/**
  * Created by Marc-André Lamothe on 30/11/16.
  */
package object epress {

  private[epress] case class RenderingData(
                                             var iterationIndex: Long,
                                             parameters: Map[String, String],
                                             variables: mutable.Map[String, Either[Double, String]]
                                           )

}
