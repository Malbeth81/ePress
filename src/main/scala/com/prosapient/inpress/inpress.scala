package com.prosapient

import scala.collection.mutable

/**
  * Created by Marc-André Lamothe on 30/11/16.
  */
package object inpress {

  private[inpress] case class RenderingData(
                                             var iterationIndex: Long,
                                             parameters: Map[String, String],
                                             variables: mutable.Map[String, Either[Double, String]]
                                           )

}
