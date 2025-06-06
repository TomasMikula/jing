package jing

import scala.quoted.Quotes

package object macroUtil {

  transparent inline def qr(using q: Quotes): q.reflect.type =
    q.reflect

}
