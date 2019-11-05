package com.peknight.fp.iomonad

import com.peknight.fp.monad.Monad

case class ConsoleState[A](run: Buffers => (A, Buffers)) {

}
object ConsoleState {
  import scala.language.implicitConversions
  implicit val monad: Monad[ConsoleState] = ???
}
