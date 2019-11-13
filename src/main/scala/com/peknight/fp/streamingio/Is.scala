package com.peknight.fp.streamingio

case class Is[I]() {
  sealed trait f[X]
  val Get = new f[I] {}
}
