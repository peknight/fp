package com.peknight.fp.streamingio

case class T[I, I2]() {
  sealed trait f[X] { def get: Either[I => X, I2 => X] }
  val L = new f[I] {def get = Left(identity)}
  val R = new f[I2] {def get = Right(identity)}
}
