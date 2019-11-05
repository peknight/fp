package com.peknight.fp.iomonad

trait Source {
  def readBytes(numBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit
}
