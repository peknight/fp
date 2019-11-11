package com.peknight.fp.localeffects

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}
