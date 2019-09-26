package com.peknight.fp.fpinscala.state

sealed trait Input
case object Coin extends Input
case object Turn extends Input
