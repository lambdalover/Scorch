// Copyright (c) 2012, David Jeffery
// All rights reserved.

package scorch

import scalaz._
import Scalaz._

object Pimpz {
  class BindOps[M[_]:Bind,A](p:M[A]) {
      // I would call this '>>=' rather than '>>- to be consistent with
      // Haskell but this casuses some problems as operators ending in '='
      // are treated specially in Scala.
      // (would =>> be a better operator name than >>-?)
    def >>-[B](f:A=>M[B]):M[B] = implicitly[Bind[M]].bind(p,f)
    def >>[B](q:M[B]):M[B] = p >>- (_=>q) 
  }
  implicit def BindOps[M[_]:Bind,A](p:M[A]):BindOps[M,A] = new BindOps[M,A](p)
}

object ContinuationMonad {
  type Continuation[K,A] = (A=>K)=>K
  implicit def ContinuationM[K]:Monad[({type l[A] = Continuation[K,A]})#l] = 
  		new Monad[({type l[A] = Continuation[K,A]})#l] {
    override def pure[A](a: => A) = (f => f(a))
    override def bind[A, B](p: Continuation[K,A], h:A=>Continuation[K,B]) = 
    	bk => p(a => h(a)(bk))
  }
  def callCC[K,A,B](f:(A=>Continuation[K,B])=>Continuation[K,A]):Continuation[K,A] = 
  	k => f(a => (_ => k(a)))(k)
}

