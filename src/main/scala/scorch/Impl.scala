// Copyright (c) 2012, David Jeffery
// All rights reserved.

package scorch

import scalaz._
import Scalaz._

object ContinuationMonad {
  type Continuation[K,A] = (A=>K)=>K
  implicit def ContinuationM[K]:Monad[({type l[A] = Continuation[K,A]})#l] = 
  		new Monad[({type l[A] = Continuation[K,A]})#l] {
    override def pure[A](a: => A) = (f => f(a))
    override def bind[A, B](p: Continuation[K,A], h:A=>Continuation[K,B]) = 
    	bk => p(a => h(a)(bk))
  }
  def callCC[K, A, B](f: (A => Continuation[K, B]) => Continuation[K, A]): Continuation[K, A] =
    (k => f(a => (_ => k(a)))(k))
  
}

object Impl extends Scorch {
  def UNDEFINED[T]:T = error("UNDEFINED")

  import ContinuationMonad._

  type SC[A] = Continuation[Unit,A]
  implicit def SCMonad[A]:Monad[SC] = ContinuationM

      // why do we need the 'implicitly'?
  def result[A](a:A):SC[A] = implicitly[Monad[SC]].pure(a)
  def stop[A]:SC[A] = _ => result(())
  def eagerly[A](a:SC[A]):SC[SC[A]] = UNDEFINED
  def liftIO[A](a:IO[A]):SC[A] = UNDEFINED
  def runSC[A](a:SC[A]):IO[Unit] = UNDEFINED

  def par[A](p:SC[A], q:SC[A]):SC[A]= UNDEFINED
  def append[A](p:SC[A], q:SC[A]):SC[A]= UNDEFINED
    // Can delete this?
  def bind[A,B](p:SC[A], f:A=>SC[B]):SC[B]= implicitly[Monad[SC]].bind(p,f)

  // should get the following for free from MonadPlus/Applicative?
  def guard(b:Boolean):SC[Unit]= UNDEFINED

  def applySC[A,B](f:SC[A=>B], a:SC[A]):SC[B] = UNDEFINED

  def liftApply[A,B](f:A=>B, a:SC[A]):SC[B] = UNDEFINED

  type IO[A] = List[A]
  implicit def IOMonad[A]:Monad[IO] = UNDEFINED

  type MVar[A]= List[A]
  def newEmptyMVar[A]:IO[MVar[A]] = UNDEFINED
  def takeMVar[A](m:MVar[A]):IO[A] = UNDEFINED
  def putMVar[A](m:MVar[A], a:A):IO[Unit] = UNDEFINED
  def tryPutMVar[A](m:MVar[A], a:A):IO[Unit] = UNDEFINED
  def tryTakeMVar[A](m:MVar[A]):IO[Option[A]] = UNDEFINED

  type TVar[A] = List[A]
  def newTVar[A](a:A):IO[TVar[A]] = UNDEFINED
  def modifyTVar[A](t:TVar[A],f:A=>A):IO[(A,A)] = UNDEFINED
  
  def putStrLn(s:String):IO[Unit] = UNDEFINED

  def threadDelay(d:Double):IO[Unit] = UNDEFINED

}
