package scorch

import scalaz._
import Scalaz._

object Impl extends Scorch {
  type SC[A] = List[A]
  implicit def SCMonad[A]:Monad[SC] = UNDEFINED

  def UNDEFINED[T]:T = error("NYI")

      // result already defined in Monad[_]?
  def result[A](a:A):SC[A] = UNDEFINED
  def stop[A]:SC[A] = UNDEFINED
  def eagerly[A](a:SC[A]):SC[SC[A]] = UNDEFINED
  def liftIO[A](a:IO[A]):SC[A] = UNDEFINED
  def runSC[A](a:SC[A]):IO[Unit] = UNDEFINED

  def par[A](p:SC[A], q:SC[A]):SC[A]= UNDEFINED
  def append[A](p:SC[A], q:SC[A]):SC[A]= UNDEFINED
    // >>= already defined in Monad[_]?
  def bind[A,B](p:SC[A], f:A=>SC[B]):SC[B]= UNDEFINED

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
