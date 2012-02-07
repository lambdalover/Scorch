// Copyright (c) 2012, David Jeffery
// All rights reserved.

package scorch

import scalaz._
import Scalaz._

  // XXX put this into a separate source file
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

object Impl extends Scorch {
  val io:IOModule = IOImpl
  import io._

  def UNDEFINED[T]:T = error("UNDEFINED")

  import ContinuationMonad._

  type SC[A] = Continuation[IO[Unit],A]
  implicit def SCMonad[A]:Monad[SC] = ContinuationM

    // why do we need the 'implicitly'?
  def result[A](a:A):SC[A] = implicitly[Monad[SC]].pure(a)
  def stop[A]:SC[A] = _ => doNothing
  def eagerly[A](p:SC[A]):SC[SC[A]] = UNDEFINED
  def liftIO[A](ioa:IO[A]):SC[A] = k => ioa >>= k
  def runSC[A](p:SC[A]):IO[Unit] = p(_ => doNothing)

    // helper fn
  def doNothing:IO[Unit] = implicitly[Monad[IO]].pure(())

  def par[A](p:SC[A], q:SC[A]):SC[A]= UNDEFINED
  def append[A](p:SC[A], q:SC[A]):SC[A]= UNDEFINED

    // should get this for free from MonadPlus/Applicative or somesuch?
  def guard(b:Boolean):SC[Unit]= if (b) result(()) else stop

  def applySC[A,B](fs:SC[A=>B], p:SC[A]):SC[B] = fs >>= (f => liftApply(f,p))

  def liftApply[A,B](f:A=>B, p:SC[A]):SC[B] = p >>= (a => result(f(a)))
}

object IOImpl extends IOModule {
  def UNDEFINED[T]:T = error("UNDEFINED")

  type IO[A] = (Unit=>A)
  implicit def IOMonad[A]:Monad[IO] = new Monad[IO] {
    override def pure[A](a: => A):IO[A] = _ => a
    override def bind[A,B](p:IO[A], h:A=>IO[B]):IO[B] = _ => h(p(()))(()) 
  }
  def unsafePerformIO[A](sideAffectingCode: =>A):IO[A] = 
  	implicitly[Monad[IO]].pure(sideAffectingCode)

  def toplevelRunIO[A](io:IO[A]):A = io(()) // XXX should run in separate actor?

  import scala.actors.Actor
  import scala.actors.Actor._

  type MVar[A]= {} // dummy
  def newEmptyMVar[A]:IO[MVar[A]] = UNDEFINED
  def takeMVar[A](m:MVar[A]):IO[A] = UNDEFINED
  def putMVar[A](m:MVar[A], a:A):IO[Unit] = UNDEFINED
  def tryPutMVar[A](m:MVar[A], a:A):IO[Unit] = UNDEFINED
  def tryTakeMVar[A](m:MVar[A]):IO[Option[A]] = UNDEFINED

  class TVar[A](a:A) extends Actor {
  	var st:A = a
	def act() {
		react {
			case f:(A=>A) => { 
				val oldst = st;
				st = f(a); 
				reply((oldst,st));
			}
		}
	}
	def modify(f:A=>A):(A,A) = (self !? f).asInstanceOf[(A,A)]
  }
  def newTVar[A](a:A):IO[TVar[A]] = unsafePerformIO(new TVar[A](a)) 
  def modifyTVar[A](t:TVar[A],f:A=>A):IO[(A,A)] = unsafePerformIO(t.modify(f))
  
  def threadDelay(w:Long):IO[Unit] = unsafePerformIO({
	val ac = new Actor {
		def act() {
			receive {
			 	case howlong:Long => { 
					java.lang.Thread.sleep(howlong); 
					reply(()) 
				}
			}
		}
	}
  	scala.concurrent.ops.spawn { ac.start() }
	ac !? w
	()
  })
}
