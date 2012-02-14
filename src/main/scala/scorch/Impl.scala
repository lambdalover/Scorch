// Copyright (c) 2012, David Jeffery
// All rights reserved.

package scorch

import scalaz._
import Scalaz._

import Pimpz._

object Impl extends Scorch {
  val io:IOModule = IOImpl
  import io._

  def UNDEFINED[T]:T = error("UNDEFINED")

  import ContinuationMonad._

  type SC[A] = Continuation[HIO[Unit],A]
  implicit def SCMonad[A]:Monad[SC] = ContinuationM
  implicit def SCLiftIO:LiftIO[SC] = new LiftIO[SC] {
    def liftIO[A](ioa:IO[A]):SC[A] = 
    		k => implicitly[LiftIO[HIO]].liftIO(ioa) >>- k
  }

    // why do we need the 'implicitly'?
  def result[A](a:A):SC[A] = implicitly[Monad[SC]].pure(a)
  def stop[A]:SC[A] = _ => liftIO_H(doNothing)
  def eagerly[A](p:SC[A]):SC[SC[A]] = k => for {
	res <- liftIO_H(newEmptyMVar[A])
	g <- newGroup
	_ <- local(g, forkActorH(saveOnce(p, res,g)))
	r <- k(k2 => liftIO_H(readMVar(res)) >>- k2)
  } yield r
  def saveOnce[A](p:SC[A], r:MVar[A], g:Group):HIO[Unit] = for {
	ticket <- liftIO_H(newMVar[Unit](()))
	_ <- p(x => (liftIO_H(tryTakeMVar(ticket) >>- 
			(t => (if (t == None) doNothing else putMVar(r,x) )))))
			// need to stop here instead of doNothing
			//(t => (if (t == None) stop else putMVar(r,x) ))))
  } yield ()
  
  
  def runSC[A](p:SC[A]):IO[Unit] = runHIO(p(_ => liftIO_H(doNothing))) 

    // helper fn
  def doNothing:IO[Unit] = implicitly[Monad[IO]].pure(())

  def par[A](p:SC[A], q:SC[A]):SC[A]= k => (forkActorH(p(k)) >> q(k))
  def append[A](p:SC[A], q:SC[A]):SC[A] = 
	k => for {
		g <- newGroup
		_ <- local(g, forkActorH(p(k)))
		_ <- finished(g)
		_ <- q(k)
	} yield ()

    // should get this for free from MonadPlus/Applicative or somesuch?
  def guard(b:Boolean):SC[Unit]= if (b) result(()) else stop

  def applySC[A,B](fs:SC[A=>B], p:SC[A]):SC[B] = fs >>- (f => liftApply(f,p))

  def liftApply[A,B](f:A=>B, p:SC[A]):SC[B] = p >>- (a => result(f(a)))
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

  import scala.actors.Actor
  import scala.actors.Actor._

  def toplevelRunIO[A](io:IO[A]):A = io(()) // XXX should run in separate actor?

  trait MVarInstruction[A]
  case class Put[A](a:A) extends MVarInstruction[A]
  case class Take[A] extends MVarInstruction[A]
  case class Read[A] extends MVarInstruction[A]
  case class TryPut[A](a:A) extends MVarInstruction[A]
  case class TryTake[A] extends MVarInstruction[A]

    // This isn't the cleverest implementation - MVars sitting on top of actors.
  class MVar[A] extends Actor {
  	var st:Option[A] = None
	def act() {
		loop { if (st == None) react(acceptPut) else react(acceptTake) }
	}
	def acceptPut:PartialFunction[Any,Unit] = {
		case Put(a) => { 
			st = Some[A](a.asInstanceOf[A])
			reply(())
		}
		case TryPut(a) => { 
			st = Some[A](a.asInstanceOf[A])
			reply(())
		}
		case TryTake => { 
			reply(None)
		}
	}
	def acceptTake:PartialFunction[Any,Unit] = {
		case Take => { 
			val oldst = st
			st = None
			reply(oldst.get) // accepting 'Take' so oldst!=None
		}
		case Read => {
			reply(st.get) // ditto
		}
		case TryPut(a) => { 
			reply(())
		}
		case TryTake => { 
			val oldst = st
			st = None
			reply(oldst)
		}
	}
	def putMVar[A](a:A):Unit = (self !? Put[A](a)).asInstanceOf[Unit]
	def takeMVar[A]:A = (self !? Take).asInstanceOf[A]
	def readMVar[A]:A = (self !? Read).asInstanceOf[A]
	def tryPutMVar[A](a:A):Unit = (self !? TryPut[A](a)).asInstanceOf[Unit]
	def tryTakeMVar[A]:Option[A] = (self !? TryTake).asInstanceOf[Option[A]]
  }
  def newEmptyMVar[A]:IO[MVar[A]] = unsafePerformIO(new MVar[A]())
  def putMVar[A](m:MVar[A], a:A):IO[Unit] = unsafePerformIO(m.putMVar(a))
  def takeMVar[A](m:MVar[A]):IO[A] = unsafePerformIO(m.takeMVar)
  def readMVar[A](m:MVar[A]):IO[A] = unsafePerformIO(m.readMVar)
  def tryPutMVar[A](m:MVar[A], a:A):IO[Unit] = unsafePerformIO(m.tryPutMVar(a))
  def tryTakeMVar[A](m:MVar[A]):IO[Option[A]] = unsafePerformIO(m.tryTakeMVar)

    // XXX exception handling needed here
  def atomic[A](m:MVar[Unit])(io:IO[A]):IO[A] = for {
  	_ <- takeMVar[Unit](m)
	a <- io
  	_ <- putMVar[Unit](m, ())
  } yield a

  class TVar[A](a:A) extends Actor {
  	var st:A = a
	def act() {
		loop { react {
			case f:(A=>A) => { 
				val oldst = st;
				st = f(a); 
				reply((oldst,st));
			}
		} }
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

  def forkActor(io:IO[Unit]):IO[Unit] =unsafePerformIO(actor(toplevelRunIO(io)))

  type HIO[A] = Group=>IO[A]
	// XXX this is just the environment Monad
  implicit def HIOMonad[A]:Monad[HIO] = new Monad[HIO] {
    override def pure[A](a: => A):HIO[A] = _ => result(a)
    override def bind[A,B](p:HIO[A], h:A=>HIO[B]):HIO[B] = 
	g => (p(g) >>- (a => h(a)(g)))
  }
  def runHIO[A](p:HIO[A]):IO[Unit] = for {
	g <- newPrimGroup
	_ <- p(g)
  } yield ()

  //class GroupImpl(nv:TVar[Int], inhabs:TVar[Inhabitants], mutex:MVar[Unit], waiting:TVar[List[MVar[Unit]]]) extends Group {
  class GroupImpl(nv:TVar[Int], mutex:MVar[Unit], waiting:TVar[List[MVar[Unit]]]) extends Group {
	  // XXX think about exception handling in all the code below
    def increment:IO[Unit] = modifyTVar[Int](nv, _+1) >> result(())
    def decrement:IO[Unit] = for {
	n <- modifyTVar[Int](nv, x => if (x == 0) 0 else x-1) 
	_ <- if (n._1 == 0) notifyAllWaiting else result(())
    } yield ()
    def isZero:IO[Unit] = for {
	signal <- newEmptyMVar[Unit]
	_ <- modifyTVar[List[MVar[Unit]]](waiting, signal::_)
	_ <- takeMVar(signal)
    } yield ()

    def notifyAllWaiting:IO[Unit] = for {
    	ws <- modifyTVar[List[MVar[Unit]]](waiting, _ => Nil)
	_ <- iter[MVar[Unit]](signal => putMVar(signal, ()), ws._1) 
    } yield ()

  // the following are used in the paper, but I am leaving out for now.
  // the actor-based implementation somewhat obviates the need for this anyway.
//    def register(e:Entry):IO[Unit] = atomic(mutex) { for {
//	_ <- modifyTVar(inhabs, e::(_:Inhabitants))
//    } yield () }
//    def killGroup:IO[Unit] = UNDEFINED
  }

  // is there a name for this already?
  def iter[A](f:A=>IO[Unit], thelist:List[A]):IO[Unit] = thelist match {
  	case Nil 	=> result(())
	case a::as 	=> f(a) >> iter(f,as)
  } 

  def newPrimGroup:IO[Group] = for {
  	n <- newTVar(0) 
	mutex <- newEmptyMVar[Unit]
	waiting <- newTVar[List[MVar[Unit]]](List())
  } yield new GroupImpl(n, mutex, waiting)

  implicit def HIOLiftIO:LiftIO[HIO] = new LiftIO[HIO] {
    def liftIO[A](ioa:IO[A]):HIO[A] = _ => ioa
  }

   // XXX exception handling
  def forkActorH(hio:HIO[Unit]):HIO[Unit]  = 
  	g => (g.increment >> hio(g) >> g.decrement)

  def newGroup:HIO[Group] = _ => newPrimGroup

  // Not actually killing anything in the current implementation
  //def close(g:Group):HIO[Unit] = liftIO(g.killGroup)
  def local[A](g:Group, p:HIO[A]):HIO[A] = liftIO_H(p(g))
  def finished(g:Group):HIO[Unit] = liftIO_H(g.isZero)

  /* Might need this later...
  class Kill {}
  def Kill:Kill = new Kill
  reactWithin(0) {
  	case k:Kill => { ... }
	case TIMEOUT => {
		// normal behaviour
	}
  }
  */

}
