// Copyright (c) 2012, David Jeffery
// All rights reserved.

package scorch

import scalaz._
import Scalaz._

import Pimpz._

trait Scorch {

  val io:IOModule
  import io._

    // there is a type SC[A]
  type SC[A]  
    // and there is a monad instance (implicit) for it
  implicit def SCMonad[A]:Monad[SC]

  implicit def SCLiftIO:LiftIO[SC]
    // XXX shouldn't need this(?) but in some circumstances I do for some reason
  def liftIO_SC[A](ioa:IO[A]):SC[A] = implicitly[LiftIO[SC]].liftIO(ioa) 

    // why do we need the 'implicitly'?
  def result[A](a:A):SC[A] = implicitly[Pure[SC]].pure(a)

  def stop[A]:SC[A]
  def eagerly[A](a:SC[A]):SC[SC[A]]
  def runSC[A](a:SC[A]):IO[Unit] 

  def par[A](p:SC[A], q:SC[A]):SC[A]
  def append[A](p:SC[A], q:SC[A]):SC[A]

    // should get this for free from MonadPlus/Applicative or somesuch?
  def guard(b:Boolean):SC[Unit]= if (b) result(()) else stop

    // I'm just using pureF for lifting functions, with a function-specific
    // signature to avoid a bit of explicit currying when I use it
  def pureF[A,B](f:A=>B):SC[A=>B] = result(f)

  def applySC[A,B](fs:SC[A=>B], p:SC[A]):SC[B] = fs >>- (f => liftApply(f,p))
  class SCApply[A,B](val f:SC[A=>B]) {
	  def <*>(a:SC[A]):SC[B] = applySC(f, a)
  }
  implicit def SCApply[A,B](f:SC[A=>B]) = new SCApply(f)
  def liftApply[A,B](f:A=>B, p:SC[A]):SC[B] = p >>- (a => result(f(a)))
  class LiftApply[A,B](val f:A=>B) {
	    // would use <$> but that's not a valid operator...
	  def <#>(a:SC[A]):SC[B] = liftApply(f, a)
  }
  implicit def LiftApply[A,B](f:A=>B) = new LiftApply(f)

  def sync[A,B,C](f:A=>B=>C, p:SC[A], q:SC[B]):SC[C] = for {
  	po <- eagerly(p)
  	qo <- eagerly(q)
	ro <- pureF(f) <*> po <*> qo
    } yield ro
  def const[A,B] = (a:A)=>(b:B)=>a

  def delay(l:Long):SC[Unit] = liftIO_SC(threadDelay(l))

  def printSC[A](a:SC[A]):IO[Unit] =
  	runSC(for {
		x <- a
		_ <- liftIO_SC(putStrLn("Ans = " ++ a.toString))
	} yield ())

    // operator versions of the above, and some other useful operators
  class SCOperator[A](val p:SC[A]) {
    def <|>(q:SC[A]):SC[A] = par(p,q)
    def <+>(q:SC[A]):SC[A] = append(p,q)
    def butAfter(l:Long, q:SC[A]):SC[A] = cut(p <|> (delay(l) >> q))
    def orElse(q:SC[A]):SC[A] = for {
  		tripwire <- liftIO_SC(newEmptyMVar[Unit])
		a <- (for {
				x <- p
				_ <- liftIO_SC(tryPutMVar[Unit](tripwire, ()))
			} yield x) <+> (for {
				triggered <- liftIO_SC(tryTakeMVar[Unit](tripwire))
				theRest <- (if (triggered == None) q else stop)
			} yield theRest)
	} yield a
    def notBefore(w:Long):SC[A] = sync(const[A,Unit], p, delay(w))
  }
  implicit def SCOperator[A](sc:SC[A]):SCOperator[A] = new SCOperator[A](sc)

    // why do we need to declare the type variable binding for 'stop'?
  def liftList[A](as:List[A]):SC[A] = as.map(result).foldRight(stop[A])(_ <|> _)
    // maybe a funkier way of doing it...
  def liftList2[A](as:List[A]):SC[A] = as.foldRight(stop[A])(result(_) <|> _)

  def cut[A](p:SC[A]):SC[A] = for {
  	ox <- eagerly(p)
	x <- ox
    } yield x
}

trait IOModule {
  type IO[A] 
  implicit def IOMonad[A]:Monad[IO]

  def result[A](a:A):IO[A] = implicitly[Monad[IO]].pure(a)

  def unsafePerformIO[A](sideAffectingCode: =>A):IO[A]
  def toplevelRunIO[A](io:IO[A]):A 

  trait LiftIO[M[_]] {
    def liftIO[A](a:IO[A]):M[A]
  }

  type MVar[A]
  def newEmptyMVar[A]:IO[MVar[A]]
  def newMVar[A](a:A):IO[MVar[A]] = for {
	  m <- newEmptyMVar[A] 
	  _ <- putMVar(m,a)
  } yield m
  def putMVar[A](m:MVar[A], a:A):IO[Unit]
  def takeMVar[A](m:MVar[A]):IO[A]
  def readMVar[A](m:MVar[A]):IO[A]
  def tryPutMVar[A](m:MVar[A], a:A):IO[Unit]
  def tryTakeMVar[A](m:MVar[A]):IO[Option[A]]

  def atomic[A](m:MVar[Unit])(io:IO[A]):IO[A] 

  type TVar[A]
  def newTVar[A](a:A):IO[TVar[A]]
  def modifyTVar[A](t:TVar[A],f:A=>A):IO[(A,A)]
  def readTVar[A](t:TVar[A]):IO[A] = for (c <- modifyTVar(t, id[A])) yield c._2
  
  def putStrLn(s:String):IO[Unit] = unsafePerformIO(Console.println(s))

  def threadDelay(l:Long):IO[Unit]

  def forkActor(io:IO[Unit]):IO[Unit] 

  type HIO[A]
  implicit def HIOMonad[A]:Monad[HIO]
  def runHIO[A](p:HIO[A]):IO[Unit]

  trait Group  {
    def increment:IO[Unit]
    def decrement:IO[Unit]
    def isZero:IO[Unit]

  // the following are used in the paper, but I am leaving out for now.
  // the actor-based implementation somewhat obviates the need for this anyway.
    //def register(e:Entry):IO[Unit]
    //def killGroup:IO[Unit]
  }
  //type Inhabitants = List[Entry]
  //type Inhabitants = Option[List[Entry]]  // paper says this, not sure why
  //type Entry = Either[ThreadId, Group]
  //type ThreadId = Int

  def newPrimGroup:IO[Group]

  implicit def HIOLiftIO:LiftIO[HIO]
    // XXX shouldn't need this(?) but in some circumstances I do for some reason
  def liftIO_H[A](ioa:IO[A]):HIO[A] = implicitly[LiftIO[HIO]].liftIO(ioa) 

  def forkActorH(hio:HIO[Unit]):HIO[Unit] 

  def newGroup:HIO[Group] 
  //def close(g:Group):HIO[Unit] 
  def local[A](g:Group, p:HIO[A]):HIO[A] 
  def finished(g:Group):HIO[Unit] 
}

trait Examples extends Scorch {
  import io._

  def fplang = for (
  	w <- result("Scala") <|> result("Haskell") <|> result("OCaml")
  ) yield (w ++ " is great!")

  def metronome:SC[Unit] = result () <|> (delay(2) >> metronome)

  def queens = extend(List()) <|> result("Computing 8-queens...")
  def queens2 = result("Computing 8-queens...") <+> extend(List()) 
  def extend(xs:List[Int]):SC[String] =
  	if (xs.length == 8) result(xs.toString)
	else for {
		j <- liftList(List(1,2,3,4,5,6,7,8))
		_ <- guard(!conflicts(xs, j))
		e <- extend(j::xs)
	} yield e
  def conflicts(xs:List[Int], j:Int):Boolean = true // XXX

  def scan[A,S](f:A=>S=>S, s:S, p:SC[A]):SC[S] = for {
  	accum <- liftIO_SC(newTVar[S](s))
	x <- p
	w <- liftIO_SC(modifyTVar(accum, f(x)))
    } yield w._2

  def count[A](p:SC[A]):SC[Either[A,Int]] = for {
  	accum <- liftIO_SC(newTVar(0))
	eai <- (for {
			x <- p
			_ <- liftIO_SC(modifyTVar(accum, (_:Int)+1))
		} yield Left(x):Either[A,Int]) <+> 
		(for {
			c <- liftIO_SC(readTVar(accum))
		} yield Right[A,Int](c):Either[A,Int])
    } yield eai

  def collect[A](p:SC[A]):SC[List[A]] = for {
  	accum <- liftIO_SC(newTVar[List[A]](List()))
	as <- (for {
			x <- p
			_ <- liftIO_SC(modifyTVar[List[A]](accum, 
					(xs:List[A])=>x::xs))
			s <- stop[List[A]]
		} yield s) <+> liftIO_SC(readTVar[List[A]](accum))
    } yield as

  def parallelOr(p:SC[Boolean], q:SC[Boolean]):SC[Boolean] = for {
  	ox <- eagerly(p)
  	oy <- eagerly(q)
	c  <- ( cut((ox >>- guard _) >> result(true)) <|>
	        cut((oy >>- guard _) >> result(true)) <|>
	       (pureF(or) <*> ox <*> oy) 
	      )
    } yield c
  def or:Boolean=>Boolean=>Boolean = (b1:Boolean) => (b2:Boolean) => b1 || b2 

  type Quote
  type Query
  def getQuote : Query => SC[Quote]
  def price : Quote => Int
  def least = (x:Quote) => (y:Quote) => if (price(x) < price(y)) x else y
  def threshold(x:Quote):SC[Quote] = guard (price(x) < 300) >> result(x)
  def noQuote:Quote
  def quotes(srcA:Query, srcB:Query):SC[Quote] = for {
	quoteA <- eagerly (getQuote(srcA))
	quoteB <- eagerly (getQuote(srcB))
	c      <- cut(
			(pureF(least) <*> quoteA <*> quoteB) <|>
			(quoteA >>- threshold)              <|>
			(quoteB >>- threshold)              <|>
			(delay(25) >> (quoteA <|> quoteB))  <|>
			(delay(30) >> result(noQuote))
		  )
    } yield c

  def email(to:String, msg:String):IO[Unit]
  def hassle:SC[Unit] = 
  	(metronome >> liftIO_SC(email("Simon","Hey!")) >> stop).
		butAfter(60, result(()) )
  
  // examples in paper from Orction onwards not yet done
}

