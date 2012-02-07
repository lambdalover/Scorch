package scorch

import scalaz._
import Scalaz._

trait Scorch {
    // there is a type SC[A]
  type SC[A]  
    // and there is a monad instance (implicit) for it
  implicit def SCMonad[A]:Monad[SC]

      // result already defined in Monad[_]?
  def result[A](a:A):SC[A]
  def stop[A]:SC[A]
  def eagerly[A](a:SC[A]):SC[SC[A]]
  def liftIO[A](a:IO[A]):SC[A]
  def runSC[A](a:SC[A]):IO[Unit] // or :Unit?

  def par[A](p:SC[A], q:SC[A]):SC[A]
  def append[A](p:SC[A], q:SC[A]):SC[A]
    // >>= already defined in Monad[_]?
  def bind[A,B](p:SC[A], f:A=>SC[B]):SC[B]

  // should get the following for free from MonadPlus/Applicative?
  def guard(b:Boolean):SC[Unit]
    // is 'pure' any different to 'result'?
  //def pure[A](a:A):SC[A]
    // I'm just using pureF for lifting functions, with a function-specific
    // signature to avoid a bit of explicit currying when I use it
  def pureF[A,B](f:A=>B):SC[A=>B] = result(f)

  def applySC[A,B](f:SC[A=>B], a:SC[A]):SC[B]
  class SCApply[A,B](val f:SC[A=>B]) {
	  def <*>(a:SC[A]):SC[B] = applySC(f, a)
  }
  implicit def SCApply[A,B](f:SC[A=>B]) = new SCApply(f)

  def liftApply[A,B](f:A=>B, a:SC[A]):SC[B]
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

    // define some of the above as operators
  class SCOperator[A](val p:SC[A]) {
    def <|>(q:SC[A]):SC[A] = par(p,q)
    def <+>(q:SC[A]):SC[A] = append(p,q)
    def >>=[B](f:A=>SC[B]):SC[B] = bind(p,f)
      // is this correct, or do we get repeated qs?
    def >>[B](q:SC[B]):SC[B] = p >>= (_=>q) 
    def butAfter(d:Double, q:SC[A]):SC[A] = cut(p <|> (delay(d) >> q))
    def orElse(q:SC[A]):SC[A] = for {
  		tripwire <- liftIO(newEmptyMVar[Unit])
		a <- (for {
				x <- p
				_ <- liftIO(tryPutMVar[Unit](tripwire, ()))
			} yield x) <+> (for {
				triggered <- liftIO(tryTakeMVar[Unit](tripwire))
				theRest <- (if (triggered == None) q else stop)
			} yield theRest)
	} yield a
    def notBefore(w:Double):SC[A] = sync(const[A,Unit], p, delay(w))
  }
  implicit def SCOperator[A](sc:SC[A]):SCOperator[A] = new SCOperator[A](sc)

  type IO[A] 
  implicit def IOMonad[A]:Monad[IO]

  type MVar[A]
  def newEmptyMVar[A]:IO[MVar[A]]
  def takeMVar[A](m:MVar[A]):IO[A]
  def putMVar[A](m:MVar[A], a:A):IO[Unit]
  def tryPutMVar[A](m:MVar[A], a:A):IO[Unit]
  def tryTakeMVar[A](m:MVar[A]):IO[Option[A]]

  type TVar[A]
  def newTVar[A](a:A):IO[TVar[A]]
  def modifyTVar[A](t:TVar[A],f:A=>A):IO[(A,A)]
  def readTVar[A](t:TVar[A]):IO[A] = for (c <- modifyTVar(t, id[A])) yield c._2
  
  def putStrLn(s:String):IO[Unit]

  def threadDelay(d:Double):IO[Unit]
  def delay(d:Double):SC[Unit] = liftIO(threadDelay(d))

  def printSC[A](a:SC[A]):IO[Unit] =
  	runSC(for {
		x <- a
		_ <- liftIO(putStrLn("Ans = " ++ a.toString))
	} yield ())

    // why do we need to declare the type variable binding for 'stop'?
  def liftList[A](as:List[A]):SC[A] = as.map(result).foldRight(stop[A])(_ <|> _)
    // maybe a funkier way of doing it...
  def liftList2[A](as:List[A]):SC[A] = as.foldRight(stop[A])(result(_) <|> _)

  //def cut[A](p:SC[A]):SC[A] = p.join compose eagerly
  def cut[A](p:SC[A]):SC[A] = for {
  	ox <- eagerly(p)
	x <- ox
    } yield x
}

trait Examples extends Scorch {
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
  	accum <- liftIO(newTVar[S](s))
	x <- p
	w <- liftIO(modifyTVar(accum, f(x)))
    } yield w._2

  def count[A](p:SC[A]):SC[Either[A,Int]] = for {
  	accum <- liftIO(newTVar(0))
	eai <- (for {
			x <- p
			_ <- liftIO(modifyTVar(accum, (_:Int)+1))
		} yield Left(x):Either[A,Int]) <+> 
		(for {
			c <- liftIO(readTVar(accum))
		} yield Right[A,Int](c):Either[A,Int])
    } yield eai

  def collect[A](p:SC[A]):SC[List[A]] = for {
  	accum <- liftIO(newTVar[List[A]](List()))
	as <- (for {
			x <- p
			_ <- liftIO(modifyTVar[List[A]](accum, 
					(xs:List[A])=>x::xs))
			s <- stop[List[A]]
		} yield s) <+> liftIO(readTVar[List[A]](accum))
    } yield as

  def parallelOr(p:SC[Boolean], q:SC[Boolean]):SC[Boolean] = for {
  	ox <- eagerly(p)
  	oy <- eagerly(q)
	c  <- ( cut((ox >>= guard _) >> result(true)) <|>
	        cut((oy >>= guard _) >> result(true)) <|>
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
			(quoteA >>= threshold)              <|>
			(quoteB >>= threshold)              <|>
			(delay(25) >> (quoteA <|> quoteB))  <|>
			(delay(30) >> result(noQuote))
		  )
    } yield c

  def email(to:String, msg:String):IO[Unit]
  def hassle:SC[Unit] = 
  	(metronome >> liftIO(email("Simon","Hey!")) >> stop).
		butAfter(60, result(()) )
  
  // examples in paper from Orction onwards not yet done
}
