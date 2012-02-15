// Copyright (c) 2012, David Jeffery
// All rights reserved.

package scorch

import scalaz._
import Scalaz._

import Pimpz._

object ActorEv extends ActorEval
trait ActorEval extends Scorch {
  val io:IOModule = IOImpl
  import io._

  import scala.actors.Actor
  import scala.actors.Actor._
  import scala.actors.Channel
  import scala.actors.OutputChannel

  def UNDEFINED[T]:T = error("UNDEFINED")

  type SC[A] = OutputChannel[A] => Actor 
  implicit def SCMonad[A]:Monad[SC] = new Monad[SC] {
    def pure[A](a: => A):SC[A] = channel => new Actor { def act() = channel!a }
    def bind[A,B](p:SC[A], h:A=>SC[B]):SC[B] = channel => new Actor {
	val pchannel = new Channel[A]
	def act() = {
		p(pchannel).start
		loop { pchannel.react {
			case a => h(a)(channel).start
		} }
	}
    }
  }

  implicit def SCLiftIO:LiftIO[SC] = new LiftIO[SC] {
    def liftIO[A](ioa:IO[A]):SC[A] = 
    		channel => new Actor {
			def act() = channel!toplevelRunIO(ioa)
		}
  }

  def stop[A]:SC[A] = _ => new Actor { def act() = {} }
    // XXX eagerly makes no effort to kill off anything after the first event
  def eagerly[A](p:SC[A]):SC[SC[A]] = channel => new Actor {
	val pchannel = new Channel[A]
  	def act() = { 
		p(pchannel).start
		channel!(chan2 => new Actor {
		  def act() = {
			pchannel.react { // not looping!
			  case a => chan2!a
			}
		  }
		} )
	}
  }
  def runSC[A](p:SC[A]):IO[Unit]  = unsafePerformIO(p(new Channel[A]).start)

  def par[A](p:SC[A], q:SC[A]):SC[A] = channel => new Actor {
  	def act() = {
		p(channel).start
		q(channel).start
	}
  }
  def append[A](p:SC[A], q:SC[A]):SC[A] = channel => new Actor {
  	def act() = {
		p(channel).start
		// XXX How to wait for p to finish?
		UNDEFINED
		q(channel).start
	}
  }
}

/* the "real", full SC interface:
  type SC[A] = {}
  implicit def SCMonad[A]:Monad[SC] = UNDEFINED

  implicit def SCLiftIO:LiftIO[SC] = UNDEFINED

  def stop[A]:SC[A] = UNDEFINED
  def eagerly[A](a:SC[A]):SC[SC[A]] = UNDEFINED
  def runSC[A](a:SC[A]):IO[Unit]  = UNDEFINED

  def par[A](p:SC[A], q:SC[A]):SC[A] = UNDEFINED
  def append[A](p:SC[A], q:SC[A]):SC[A] = UNDEFINED
*/
