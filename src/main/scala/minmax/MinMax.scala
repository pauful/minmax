package minmax

import Stream._

trait State[St<:State[St,Sc,T], Sc<:Score[Sc], T<:Table] extends Ordered[St] {
  
  val score: Sc
  val lastMove: Move
  val childs: List[St]
  
  def posibleStates(h: Heuristic[St,Sc,T]): Seq[St]
  
  def score(h: Heuristic[St,Sc,T]): St
  
  def updateScore(childs: Seq[St]): St
  
  def compare(b: St): Int = score compare b.score
  
  def isEndOfTheGame: Boolean
}

trait Move {
  def name: String
}

trait Table

trait Heuristic[St<:State[St,Sc,T], Sc<:Score[Sc], T<:Table] {
  def score(s: T): Sc
  def prune(state: St): Boolean
}

trait Score[S<:Score[S]] extends Ordered[S]  {
  def compare(s: S): Int
}

trait MinMax[St<:State[St,Sc,T], Sc<:Score[Sc], T<:Table] {
  
  val heuristic: Heuristic[St,Sc,T]
  val initial: St
  
  def from(state: St): Seq[St] = {
    if(state.isEndOfTheGame) {
      Nil
    } else {
        for {
        ns <- state.posibleStates(heuristic)
      } yield {
        if(heuristic.prune(ns)){
          ns
        } else {
          ns.updateScore(from(ns))
        }
      }  
    }
  }
  
  def bestNextAction: Option[Move] = {
    from(initial) match {
      case Nil => None
      case xs => Some(xs.maxBy(_.score).lastMove)
    }
  }
  
}
