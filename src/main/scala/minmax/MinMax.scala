package minmax

import Stream._

trait State[St<:State[St,Sc], Sc<:Score[Sc]] extends Ordered[St] {
  
  val score: Sc
  val move: Move
  val childs: List[St]
  
  def posibleStates(h: Heuristic[St,Sc]): Seq[St]
  
  def updateScore(childs: Seq[St]): St
  
  def compare(b: St): Int = score compare b.score
}

trait Move

trait Heuristic[St<:State[St,Sc], Sc<:Score[Sc]] {
  def score(s: St): Sc
  def prune(state: St): Boolean
}

trait Score[S<:Score[S]] extends Ordered[S]  {
  def compare(s: S): Int
}

trait MinMax[St<:State[St,Sc], Sc<:Score[Sc]] {
  
  val heuristic: Heuristic[St,Sc]
  val initial: St
  
  def possibleMoves(state: St): Seq[St] = {
    for {
      a <- state.posibleStates(heuristic)
    } yield a
   
  }
  
  def from(state: St): Seq[St] = {
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
  
  def bestNextAction: Move = {
    from(initial).maxBy(_.score).move
  }
  
}
