package boardgame.tictactoe

import minmax._
import boardgame._

case class TicTacToeHeuristic(val values: Map[String,Map[String, Int]]) extends Heuristic[BoardState, BoardScore,TableBoard] {
  
  def myCenterScore(s: TableBoard): Int = {
    if(s.terrain(2,2) == Me(2,2))
      values get "center" get s.terrain(2,2).name
    else
      0
  }
  
  def rivalCenterScore(s: TableBoard): Int = {
    if(s.terrain(2,2) == Rival(2,2))
      values get "center" get s.terrain(2,2).name
    else
      0
  }
  
  def filterMe(m: Move): Boolean = m match {
    case Me(_,_) => true
    case Rival(_,_) => false
    case None(_,_) => false
  }
  
  def filterRival(m: Move): Boolean = m match {
    case Me(_,_) => false
    case Rival(_,_) => true
    case None(_,_) => false
  }
  
  def myPairs(s:TableBoard): Int = {
    
     val nrows = s.myPieces.groupBy(_.x)
       .map({case (x,l) => if(l.size == 2 && s.rivalPieces.groupBy(_.x).get(x).size == 0) 1  else 0})
       .foldLeft(0)(_+_)
    
     val ncols = s.myPieces.groupBy(_.y)
       .map({case (y,l) => if(l.size == 2 && s.rivalPieces.groupBy(_.y).get(y).size == 0) 1  else 0})
       .foldLeft(0)(_+_)
       
     val dm = s.diagonalsList.map(t => t.filter(filterMe).size == 2 && t.filter(filterRival).size == 0).filter(_==true).size
     
     val pairsMe = nrows + ncols
     
     (values get "pairs" get "Me") * (pairsMe + dm)
  }
  
  def rivalPairs(s:TableBoard): Int = {
     
     val rrows = s.rivalPieces.groupBy(_.x)
       .map({case (x,l) => if(l.size == 2 && s.myPieces.groupBy(_.x).get(x).size == 0) 1  else 0})
       .foldLeft(0)(_+_)
    
     val rcols = s.rivalPieces.groupBy(_.y)
       .map({case (y,l) => if(l.size == 2 && s.myPieces.groupBy(_.y).get(y).size == 0) 1  else 0})
       .foldLeft(0)(_+_)
       
     val dr = s.diagonalsList.map(t => t.filter(filterRival).size == 2 && t.filter(filterMe).size == 0).filter(_==true).size
     
     val pairsRival = rrows + rcols
     
     (values get "pairs" get "Rival") * (pairsRival + dr)
  }
  
  def myCorners(s:TableBoard): Int = {
    val myCorners = s.myPieces.filter(t => (t.x == 1 && t.y == 1) || (t.x == 1 && t.y == 3)
                                                  || (t.x == 3 && t.y == 1) || (t.x == 3 && t.y == 3)).size
                                                  
    (myCorners * (values get "corners" get "Me"))                                          
  }
  
  def rivalCorners(s:TableBoard): Int = {
    
    val rivalCorners = s.rivalPieces.filter(t => (t.x == 1 && t.y == 1) || (t.x == 1 && t.y == 3)
                                                  || (t.x == 3 && t.y == 1) || (t.x == 3 && t.y == 3)).size
                                                  
    ((values get "corners" get "Me") * rivalCorners)                                          
  }
  
  def win(s: TableBoard): Int = {
    val nrows = s.myPieces.groupBy(_.x).filter({case (_,l) => l.size ==3}).size
    val ncols = s.myPieces.groupBy(_.y).filter({case (_,l) => l.size ==3}).size
    val diagonal1 = if(s.myPieces.filter({case Me(x,y) => x==y}).size == 3) 1 else 0
    val diagonal2 = if (((s.max to s.min by -1) zip (s.min to s.max) )
      .map({case (x,y)=>s.terrain(x,y)}).filter({
        case Me(_,_) => true
        case _ => false}).size == 3) 1 else 0
    
    (values get "win" get "Me") * (nrows + ncols + diagonal1 + diagonal2)
  }
  
  def defeat(s: TableBoard): Int = {
    val nrows = s.rivalPieces.groupBy(_.x).filter({case (_,l) => l.size ==3}).size
    val ncols = s.rivalPieces.groupBy(_.y).filter({case (_,l) => l.size ==3}).size
    val diagonal = if(s.rivalPieces.filter({case Rival(x,y) => x==y}).size == 3) 1 else 0
    val diagonal2 = if (((s.max to s.min by -1) zip (s.min to s.max) )
      .map({case (x,y)=>s.terrain(x,y)}).filter({
        case Rival(_,_) => true
        case _ => false}).size == 3) 1 else 0
    
    (values get "win" get "Rival") * (nrows + ncols + diagonal + diagonal2)
  }
  
  def score(t: TableBoard, m: Move): BoardScore = {
    BoardScore((win(t) + myCorners(t) + myPairs(t) + myCenterScore(t)) - 
          (defeat(t) + rivalCorners(t) + rivalCenterScore(t) + rivalPairs(t)))
  }
  
  def prune(s: BoardState, iteration: Int): Boolean = {
    s.score.value > (values get "prune" get "Me") || s.score.value < (values get "prune" get "Rival") || iteration >= (values get "steps" get "Me")
  }
}
