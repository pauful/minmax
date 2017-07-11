package boardgame.tictactoe

import minmax._
import boardgame._

object TicTacToeHeuristic extends Heuristic[BoardState, BoardScore,TableBoard] {
  
  def centerScore(s: TableBoard): Int = {
    s.terrain(2,2) match {
      case Me(_,_) => 6
      case Rival(_,_) => -6
      case None(_,_) => 0
    }
  }
  
  def pairs(s:TableBoard): Int = {
    
     val nrows = s.myPieces.groupBy(_.x)
     .map({case (_,l) => if(l.size > 1 && Math.abs(l(0).y - l(1).y) == 1) 1  else 0})
     .foldLeft(0)(_+_)
    
     val ncols = s.myPieces.groupBy(_.y)
     .map({case (_,l) => if(l.size > 1 && Math.abs(l(0).x - l(1).x) == 1) 1  else 0})
     .foldLeft(0)(_+_)
     
     val rrows = s.rivalPieces.groupBy(_.x)
     .map({case (_,l) => if(l.size > 1 && Math.abs(l(0).y - l(1).y) == 1) 1  else 0})
     .foldLeft(0)(_+_)
    
     val rcols = s.rivalPieces.groupBy(_.y)
     .map({case (_,l) => if(l.size > 1 && Math.abs(l(0).x - l(1).x) == 1) 1  else 0})
     .foldLeft(0)(_+_)
     
     val pairsMe = nrows + ncols
     
     val pairsRival = rrows + rcols
     
     2 * pairsMe - 2 * pairsRival
  }
  
  def corners(s:TableBoard): Int = {
    val myCorners = s.myPieces.filter(t => (t.x == 1 && t.y == 1) || (t.x == 1 && t.y == 3)
                                                  || (t.x == 3 && t.y == 1) || (t.x == 3 && t.y == 3)).size
    
    val rivalCorners = s.rivalPieces.filter(t => (t.x == 1 && t.y == 1) || (t.x == 1 && t.y == 3)
                                                  || (t.x == 3 && t.y == 1) || (t.x == 3 && t.y == 3)).size
                                                  
    myCorners * 2 - 2 * rivalCorners                                          
  }
  
  def win(s: TableBoard): Int = {
    val nrows = s.myPieces.groupBy(_.x).filter({case (_,l) => l.size ==3}).size
    val ncols = s.myPieces.groupBy(_.y).filter({case (_,l) => l.size ==3}).size
    
    1000 * nrows + 1000 * ncols
  }
  
  def defeat(s: TableBoard): Int = {
    val nrows = s.rivalPieces.groupBy(_.x).filter({case (_,l) => l.size ==3}).size
    val ncols = s.rivalPieces.groupBy(_.y).filter({case (_,l) => l.size ==3}).size
    
    -1000 * nrows + -1000 * ncols
  }
  
  def score(t: TableBoard): BoardScore = {
    BoardScore(defeat(t) + win(t) + corners(t) + centerScore(t) + pairs(t))
  }
  
  def prune(s: BoardState): Boolean = {
    s.score.value > 999 || s.score.value < -999 || s.childs.size == 2
  }
}
