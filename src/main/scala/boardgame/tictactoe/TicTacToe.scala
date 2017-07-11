package boardgame.tictactoe

import minmax._
import boardgame._

case class TicTacToeTable(val terrain: (Int, Int) => Move, val min: Int, val max: Int) extends TableBoard {
  
  def move(m: Move): TicTacToeTable = {
    m match {
      case Me(x,y) => if((x > max || x < min) || (y > max || y < min)) this else {
        TicTacToeTable((fx: Int,fy: Int) => if (fx == x && fy == y) Me(fx,fy) else this.terrain(fx,fy), min, max)
      }
      case Rival(x,y) => if((x > max || x < min) || (y > max || y < min)) this else {
        TicTacToeTable((fx: Int,fy: Int) => if (fx == x && fy == y) Rival(fx,fy) else this.terrain(fx,fy), min, max)
      }
      case None(x,y) => if((x > max || x < min) || (y > max || y < min)) this else {
        TicTacToeTable((fx: Int,fy: Int) => if (fx == x && fy == y) None(fx,fy) else this.terrain(fx,fy), min, max)
      }
    }
  }
  
  def possibleMoves[None]: Seq[None] = {
    for {
        x <- min to max
        y <- min to max
        if terrain(x,y).isEmpty
      } yield terrain(x,y).asInstanceOf[None]
  }
  
  override def toString: String = {
    (min to max).flatMap({x => (min to max).map({y => terrain(x,y) + " "}).mkString("")+ "\n" } ).mkString("")
  }
}

case class TicTacToeState(val score: TicTacToeScore, val move: Move, val childs: List[TicTacToeState], val board: TicTacToeTable) extends State[TicTacToeState,TicTacToeScore,TicTacToeTable] {
  
  def posibleStates(h: Heuristic[TicTacToeState,TicTacToeScore,TicTacToeTable]): Seq[TicTacToeState] = {

    board.possibleMoves[None].map({
      m => move match {
        case Me(x,y) => TicTacToeState(null, Rival(m.x, m.y), this::this.childs, board.move(Rival(m.x, m.y))).score(h)
        case Rival(x,y) => TicTacToeState(null, Me(m.x, m.y), this::this.childs, board.move(Me(m.x, m.y))).score(h)
      }
    })    
  }
  
  def score(h: Heuristic[TicTacToeState,TicTacToeScore,TicTacToeTable]): TicTacToeState = {
    TicTacToeState(h.score(this.board), this.move, this.childs, this.board)
  }
  
  def updateScore(childs: Seq[TicTacToeState]): TicTacToeState = {
    this.move match {
      case Me(_,_) => TicTacToeState(childs.maxBy(_.score).score, this.move, this.childs, this.board)
      case Rival(_,_) => TicTacToeState(childs.minBy(_.score).score, this.move, this.childs, this.board)
    }
  }
  
  def isEndOfTheGame: Boolean = board.possibleMoves.size == 0
}

case class TicTacToeScore(val value: Int) extends Score[TicTacToeScore] {
  def compare(s: TicTacToeScore): Int = {
    this.value compareTo s.value
  }
}

object TicTacToeHeuristic extends Heuristic[TicTacToeState, TicTacToeScore,TicTacToeTable] {
  
  def centerScore(s: TicTacToeTable): Int = {
    s.terrain(2,2) match {
      case Me(_,_) => 6
      case Rival(_,_) => -6
      case None(_,_) => 0
    }
  }
  
  def pairs(s:TicTacToeTable): Int = {
    
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
  
  def corners(s:TicTacToeTable): Int = {
    val myCorners = s.myPieces.filter(t => (t.x == 1 && t.y == 1) || (t.x == 1 && t.y == 3)
                                                  || (t.x == 3 && t.y == 1) || (t.x == 3 && t.y == 3)).size
    
    val rivalCorners = s.rivalPieces.filter(t => (t.x == 1 && t.y == 1) || (t.x == 1 && t.y == 3)
                                                  || (t.x == 3 && t.y == 1) || (t.x == 3 && t.y == 3)).size
                                                  
    myCorners * 2 - 2 * rivalCorners                                          
  }
  
  def win(s: TicTacToeTable): Int = {
    val nrows = s.myPieces.groupBy(_.x).filter({case (_,l) => l.size ==3}).size
    val ncols = s.myPieces.groupBy(_.y).filter({case (_,l) => l.size ==3}).size
    
    1000 * nrows + 1000 * ncols
  }
  
  def defeat(s: TicTacToeTable): Int = {
    val nrows = s.rivalPieces.groupBy(_.x).filter({case (_,l) => l.size ==3}).size
    val ncols = s.rivalPieces.groupBy(_.y).filter({case (_,l) => l.size ==3}).size
    
    -1000 * nrows + -1000 * ncols
  }
  
  def score(t: TicTacToeTable): TicTacToeScore = {
    TicTacToeScore(defeat(t) + win(t) + corners(t) + centerScore(t) + pairs(t))
  }
  
  def prune(s: TicTacToeState): Boolean = {
    s.score.value > 999 || s.score.value < -999 || s.childs.size == 2
  }
}
