package boardgame

import minmax._

case class TableBoard(val terrain: (Int, Int) => Move, val min: Int, val max: Int) extends Table {
  
  type Terrain = (Int, Int) => Move
  
  def move(m: Move): TableBoard = {
    m match {
      case Me(x,y) => if((x > max || x < min) || (y > max || y < min)) this else {
        TableBoard((fx: Int,fy: Int) => if (fx == x && fy == y) Me(fx,fy) else this.terrain(fx,fy), min, max)
      }
      case Rival(x,y) => if((x > max || x < min) || (y > max || y < min)) this else {
        TableBoard((fx: Int,fy: Int) => if (fx == x && fy == y) Rival(fx,fy) else this.terrain(fx,fy), min, max)
      }
      case None(x,y) => if((x > max || x < min) || (y > max || y < min)) this else {
        TableBoard((fx: Int,fy: Int) => if (fx == x && fy == y) None(fx,fy) else this.terrain(fx,fy), min, max)
      }
    }
  }
  
  def possibleMoves[None]: Seq[None] = {
    for {
        x <- min to max
        y <- min to max
        if terrain(x,y) == None(x,y)
      } yield terrain(x,y).asInstanceOf[None]
  }
  
  override def toString: String = {
    (min to max).flatMap({x => (min to max).map({y => terrain(x,y) + " "}).mkString("")+ "\n" } ).mkString("")
  }
  
  val myPieces: Seq[Me] = for {
      x <- min to max
      y <- min to max
      if (terrain(x,y) == Me(x,y))
    } yield Me(x,y)
    
   val rivalPieces: Seq[Rival] = for {
      x <- min to max
      y <- min to max
      if (terrain(x,y) == Rival(x,y))
    } yield Rival(x,y)
}


case class BoardState(val score: BoardScore, val lastMove: Move, val childs: List[BoardState], val board: TableBoard) extends State[BoardState,BoardScore,TableBoard] {
  
  def posibleStates(h: Heuristic[BoardState,BoardScore,TableBoard]): Seq[BoardState] = {

    board.possibleMoves[None].map({
      m => lastMove match {
        case Me(x,y) => BoardState(BoardScore(0), Rival(m.x, m.y), this::this.childs, board.move(Rival(m.x, m.y))).score(h)
        case Rival(x,y) => BoardState(BoardScore(0), Me(m.x, m.y), this::this.childs, board.move(Me(m.x, m.y))).score(h)
      }
    })    
  }
  
  def score(h: Heuristic[BoardState,BoardScore,TableBoard]): BoardState = {
    BoardState(h.score(this.board), this.lastMove, this.childs, this.board)
  }
  
  def updateScore(childs: Seq[BoardState]): BoardState = {
    this.lastMove match {
      case Me(_,_) => BoardState(childs.maxBy(_.score).score, this.lastMove, this.childs, this.board)
      case Rival(_,_) => BoardState(childs.minBy(_.score).score, this.lastMove, this.childs, this.board)
    }
  }
  
  def isEndOfTheGame: Boolean = board.possibleMoves.size == 0 || !childs.filter(t => t.score.value > 999 || t.score.value < -999).isEmpty
}

case class BoardScore(val value: Int) extends Score[BoardScore] {
  def compare(s: BoardScore): Int = {
    this.value compareTo s.value
  }
}

case class Me(val x: Int, val y:Int) extends Move {
  def name = "Me"
}
case class Rival(val x: Int, val y:Int) extends Move {
  def name = "Rival"
}
case class None(val x: Int, val y:Int) extends Move {
  def name = "None"
}