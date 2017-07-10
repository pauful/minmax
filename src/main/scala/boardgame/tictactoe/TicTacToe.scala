package boardgame.tictactoe

import minmax._
import boardgame._

case class TicTacToeTable(val terrain: (Int, Int) => Move) extends TableBoard {
  val square = 3
  def move(m: Move): TicTacToeTable = {
    m match {
      case Me(x,y) => if((x > 3 || x < 0) || (y > 3 || y < 0)) this else {
        new TicTacToeTable((fx: Int,fy: Int) => if (fx == x && fy == y) new Me(fx,fy) else this.terrain(fx,fy))
      }
      case Rival(x,y) => if((x > 3 || x < 0) || (y > 3 || y < 0)) this else {
        new TicTacToeTable((fx: Int,fy: Int) => if (fx == x && fy == y) new Rival(fx,fy) else this.terrain(fx,fy))
      }
      case None(x,y) => if((x > 3 || x < 0) || (y > 3 || y < 0)) this else {
        new TicTacToeTable((fx: Int,fy: Int) => if (fx == x && fy == y) new None(fx,fy) else this.terrain(fx,fy))
      }
    }
  }
  
  override def toString: String =
    terrain(1,1) + " " + terrain(1,2)+ " " + terrain(1,3) + "\n" + terrain(2,1) + " " + terrain(2,2)+ " " + terrain(2,3) + "\n" + terrain(3,1) + " " + terrain(3,2)+ " " + terrain(3,3)
}

case class TicTacToeState(val score: TicTacToeScore, val move: Move, val childs: List[TicTacToeState], val board: TicTacToeTable) extends State[TicTacToeState,TicTacToeScore] {
  
  def posibleStates(h: Heuristic[TicTacToeState,TicTacToeScore]): Seq[TicTacToeState] = {
    for {
      x <- 1 to 3
      y <- 1 to 3
      if board.terrain(x,y) == None(x,y)
    } yield {
      val nm = move match {
        case Me(_,_) => Rival(x,y)
        case _ => Me(x,y)
      }
      
      val state = TicTacToeState(null, nm, this::this.childs, board.move(nm))
      TicTacToeState(h.score(state), nm, state.childs, state.board)
    }
  }
  
  
  def updateScore(childs: Seq[TicTacToeState]): TicTacToeState = {
    this.move match {
      case Me(_,_) => TicTacToeState(childs.maxBy(_.score).score, this.move, this.childs, this.board)
      case Rival(_,_) => TicTacToeState(childs.minBy(_.score).score, this.move, this.childs, this.board)
    }
  }
}

case class TicTacToeScore(val value: Int) extends Score[TicTacToeScore] {
  def compare(s: TicTacToeScore): Int = {
    this.value compareTo s.value
  }
}

object TicTacToeHeuristic extends Heuristic[TicTacToeState, TicTacToeScore] {
  
  def centerScore(s: TicTacToeState): Int = {
    s.board.terrain(2,2) match {
      case Me(_,_) => 6
      case Rival(_,_) => -6
      case None(_,_) => 0
    }
  }
  
  def pairs(s:TicTacToeState): Int = {
    
     val nrows = s.board.myPieces.groupBy(_.x)
     .map({case (_,l) => if(l.size > 1 && Math.abs(l(0).y - l(1).y) == 1) 1  else 0})
     .foldLeft(0)(_+_)
    
     val ncols = s.board.myPieces.groupBy(_.y)
     .map({case (_,l) => if(l.size > 1 && Math.abs(l(0).x - l(1).x) == 1) 1  else 0})
     .foldLeft(0)(_+_)
     
     val rrows = s.board.rivalPieces.groupBy(_.x)
     .map({case (_,l) => if(l.size > 1 && Math.abs(l(0).y - l(1).y) == 1) 1  else 0})
     .foldLeft(0)(_+_)
    
     val rcols = s.board.rivalPieces.groupBy(_.y)
     .map({case (_,l) => if(l.size > 1 && Math.abs(l(0).x - l(1).x) == 1) 1  else 0})
     .foldLeft(0)(_+_)
     
     val pairsMe = nrows + ncols
     
     val pairsRival = rrows + rcols
     
     2 * pairsMe - 2 * pairsRival
  }
  
  def corners(s:TicTacToeState): Int = {
    val myCorners = s.board.myPieces.filter(t => (t.x == 1 && t.y == 1) || (t.x == 1 && t.y == 3)
                                                  || (t.x == 3 && t.y == 1) || (t.x == 3 && t.y == 3)).size
    
    val rivalCorners = s.board.rivalPieces.filter(t => (t.x == 1 && t.y == 1) || (t.x == 1 && t.y == 3)
                                                  || (t.x == 3 && t.y == 1) || (t.x == 3 && t.y == 3)).size
                                                  
    myCorners * 2 - 2 * rivalCorners                                          
  }
  
  def win(s: TicTacToeState): Int = {
    val nrows = s.board.myPieces.groupBy(_.x).filter({case (_,l) => l.size ==3}).size
    val ncols = s.board.myPieces.groupBy(_.y).filter({case (_,l) => l.size ==3}).size
    
    1000 * nrows + 1000 * ncols
  }
  
  def defeat(s: TicTacToeState): Int = {
    val nrows = s.board.rivalPieces.groupBy(_.x).filter({case (_,l) => l.size ==3}).size
    val ncols = s.board.rivalPieces.groupBy(_.y).filter({case (_,l) => l.size ==3}).size
    
    -1000 * nrows + -1000 * ncols
  }
  
  def score(s: TicTacToeState): TicTacToeScore = {
    TicTacToeScore(defeat(s) + win(s) + corners(s) + centerScore(s) + pairs(s))
  }
  
  def prune(s: TicTacToeState): Boolean = {
    s.score.value > 999 || s.score.value < -999 || s.childs.size == 2
  }
}

case class TicTacToeGame(val initial: TicTacToeState) extends MinMax[TicTacToeState,TicTacToeScore] {
    val heuristic = TicTacToeHeuristic
    
    
    def main(args: Array[String]): Unit = {
      println("Hello, world!")
    }
}

object TicTacToePlay {
  
  def getUserInput: (Int,Int) = {
    println("row?")
    val row = scala.io.StdIn.readLine().toInt
    println("col?")
    val col = scala.io.StdIn.readLine().toInt
    (col,row)
  }
  
  def main(args: Array[String]): Unit = {
    var terrain: (Int, Int) => Move = {
      (x,y) => None(x,y)
    }
    var table = TicTacToeTable(terrain)
    var currentState = TicTacToeState(TicTacToeScore(0), Rival(0,0), Nil, TicTacToeTable(terrain))
    println("Do you wanna start? [yes,no]")
    if(scala.io.StdIn.readLine() == "yes") {
      
      table = TicTacToeTable(terrain).move(Rival(1,1))
      var input = getUserInput
      currentState = TicTacToeState(TicTacToeScore(0), Rival(input._1,input._2), Nil, table)
      table = TicTacToeTable(table.terrain).move(Rival(input._1,input._2))
    }
    
    while(!TicTacToeHeuristic.prune(currentState)) {
      table = TicTacToeTable(table.terrain).move(TicTacToeGame(currentState).bestNextAction)
      println(table)
      println
      var input = getUserInput
      currentState = TicTacToeState(TicTacToeScore(0), Rival(input._1,input._2), Nil, table)
      table = TicTacToeTable(table.terrain).move(Rival(input._1,input._2))
    }
    
      
  }
}


