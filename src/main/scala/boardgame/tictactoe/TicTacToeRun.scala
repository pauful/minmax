package boardgame.tictactoe

import minmax._
import boardgame._

import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io.Source
import scala.io.StdIn._

object TicTacToeRun {
  
  def getUserInput: (Int,Int) = {
    println("col?")
    val col = readLine().toInt
    println("row?")
    val row = readLine().toInt
    (col,row)
  }
  
  case class TicTacToeGame(val initial: BoardState, val heuristic: TicTacToeHeuristic) extends MinMax[BoardState,BoardScore,TableBoard]
  
  def main(args: Array[String]): Unit = {
    implicit val formats = DefaultFormats
    val values = parse(Source.fromInputStream(getClass.getResourceAsStream("/values.json")).getLines.mkString)
                .extract[Map[String, Map[String,Int]]]
    
    val h = new TicTacToeHeuristic(values)
    
    var terrain: (Int, Int) => Move = {
      (x,y) => None(x,y)
    }
    var currentState = BoardState(BoardScore(0), Rival(0,0), Nil, TableBoard(terrain, 1, 3))
    println("Do you wanna start? [yes,no]")
    if(readLine() == "yes") {
      val input = getUserInput
      
      currentState = BoardState(BoardScore(0), Rival(input._1,input._2), Nil, 
            currentState.board.move(Rival(input._1,input._2)))
    }
    while((h.win(currentState.board) < 900 || h.defeat(currentState.board) > -900)  && !currentState.isEndOfTheGame) {
      currentState.lastMove match {
        case Me(_,_) => {
          var input = getUserInput
          val newBoard = currentState.board.move(Rival(input._1,input._2))
          currentState = BoardState(h.score(newBoard, Rival(input._1,input._2)), Rival(input._1,input._2), Nil, newBoard)
        }
        case Rival(_,_) => {
          val bestState = TicTacToeGame(currentState, h).bestNextAction.getOrElse(null)
          val board = currentState.board.move(bestState.lastMove)
          currentState = BoardState(bestState.score, bestState.lastMove, Nil, board)
        }
      }
      println
      println(currentState.lastMove)
      println(currentState.board)
      println
      println(currentState.score.value)
      println(currentState.isEndOfTheGame)
//      print(h.prune(currentState))
    }  
  }
}


