package boardgame.tictactoe

import minmax._
import boardgame._

import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io.Source

object TicTacToeRun {
  
  def getUserInput: (Int,Int) = {
    println("row?")
    val row = scala.io.StdIn.readLine().toInt
    println("col?")
    val col = scala.io.StdIn.readLine().toInt
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
    var table = TableBoard(terrain, 1, 3)
    var currentState = BoardState(BoardScore(0), Rival(0,0), Nil, TableBoard(terrain, 1, 3))
    println("Do you wanna start? [yes,no]")
    if(scala.io.StdIn.readLine() == "yes") {
      
      table = TableBoard(terrain, 1, 3).move(Rival(1,1))
      var input = getUserInput
      currentState = BoardState(BoardScore(0), Rival(input._1,input._2), Nil, table)
      table = TableBoard(table.terrain, 1, 3).move(Rival(input._1,input._2))
    }
    
    while(h.prune(currentState) || !currentState.isEndOfTheGame) {
      table = TableBoard(table.terrain, 1, 3).move(TicTacToeGame(currentState, h).bestNextAction)
      println(table)
      println
      var input = getUserInput
      currentState = BoardState(BoardScore(0), Rival(input._1,input._2), Nil, table)
      table = TableBoard(table.terrain, 1, 3).move(Rival(input._1,input._2))
    }  
  }
}


