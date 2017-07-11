package boardgame.tictactoe

import minmax._
import boardgame._

object TicTacToeRun {
  
  def getUserInput: (Int,Int) = {
    println("row?")
    val row = scala.io.StdIn.readLine().toInt
    println("col?")
    val col = scala.io.StdIn.readLine().toInt
    (col,row)
  }
  
  case class TicTacToeGame(val initial: TicTacToeState) extends MinMax[TicTacToeState,TicTacToeScore] {
    val heuristic = TicTacToeHeuristic
    
    
    def main(args: Array[String]): Unit = {
      println("Hello, world!")
    }
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


