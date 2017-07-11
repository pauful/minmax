package boardgame.tictactoe

import minmax._
import boardgame._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MinMaxTest extends FunSuite {
  
  case class TicTacToeGame(val initial: BoardState) extends MinMax[BoardState,BoardScore,TableBoard] {
    val heuristic = TicTacToeHeuristic
  }
  
  test("initial move") {
    val terrain: (Int, Int) => Move = {
      (x,y) => None(x,y)
    }
    var initial= BoardState(BoardScore(0), Rival(0,0), Nil, TableBoard(terrain, 1, 3) )
    var ttt = TicTacToeGame(initial)
    var na= ttt.bestNextAction
    assert(na == Me(2,2))
    
    var newTerrain = TableBoard(terrain, 1, 3).move(ttt.bestNextAction).move(Rival(1,1))
    println(newTerrain)
    println
    initial= BoardState(BoardScore(0), Rival(1,1), Nil, TableBoard(newTerrain.terrain, 1, 3) )
    ttt = TicTacToeGame(initial)
    assert(ttt.bestNextAction == Me(1,2))
    
    newTerrain = TableBoard(newTerrain.terrain, 1, 3).move(ttt.bestNextAction).move(Rival(3,2))
    println(newTerrain)
    println
    initial= BoardState(BoardScore(0), Rival(1,2), Nil, TableBoard(newTerrain.terrain, 1, 3) )
    ttt = TicTacToeGame(initial)
    assert(ttt.bestNextAction == Me(1,3))
  }
  
}