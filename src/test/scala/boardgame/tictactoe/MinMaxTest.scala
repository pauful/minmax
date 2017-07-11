package boardgame.tictactoe

import minmax._
import boardgame._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class MinMaxTest extends FunSuite {
  
  case class TicTacToeGame(val initial: BoardState, val heuristic: TicTacToeHeuristic) extends MinMax[BoardState,BoardScore,TableBoard]
  
  test("initial move") {
    implicit val formats = DefaultFormats
    val values = parse(Source.fromInputStream(getClass.getResourceAsStream("/values.json")).getLines.mkString)
                .extract[Map[String, Map[String,Int]]] 
    
    val h = new TicTacToeHeuristic(values)
    val terrain: (Int, Int) => Move = {
      (x,y) => None(x,y)
    }
    var initial= BoardState(BoardScore(0), Rival(0,0), Nil, TableBoard(terrain, 1, 3) )
    var ttt = TicTacToeGame(initial,h)
    assert(ttt.bestNextAction.get == Me(2,2))
    
    var newTerrain = TableBoard(terrain, 1, 3).move(ttt.bestNextAction.getOrElse(None(0,0))).move(Rival(1,1))
    println(newTerrain)
    println
    initial= BoardState(BoardScore(0), Rival(1,1), Nil, TableBoard(newTerrain.terrain, 1, 3) )
    ttt = TicTacToeGame(initial,h)
    assert(ttt.bestNextAction.get == Me(1,2))
    
    newTerrain = TableBoard(newTerrain.terrain, 1, 3).move(ttt.bestNextAction.getOrElse(None(0,0))).move(Rival(3,2))
    println(newTerrain)
    println
    initial= BoardState(BoardScore(0), Rival(1,2), Nil, TableBoard(newTerrain.terrain, 1, 3) )
    ttt = TicTacToeGame(initial,h)
    assert(ttt.bestNextAction.get == Me(1,3))
  }
  
}