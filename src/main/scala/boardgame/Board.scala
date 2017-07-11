package boardgame

import minmax._

trait TableBoard{
  def move(m: Move): TableBoard
  def possibleMoves[S >: Move]: Seq[S]
  val square: Int
  type Terrain = (Int, Int) => Move
  
  val terrain: Terrain
  
  val myPieces: Seq[Me] = for {
      x <- 1 to square
      y <- 1 to square
      if (terrain(x,y) == Me(x,y))
    } yield Me(x,y)
    
   val rivalPieces: Seq[Rival] = for {
      x <- 1 to square
      y <- 1 to square
      if (terrain(x,y) == Rival(x,y))
    } yield Rival(x,y)
}

case class Me(val x: Int, val y:Int) extends Move {
  def isEmpty = false
}
case class Rival(val x: Int, val y:Int) extends Move {
  def isEmpty = false
}
case class None(val x: Int, val y:Int) extends Move {
  def isEmpty = true
}