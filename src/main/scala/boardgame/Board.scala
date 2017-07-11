package boardgame

import minmax._

trait TableBoard extends Table{
  def move(m: Move): TableBoard
  def possibleMoves[S >: Move]: Seq[S]
  val min: Int
  val max: Int
  type Terrain = (Int, Int) => Move
  
  val terrain: Terrain
  
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

case class Me(val x: Int, val y:Int) extends Move {
  def isEmpty = false
}
case class Rival(val x: Int, val y:Int) extends Move {
  def isEmpty = false
}
case class None(val x: Int, val y:Int) extends Move {
  def isEmpty = true
}