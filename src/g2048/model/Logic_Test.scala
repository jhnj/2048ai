package g2048.model
import org.junit.Test
import org.junit.Assert._
//import org.scalatest._

class Logic_Test {
  
  @Test def possibleMoves() {
	  val board = List(2,2,2,2,4,4,4,4,8,8,8,8,16,16,16,16)


	  val directions = List("right", "left", "up", "down")
	  assertEquals(board, Game.move("up", board)._1)
    assertEquals(List("right", "left"), directions.filter(d => Game.move(d, board)._1 != board))
    assertEquals(directions.filter(d => Game.possibleMove(d, board)), (List("left", "right").reverse))
  }
  
  
  @Test def rightMove {
	  val board = List(2,2,2,2,4,4,4,4,8,8,8,8,16,16,16,16)


    assertEquals("down", Ai.getMove(board, 1))
    assertEquals("down", Ai.getMove(List(2,4,4,2,0,0,4,0,2,2,8,4,2,0,0,8), 3))
  }
  
}