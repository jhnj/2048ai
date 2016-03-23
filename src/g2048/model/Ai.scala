package g2048.model
import scala.util.Random
import scala.math._

object Ai {
  
  /** for some reason moves only up and down **/
  
  
  val random = new Random()
  
  def getMove(board: List[Int], maxDepth: Int): String = {
    
    def maximize(state: List[Int], alpha: Int, beta: Int, depth: Int): Int = {
		  val directions = List("right", "left", "up", "down")
		  val possibleMoves = directions.filter(d => Game.possibleMove(d, state))
		  
		  if (depth == 0 || possibleMoves.isEmpty) (evaluate(state)*1000).toInt
		  
		  else {
		    var newA = alpha
		    // recursive method to go through the moves and terminate early if alpha >= beta
		    def next(moves: List[String]): Unit= {
       		val newState = Game.move(moves.head, state)._1
  				newA = math.max(newA, minimize(newState, newA, beta, depth - 1))
  				
  				//recursive call if there are moves left and alpha < beta
  				if (!(moves.length == 1 || newA >= beta)) next(moves.tail)
		      
		    }
		    next(possibleMoves)
		    newA
		  }
      
    }
    
    def minimize(state: List[Int], alpha: Int, beta: Int, depth: Int): Int = {
      val empties = state.zipWithIndex.filter(_._1 == 0).map(_._2)

      if (depth == 0 || empties.length == 0) (evaluate(state)*1000).toInt
      
      
      else {
    	  val new2 = empties.map(i => state.updated(i, 2))
                  			  .map(s => (s, Game.smoothness(s)))
			  
			  val new4 = empties.map(i => state.updated(i, 4))
			                    .map(s => (s, Game.smoothness(s)))
			                    
			  val max = math.max(new2.maxBy(_._2)._2, new4.maxBy(_._2)._2)
			  
			  
			  
        var newB: Int = beta
     		// recursive method to go through the empty squares and terminate early if alpha >= beta
        def next(newState: List[List[Int]]): Unit = {
          //val newState = state.updated(indX.head, 2)
          newB = math.min(newB, maximize(newState.head, alpha, newB, depth - 1))
          
          //recursive call if there are empties left and alpha < beta
  				if (!(newState.length == 1 || alpha >= newB)) next(newState.tail)
        }
        
        next((new2 ++ new4).filter(_._2 == max).map(_._1))
        newB
       }
    }
    
    val moves = List("right", "left", "up", "down")
    val possibleMoves = moves.filter(d => Game.possibleMove(d, board))
    
    // throw an exception if more moves can't be made    
    object GameOver extends Exception {}
    if (possibleMoves.isEmpty) throw GameOver
    
    possibleMoves.map(m => (m, minimize(Game.move(m, board)._1, Integer.MIN_VALUE, Integer.MAX_VALUE, maxDepth)))
                 .maxBy(_._2)._1
  }
  
  
  //evaluates the current board
  private def evaluate(board: List[Int]) = {
    val (smoothW, monoW, emptyW, maxW) = (0.1, 1.0, 2.7, 1.0)
    log(board.filter(_ == 0).length) * emptyW - Game.smoothness(board) * smoothW + 
    log(board.maxBy(a => a)) / log(2) * maxW + Game.mono2(board) * monoW
  }
}