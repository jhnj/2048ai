package g2048.model
import scala.util.Random
import scala.math._

object Game {
  
  val random = new Random
  
  //prints the board for debugging purposes
  def printBoard(l: List[Int]) = {
    for (line <- l.grouped(4)) println(line.mkString("\t"))
  }
  
  //Returns the columns in the order down to up ↑
  /*private*/ def down(nums: List[Int]): List[List[Int]] = {
    (for {
      x <- 0 to 3
      y <- x to 15 by 4
    } yield (nums(y))).toList.grouped(4).toList.map(_.reverse)
  }
  
  //Returns the columns in the order up to down ↓
  /*private*/ def up(nums: List[Int]): List[List[Int]] = {
    (for {
      x <- 0 to 3
      y <- x to 15 by 4
    } yield (nums(y))).toList.grouped(4).toList
  }
  
  //Returns the rows from right to left  <-
  /*private*/ def right(nums: List[Int]): List[List[Int]] = nums.grouped(4).toList.map(_.reverse)
  
  //Returns the rows from left to right  ->
  /*private*/ def left(nums: List[Int]): List[List[Int]] = nums.grouped(4).toList

  
  //Combines a list(row or column) in the direction <-
  def combine(nums: List[Int]): (List[Int], Int) = {
    def calc(nextNums: List[Int], returnNums: List[Int], score: Int): (List[Int], Int) = {
      nextNums.filter(_ != 0) match {
        case List() => (returnNums.padTo(4, 0), score)
        case x :: xs => 
          if (xs.isEmpty || x != xs.head) calc(xs, returnNums :+ x, score)
          else calc(xs.tail, returnNums :+ (x + xs.head), score + x + xs.head)
      }
    }
  calc(nums, List(), 0)
  }
  
  def move(direction: String, board: List[Int]): (List[Int], Int) = {
    val newBoard = (for (x <- direction match {
                              case "up" => this.up(board)
                              case "down" => this.down(board)
                              case "left" => this.left(board)
                              case "right" => this.right(board)
    }) yield (combine(x))).unzip
    
    direction match {
      case "up" => (up(newBoard._1.flatten).flatten, newBoard._2.sum)
      case "down" => (down(newBoard._1.flatten).flatten.reverse, newBoard._2.sum)
      case "left" => (left(newBoard._1.flatten).flatten, newBoard._2.sum)
      case "right" => (right(newBoard._1.flatten).flatten, newBoard._2.sum)
    }
  }
  
  def addRandom(board: List[Int]) = {
    def randomEmpty: Int = {
      val ind = this.random.nextInt(16)
      if (board(ind) == 0) ind
      else randomEmpty
    }
    val b = board.toArray
    b(randomEmpty) = if (random.nextInt(10) > 8) 4 else 2
    b.toList
  }
  
  def gameOver(board: List[Int]): Boolean = {
    Array("up", "down", "left", "right").forall(x => move(x, board)._1 == board)
  }
  //checks if a move is possible(ie, anything happens)
  def possibleMove(direction: String, board: List[Int]) = move(direction, board)._1 != board
  
  
  def turn(direction: String, board: List[Int]): (List[Int], Int, Boolean) = {
    val over = gameOver(board)
    val newState = move(direction, board)
    if (newState._1 == board) (board, 0, over)
    else (addRandom(newState._1), newState._2, over)
  }
  //the boards smoothness(if the values would indicate height)
  //log2 (how many moves are required to combine 2 pieces
  def smoothness(board: List[Int]) = {
    //the difference between 2 values log2
    def difference(m: Int, n: Int) = {
      abs(log(m)/log(2) - log(n)/log(2))
    }
    
    (for (
      x <- 0 until 4
    ) yield ({
    	def inB(n: Int) = n >= 0 && n < 4
      def next(indX: Int, newIndX: Int, row: List[Int]): Double = {
    	  if (row(indX) == 0) 0
    	  else if (!inB(newIndX + 1)) 0
        else if (row(newIndX + 1) != 0) difference(row(indX), row(newIndX + 1))
        else next(indX, newIndX + 1, row)
      }
    	
    	val up = Game.up(board).map(next(x, x, _))
    	val down = Game.down(board).map(next(x, x, _))
    	val left = Game.left(board).map(next(x, x, _))
    	val right = Game.right(board).map(next(x, x, _))
    	
    	up.sum + down.sum + left.sum + right.sum
    })).sum
     
  }
  
  def mono2(board: List[Int]) = {
    val dValues = Array(0.0,0.0,0.0,0.0)
    //the difference between 2 values log2
    def difference(m: Int, n: Int) = {
      if (m > n)
        1//log(m)/log(2) - log(n)/log(2)
      else
        0
    }
    
    for (
      x <- 0 until 4
    ) {
    	def inB(n: Int) = n >= 0 && n < 4
      def next(indX: Int, newIndX: Int, row: List[Int]): Double = {
    	  if (row(indX) == 0) 0
    	  else if (!inB(newIndX + 1)) 0
        else if (row(newIndX + 1) != 0) difference(row(indX), row(newIndX + 1))
        else next(indX, newIndX + 1, row)
      }
    	
    	val up = Game.up(board).map(next(x, x, _))
    	val down = Game.down(board).map(next(x, x, _))
    	val left = Game.left(board).map(next(x, x, _))
    	val right = Game.right(board).map(next(x, x, _))
    	
    	dValues(0) += up.sum
    	dValues(1) += down.sum
    	dValues(2) += left.sum
    	dValues(3) += right.sum
    }
    max(dValues(0), dValues(1)) + max(dValues(2), dValues(3)) 
  }
  
  
  def islands(board: List[Int]) = {
    
  }
}