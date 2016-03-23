package gui
import scala.swing._
import scala.swing.event._
import java.awt.Graphics2D
import java.awt.Color
import java.awt.Font
import g2048.model._

class GamePanel(var board: List[Int]) extends GridPanel(4*60, 4*60){
  
  //starts a new game
  def newGame = {
    board = Game.addRandom((for(i <- 0 until 16) yield 0).toList)
    totalScore = 0
    isOver = false
    this.repaint
  }
  
  private var totalScore = 0
  def score = totalScore
  private var isOver = false
  def gameOver = isOver
  
  override def paintComponent(g: Graphics2D): Unit = {
    g.setColor(Color.DARK_GRAY)
    g.setFont(new Font ("Helvetica", Font.BOLD, 18))
    g.fillRect(0, 0, 240, 240)
    for (x <- 0 until 4; y <- 0 until 4) {
      val value = board(x + 4 * y)
      g.setColor(getColor(value))
      //g.drawRect(x * 60, y * 60, 60, 60) 
      g.fillRect(x * 60 + 5, y * 60 + 5, 50, 50)
      g.setColor(Color.BLACK)
      if (value != 0) g.drawString(value.toString, x * 60  + 30 - value.toString.length * 5, y * 60  + 35)
    }
    
  }
  this.repaint
  
  def getColor(value: Int) = value match{
    case 2 => Color.RED
    case 4 => Color.GREEN
    case 8 => new Color(255, 255, 170)
    case 16 => new Color(255, 255, 128)
    case 32 => new Color(255, 255, 85)
    case 64 => new Color(255, 255, 42)
    case 128 => new Color(213, 213, 0)
    case 256 => new Color(170, 170, 0)
    case 512 => new Color(128, 128, 0)
    case 1024 => new Color(85, 85, 0)
    case x if x >= 2048 => new Color(43, 43, 0)
    case _ => Color.LIGHT_GRAY
  }
  
  def key(key: String) = {
    val newState = Game.turn(key, board)
      if (!newState._3) {
        board = newState._1
        totalScore += newState._2
        Main.score.text = "score: " + totalScore.toString
      } else {
        this.isOver = true
        Main.gameOver
      }
      this.repaint
  }
  
  def runAI = {
    key(Ai.getMove(board, 6))
    this.repaint
  }
  
  listenTo(keys)
  
  this.reactions += {
    case KeyPressed(_, Key.Right, _, _) => key("right")
    case KeyPressed(_, Key.Left, _, _) => key("left")
    case KeyPressed(_, Key.Up, _, _) => key("up")
    case KeyPressed(_, Key.Down, _, _) => key("down")
  }
  
  focusable = true
  this.requestFocus()
  
}