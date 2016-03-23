package gui
import scala.swing._
import scala.swing.event._
import javax.swing.Timer

object Main extends SimpleSwingApplication{
  val frame = new Frame
  def top = frame
  
  frame.visible = true
  frame.title = "2048"
  frame.minimumSize = new Dimension(300, 355)
  
  
  val Game = new GamePanel(List(2,4,4,2,0,0,4,0,2,2,8,4,2,0,0,8))
  Game.minimumSize_=(new Dimension(240, 240))
  
  val newGame = new Button("New game") {
    reactions += {
      case ButtonClicked(_) => {
        Game.newGame
        Game.focusable = true
        Game.requestFocus()
      }
    }
  }
  val timer: Timer = new Timer(1, Swing.ActionListener(e =>
    {
    	try {
    	  if (Game.gameOver) timer.stop
    	  else Game.runAI
//    	  println("moved")
    	} catch {
    	  case e: MatchError => timer.stop
    	  case e: Exception => timer.stop
    	}
    }))
  
  val Ai = new Button("AI") {
    reactions += {
      case ButtonClicked(_) => {
        timer.start
      }
    }
  }
  
  val score = new Label("score: ")
//  println(newGame.preferredSize)
  
  frame.contents =  new BoxPanel(Orientation.Vertical) {
      contents += Game
      contents += newGame
      contents += Ai
      contents += score
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }
  
  Game.requestFocus
  
  def gameOver = {
    println("game over")
    Game.focusable = false
  }
  
  
}