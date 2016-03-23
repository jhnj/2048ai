/** Exempel på swing användning **/




package g2048.model
//create a new folder with just swing.scala
//scalac swing.scala && scala Draw
//This application generates a bunch of random points
//after clicking you'll see them all move down and disappear

//I hope this helps those new to swing.
//This could serve as a starting ground to make a game.
//Have fun!
//license: public domain

import scala.swing._
import swing._
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.event._
import util.Random

//Thanks to:
//http://stackoverflow.com/questions/20459704/scala-swing-repaint
//http://stackoverflow.com/questions/15862769/periodic-repaint-of-scala-swing-simpleswingapplication

object grid {
  val width = 800
  val height = 300
  var data = Array.fill(width,15)(Random.nextInt(height))  

  def update = {
    data = data.map(_.map(_+1).filter(_ < height))
  }
}

object Draw extends SimpleSwingApplication {
  var toggle = 0

  var noise = new DataPanel {
        preferredSize = new Dimension(grid.width, grid.height)
  }

  def dostuff = {
    grid.update
    noise.repaint()
  }

  def top = new MainFrame {
    val timer = new javax.swing.Timer(5, Swing.ActionListener(e =>
    {
      dostuff
    }))
    val button = Button("Stop/Start") {
      toggle = (toggle + 1)%2
      if (toggle == 1) timer.start() else timer.stop()
    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents += noise
      contents += button
    }
  }
}

class DataPanel extends Panel {

  override def paintComponent(g: Graphics2D) {
    g.setColor(new Color(255,255,255))
    g.clearRect(0, 0, size.width, size.height)
    g.setColor(new Color(0,0,0))
    grid.data.zipWithIndex.map(t => 
                          t._1.map(i => 
                          g.drawLine(t._2, i, t._2, i)))
    //
    // for (i <- 0 to data.length -1) {
    //   for (j <- 0 to data(i).length -1) {
    //      x = data(i)(j)
    //     g.drawLine(i, x, i, x)
    //   }
    // }
  }
}