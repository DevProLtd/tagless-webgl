package scalagl

import scalagl.interpreters._
import scalagl.programs.EngineProgram

object Main {

  def main(args: Array[String]): Unit = 
    EngineProgram
      .simpleRace(
        new RenderEngineInterpreterDrawImage(
          new DrawImageInterpreterWebGL(GLInterpreterIO),
          DomInterpreterIO))
      .unsafeRunAsync {
        case Left(t) => t.printStackTrace()
        case _ => ()
      }

}
