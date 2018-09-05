package scalagl.programs

import java.lang.Math.{cos, sin}

import cats.Monad
import cats.implicits._
import org.scalajs.dom.ext.KeyCode.{Down, Up, Left => L, Right => R}
import org.scalajs.dom.raw.WebGLTexture
import scalagl.algebra.{Rgba => _, _}
import scalagl.interpreters.{InitializationOptions, RenderContext}
import scalagl.math.Util.Rgba
import scalagl.math.{Matrix4, Vector4}

import scala.collection.Set
import scala.collection.immutable.SortedMap

object EngineProgram {

  val rotationSpeed = 0.06f
  val speed = 0.08f
  val offsetY = 0.85f
  val offsetZ = 0.4f

  implicit class KeysOps(val keys: Set[Key]) extends AnyVal {
    def isPressed(keyCode: Int): Boolean = keys.contains(Key(keyCode))
  }

  def initial[Tex](textures: SortedMap[String, Tex]): RenderOutput[Tex] = {

    val eye = Matrix4.rotateAround(0, 0, 0, offsetY, offsetZ)
    val camera = Camera(eye, Vector4(0, 0, 0, 0))
    val track = RenderObject(
      pos = Pos2D(10, -9),
      height = 30,
      width = 30,
      rotation = 0,
      modeSeven = false,
      tex = textures("track"),
      behaviour = (old: RenderObject[Tex], keys) => old)
    val car = RenderObject(
      Pos2D(0, 0),
      -0.7f,
      0.75f,
      0,
      true,
      textures("car"), { (old: RenderObject[Tex], keys) =>
        if (keys.isPressed(Up) ^ keys.isPressed(Down)) {
          val ahead = if (keys.isPressed(Up)) 1.0f else -1.0f
          val newObj =
            if (keys.isPressed(L)) old.copy(rotation = old.rotation + (ahead * rotationSpeed))
            else if (keys.isPressed(R)) old.copy(rotation = old.rotation - (ahead * rotationSpeed))
            else old

          val dx = ahead * speed * sin(newObj.rotation).toFloat
          val dy = ahead * speed * -cos(newObj.rotation).toFloat

          newObj.copy(pos = newObj.pos.copy(x = newObj.pos.x + dx, y = newObj.pos.y + dy))
        } else old
      }
    )
    RenderOutput(camera, List(track, car))
  }

  def updateCam(old: Camera, keys: Set[Key]): Camera =
    if (keys.isPressed(Up) ^ keys.isPressed(Down)) {
      val ahead = if (keys.isPressed(Up)) 1.0f else -1.0f
      val newCam =
        if (keys.isPressed(L)) {
          val eye =
            Matrix4.rotateAround(old.lookAt.x, old.lookAt.y, old.lookAt.w + (ahead * rotationSpeed), offsetY, offsetZ)
          Camera(eye, Vector4(old.lookAt.x, old.lookAt.y, 0, old.lookAt.w + (ahead * rotationSpeed)))
        } else if (keys.isPressed(R)) {
          val eye =
            Matrix4.rotateAround(old.lookAt.x, old.lookAt.y, old.lookAt.w - (ahead * rotationSpeed), offsetY, offsetZ)
          Camera(eye, Vector4(old.lookAt.x, old.lookAt.y, 0, old.lookAt.w - (ahead * rotationSpeed)))
        } else
          old

      val dx = ahead * speed * sin(newCam.lookAt.w).toFloat
      val dy = ahead * speed * -cos(newCam.lookAt.w).toFloat

      newCam.copy(
        lookAt = newCam.lookAt.copy(x = newCam.lookAt.x + dx, y = newCam.lookAt.y + dy),
        camPos = newCam.camPos.copy(x = newCam.camPos.x + dx, y = newCam.camPos.y + dy))

    } else old

  def simpleRace[F[_] : Monad, E](R: RenderEngine[F, InitializationOptions, E, RenderContext, WebGLTexture]): F[Unit] =
    R.initialize(
      options = InitializationOptions(
        backgroundColor = Rgba(0, 0, 0.12, 1.0),
        textures = SortedMap("car" -> "car.png", "track" -> "racetrack.png")))
      .flatMap(_.traverse_(c => R.renderLoop(c, initial(c.textures), updateCam)))
}
