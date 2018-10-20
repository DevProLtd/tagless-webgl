package scalagl.interpreters

import cats.Monad
import cats.implicits._
import org.scalajs.dom.raw.{WebGLProgram, WebGLTexture}
import scalagl.algebra._
import scalagl.math.Util.Rgba
import scalagl.math.{Matrix4, Quaternion, Vector4}

import scala.collection.immutable.SortedMap
import scala.collection.{Set, mutable}
import scala.language.higherKinds

case class InitializationOptions(backgroundColor: Rgba, textures: SortedMap[String, String])

sealed trait InitializationError

case class VertexShaderError(e: String) extends InitializationError

case class FragmentShaderError(e: String) extends InitializationError

case class ProgramLinkingError(e: String) extends InitializationError

object InitializationError {
  def vertex(e: String): InitializationError = VertexShaderError(e)

  def fragment(e: String): InitializationError = FragmentShaderError(e)

  def program(e: String): InitializationError = ProgramLinkingError(e)
}

case class RenderContext(
      program: WebGLProgram,
      textures: SortedMap[String, WebGLTexture],
      projection: Matrix4,
      initializationOptions: InitializationOptions)

class RenderEngineInterpreterDrawImage[F[_]: Monad](W: DrawImage[F], D: Dom[F])
    extends RenderEngine[F, InitializationOptions, InitializationError, RenderContext, WebGLTexture] {

  val vertSrc =
    """
      |attribute vec4 a_position;
      |attribute vec2 a_texcoord;
      |
      |uniform mat4 u_matrix;
      |
      |varying vec2 v_texcoord;
      |
      |void main() {
      |   gl_Position = u_matrix * a_position;
      |   v_texcoord = a_texcoord;
      |}
    """.stripMargin

  val fragSrc =
    """
      |precision mediump float;
      |
      |varying vec2 v_texcoord;
      |
      |uniform sampler2D u_texture;
      |
      |void main() {
      |   gl_FragColor = texture2D(u_texture, v_texcoord);
      |   if(gl_FragColor.a < 0.2)
      |     discard;
      |}
    """.stripMargin

  def initialize(options: InitializationOptions): F[Either[InitializationError, RenderContext]] =
    for {
      canvas <- W.createFullSizeCanvas()
      _      <- D.appendToBody(canvas)

      projection = Matrix4.forPerspective(90, canvas.width / canvas.height, 0.3f, 100f)

      fragmentShader <- W.compileFragmentShader(fragSrc)
      vertexShader   <- W.compileVertexShader(vertSrc)

      program <- (
        vertexShader.leftMap(InitializationError.vertex),
        fragmentShader.leftMap(InitializationError.fragment))
        .mapN((v, f) => W.createProgram(v, f).map(_.leftMap(InitializationError.program)))
        .flatSequence

      images   <- options.textures.traverse(D.createImageElement)
      textures <- images.traverse(W.createTextureInfo)

    } yield
      program.map(RenderContext(_, textures, projection, options))


  def renderLoop(c: RenderContext, seed: RenderOutput[WebGLTexture], f: (Camera, Set[Key]) => Camera): F[Unit] = {

    val keySet = mutable.Set.empty[Key]

    val loop = D.renderLoop(
      seed, { (previous: RenderOutput[WebGLTexture], time) =>
        val camera = f(previous.c, keySet)

        val view = Matrix4.setLookAtM(
          eyeX = camera.camPos.x,
          eyeY = camera.camPos.y,
          eyeZ = camera.camPos.z,
          centerX = camera.lookAt.x,
          centerY = camera.lookAt.y,
          centerZ = camera.lookAt.z,
          upX = 0,
          upY = 0,
          upZ = 1
        )

        val projectionView = c.projection * view

        def updated = RenderOutput(camera, previous.objects.map(o => o.behaviour(o, keySet)))

        def draw(r: RenderObject[WebGLTexture]): F[Unit] = {
          val translate = Matrix4.forTranslation(Vector4(x = r.pos.x, y = r.pos.y, z = -1.0f, w = 0))
          val rotation = Matrix4.setRotationRad(rad = r.rotation, dirX = 0, dirY = 0, dirZ = 1)

          val rotationX =
            if (r.modeSeven) Matrix4.forRotation(Quaternion(x = -0.4f, y = 0, z = 0, w = 0.7f))
            else Matrix4.identity

          val scale = Matrix4.forScale(Vector4(r.width, r.height, 1, 0))
          val model = translate * rotation * rotationX * scale
          val matrix = projectionView * model

          W.drawImage(c.program, r.tex, matrix)
        }

        val bg = c.initializationOptions.backgroundColor
        for {
          _ <- W.clearScreen(bg.r, bg.g, bg.b, 1.0)
          _ <- updated.objects.traverse_(draw)
        } yield updated
      }
    )

    D.onKeyDown(e => Monad[F].pure(keySet add Key(e.keyCode))) *>
      D.onKeyUp(e => Monad[F].pure(keySet remove Key(e.keyCode))) *>
      loop
  }
}
