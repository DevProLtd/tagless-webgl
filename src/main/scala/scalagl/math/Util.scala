package scalagl.math

object Util {

  case class Rgba(r:Double, g: Double, b: Double, a: Double)

  def clamp(value: Float, min: Float, max: Float): Float =
    math.min(math.max(value, min), max)

  def lerp(from: Float, to: Float, t: Float): Float =
    from * (1 - t) + to * t
}
