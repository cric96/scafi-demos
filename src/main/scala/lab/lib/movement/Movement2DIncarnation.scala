package lab.lib.movement

import it.unibo.scafi.incarnations.BasicAbstractIncarnation
import it.unibo.scafi.lib.StandardLibrary

/**
 * A ScaFi incarnation that enable movement in euclidean 2D space. The actual movement is made externally by this incarnation.
 */
object Movement2DIncarnation extends MovementLibrary with BasicAbstractIncarnation with StandardLibrary {
  /** No internal changes are made, the support returns only the velocity computed */
  trait Scafi2DSupport extends MovementSupport {
    override def move(velocity: Velocity): Velocity = velocity
  }
  /** A class of MovementProgram for moving nodes */
  trait Movement2DProgram extends MovementProgram with Scafi2DSupport {}
  //needed by ScaFi for some internal implementation
  override implicit val idBounded: Movement2DIncarnation.Builtins.Bounded[Int] = Builtins.Bounded.of_i
  override type Time = Double
}
