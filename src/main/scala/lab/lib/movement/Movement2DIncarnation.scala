package lab.lib.movement

import it.unibo.scafi.incarnations.BasicAbstractIncarnation
import it.unibo.scafi.lib.StandardLibrary

object Movement2DIncarnation extends MovementLibrary with BasicAbstractIncarnation with StandardLibrary{

  trait Scafi2DSupport extends MovementSupport {
    override def move(velocity: Velocity): Velocity = velocity
  }

  trait Movement2DProgram extends MovementProgram with Scafi2DSupport {}

  override implicit val idBounded: Movement2DIncarnation.Builtins.Bounded[Int] = Builtins.Bounded.of_i
  override type Time = Double
}
