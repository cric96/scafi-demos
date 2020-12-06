package lab.lib.movement

import it.unibo.scafi.incarnations.Incarnation
import it.unibo.scafi.space.Point3D

/**
 * A collection of movement behaviours integrated with Aggregate Programming
 */
trait MovementLibrary extends BasicMovement_Lib with Flock_Lib {
  self : Incarnation =>
  override type P = Point3D
  type Velocity = P //type alias, to improve readability
  /* companion object for velocity creation */
  object Velocity {
    val Zero = new Velocity(0,0,0)
    def apply(x : Double, y : Double) : Velocity = new Velocity(x, y, 0)
  }
  /**
   * device dependent, it could produce a side effect. Each incarnation define how to move nodes actually.
   */
  trait MovementSupport {
    /**
     * Effectively move the node in which the aggregate computation happens.
     * @param velocity vector that directs the node
     * @return the velocity vector actually applied (the node could make some transformations before using it to change its velocity)
     */
    def move(velocity: Velocity) : Velocity
  }
  /**
   * A class of AggregateProgram used to move nodes
   */
  trait MovementProgram extends AggregateProgram with StandardSensors {
    self : MovementSupport =>
    override final def main() : Velocity = move(movementBody())

    /**
     * This method produces the velocity that will move the node
     */
    def movementBody() : Velocity
  }
}

object MovementLibrary {
  //dependency
  type Subcomponent = Incarnation with MovementLibrary
}
