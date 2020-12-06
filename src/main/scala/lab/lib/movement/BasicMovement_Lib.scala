package lab.lib.movement

import it.unibo.scafi.space.{Point2D, Point3D}

/**
 * A collection of movement behaviours that don't need any type of coordination between agents.
 */
trait BasicMovement_Lib {
  self : MovementLibrary.Subcomponent =>
  /**
   * AggregateProgram component used to defines movement behaviours useful for:
   *  - reach some position in the space;
   *  - stand still;
   *  - rotate toward a point.
   *  for using this component, you should mix it with an AggregateProgram:
   *
   *  class ProgramA extends AggregatePrograms with StandardSensor with Movement2D
   *  -- OR --
   *  class ProgramA extends Movement2DProgram with Movement2D
   *
   *  The output of each method return unit vector (i.e. a vector with unitary module).
   */
  trait Movement2D {
    self : FieldCalculusSyntax with StandardSensors =>
    /**
     * It rotates the node toward a point in a clock wise manner.
     *  X                      X
     *     O     =clockwise=>  0
     *        Y                Y
     * @param center position the center of the rotation (O in the example above)
     * @return the velocity that rotates the node accordingly
     */
    def clockwiseRotation(center : P) : Velocity = {
      val centerVector = currentPosition() - center
      Point2D(-centerVector.y, centerVector.x).normalized
    }
    /**
     * It rotates the node toward a point in an anticlock wise manner.
     *  X
     *     O     =anticlockwise=>  X  0  Y
     *        Y
     * @param center center of the rotation (O in the example above)
     * @return the velocity that rotates the node accordingly
     */
    def anticlockwiseRotation(center : P) : Velocity = - clockwiseRotation(center)

    /**
     * It moves the node toward a position.
     *
     * X _ _ O  =goToPoint=> _ X _ O
     * @param point the desired position.
     * @return the velocity the move the node toward the desired position
     */
    def goToPoint(point : P) : Velocity = (point - currentPosition()).normalized

    /**
     * _ X _  =standStill=> _ X _
     * @return a null vector (each component is equal to zero)
     */
    val standStill : Velocity = Velocity.Zero
  }

  /**
   * Movement library built on top of Movement2D that enables some complex behaviours.
   * Currently, this library defines only the "explore" behaviour.
   */
  trait Steering {
    self : Movement2D with FieldCalculusSyntax with StandardSensors =>
    /**
     * It defines the logic for exploring a specified area delimited by two coordinates.
     * Internally, it produces a random coordinate inside the box and then computes a velocity
     * that go toward that position. After trajectoryTime (or if the node reaches the position)
     * this methods computes another random position.
     *  (minX, minY) ---------|
     *      |      explore    |
     *      |        area     |
     *      |_ _ _ _ _ _ (maxX, maxY=
     * @param minCoord the top left area limit
     * @param maxCoord the bottom right area limit
     * @param trajectoryTime define how long the node maintains a direction.
     * @param reachGoalRange the threshold to consider the position reached
     * @return the unit vector that brings the node to explore the area.
     */
    def explore(minCoord : P, maxCoord : P, trajectoryTime : Int, reachGoalRange : Double = 0) : Velocity = {
      require(trajectoryTime > 0)
      def randomCoord : Point2D = Point2D(
          minCoord.x + (math.random() * (maxCoord.x - minCoord.x)),
          minCoord.y + (math.random() * (maxCoord.y - minCoord.y)))
      val (_, _, velocity) = rep((randomCoord, trajectoryTime, Point3D.Zero)){
        case (_, decay, v) if (decay == 0) => (randomCoord, trajectoryTime, v)
        case (goal, _, _) if (goal.distance(currentPosition()) < reachGoalRange)=> (goal, 0, goToPoint(goal))
        case (goal, decay, _) => (goal, decay - 1, goToPoint(goal))
      }
      velocity
    }
  }
}
