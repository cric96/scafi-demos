package lab.demo

import it.unibo.scafi.space.Point2D
import lab.lib.movement.Movement2DIncarnation._
import lab.lib.movement.{Movement2DIncarnation, _}

trait SensorFacade {
  self :  Movement2DProgram =>
  def sense1 = sense[Boolean]("sens1")
  def sense2 = sense[Boolean]("sens2")
  def sense3 = sense[Boolean]("sens3")
}

class ConstantMovement extends Movement2DProgram {
  override def movementLogic(): Velocity = (1.0, 1.0)
}

class BasicMovement extends Movement2DProgram with Movement2D {
  override def movementLogic(): Velocity = clockwiseRotation((0.0, 0.0))
}

class FlockMovement extends Movement2DProgram with FlockLib {
  override def movementLogic(): Movement2DIncarnation.Velocity = flock()
}

class MovementSelection extends Movement2DProgram with Movement2D with FlockLib with SensorFacade {
  override def movementLogic(): Movement2DIncarnation.Velocity = mux(sense1) {
    flock()
  } { mux(sense2) {
    antiflock()
  } {
    standStill
  }}
}


class Concatenation extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock {
  override def movementLogic(): Movement2DIncarnation.Velocity = FlockBehaviour().withGoal(Point2D(100.0, 100.0), 0.1).run()
}

