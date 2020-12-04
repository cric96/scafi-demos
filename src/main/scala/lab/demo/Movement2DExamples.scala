package lab.demo

import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.ScafiProgramBuilder
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Random
import it.unibo.scafi.simulation.s2.frontend.view.ViewSetting
import it.unibo.scafi.space.Point2D
import it.unibo.scafi.space.Point3D.Zero
import lab.gui.patch.{ActionMovement, RadiusLikeSimulation}
import lab.lib.movement.Movement2DIncarnation._
import lab.lib.movement.{Movement2DIncarnation, _}
object MovementSimulation extends App {
  val distance = 150
  val howMany = 100
  val width = 800
  val height = 600

  val programClass = classOf[RescueDrone]

  val movementSimulation = SimulationInfo(program = programClass,
    metaActions = new ActionMovement :: Nil,
    exportEvaluations = List.empty
  )

  ViewSetting.labelFontSize = 5

  ScafiProgramBuilder (
    Random(howMany, width, height),
    movementSimulation,
    RadiusLikeSimulation(distance),
    neighbourRender = false,
  ).launch()

}
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

class Wander extends Movement2DProgram with Steering with Movement2D {
  override def movementLogic(): Movement2DIncarnation.Velocity = explore(Point2D(0,0), Point2D(500, 500), 100)
}
class FlockMovement extends Movement2DProgram with FlockLib {
  override def movementLogic(): Movement2DIncarnation.Velocity = FlockBehaviour(
    attractionForce = 0.001,
    alignmentForce = 0.1,
    repulsionForce = 0.5,
    separationDistance = 10.0
  ).run()
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

//TASK 1: find food
class FindFood extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock with Steering
  with SensorFacade with BlockC with BlockS with BlockT {
  def isFood = sense3

  def goTo(destination : P) : Velocity = withSeparation(goToPoint(destination) * 0.5)(10.0)

  override def movementLogic(): Movement2DIncarnation.Velocity = {
    val (foodFound, foodPosition) = rep[(Boolean, P)]((false, currentPosition())){
      case (found, position) => foldhoodPlus((found, position)) {
        case (acc @ (_, _), neigh @ (neighFound, _)) => mux(neighFound) { neigh } { acc }
      } {
        (nbr(isFood), nbr(currentPosition()))
      }
    }

    val globalFoodPosition = { broadcast(foodFound, foodPosition) }
    val foundInAnyPlace = { broadcast(foodFound, foodFound) }

    mux(foundInAnyPlace) {
      goTo(globalFoodPosition)
    } {
      standStill
    }
  }
}

//TASK 2: find food and leave
class FindFoodAndLeave extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock with Steering
  with SensorFacade with BlockC with BlockS with BlockT {
  def isFood = sense3
  val thr = 25.0
  val eatTime = 10
  def seek(destination : P) : Velocity = withSeparation(goToPoint(destination) * 0.5)(10.0)
  def flee(destination : P) : Velocity = withSeparation(goToPoint(-destination) * 0.5)(separationDistance = 10.0)
  override def movementLogic(): Movement2DIncarnation.Velocity = {

    val (foodFound, foodPosition) = rep[(Boolean, P)]((false, currentPosition())){
      case (found, position) => foldhood((found, position)) {
        case (acc @ (_, _), neigh @ (neighFound, _)) => mux(neighFound) { neigh } { acc }
      } {
        (nbr(isFood), nbr(currentPosition()))
      }
    }

    val globalFoodPosition = { broadcast(foodFound, foodPosition) }
    val foundInAnyPlace = { broadcast(foodFound, foodFound) }
    val goal =  seek(globalFoodPosition)
    val awayFromGoal = flee(globalFoodPosition)

    def droneBehaviour(): Velocity = mux(foundInAnyPlace) {
      val reached = rep(false)(v => (v || currentPosition().distance(globalFoodPosition) < thr) && foundInAnyPlace)
      branch(reached) {
        mux(T(eatTime) == 0) { awayFromGoal } { goal }
      } {
        goal
      }
    } {
      standStill
    }

    branch(isFood) {
      standStill
    } {
      droneBehaviour()
    }
  }
}

//TASK 3 rescue drone..
class RescueDrone extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock with Steering
  with SensorFacade with BlockC with BlockS with BlockT {
  def isBase = sense3
  def isInjured = sense2

  val thr = 25.0
  val eatTime = 10
  val grain = 10

  def seek(destination : P) : Velocity = withSeparation(goToPoint(destination) * 0.5)(10.0)
  def flee(destination : P) : Velocity = withSeparation(goToPoint(-destination) * 0.5)(separationDistance = 10.0)

  override def movementLogic(): Movement2DIncarnation.Velocity = {
    val leader = branch(isBase) { S(grain, nbrRange) } { false }
    val potential = distanceTo(leader, nbrRange)
    val collect = C[Double, P](potential, (acc, other) => mux(positionOrdering.gt(acc, other)) { acc } { other }, mux(isInjured) { currentPosition()} { Zero }, Zero )
    val injuredPosition = broadcast(leader, collect)

    branch(isBase || isInjured) {
      standStill
    } {
      mux(injuredPosition.isZero) {
        standStill
      } {
        seek(injuredPosition)
      }
    }

  }
}
