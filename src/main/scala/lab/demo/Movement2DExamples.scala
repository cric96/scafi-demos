package lab.demo

import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.ScafiProgramBuilder
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Random
import it.unibo.scafi.simulation.s2.frontend.view.{ViewSetting, WindowConfiguration}
import it.unibo.scafi.space.Point2D
import it.unibo.scafi.space.Point3D.Zero
import lab.gui.patch.{ActionMovement, RadiusLikeSimulation}
import lab.lib.movement.Movement2DIncarnation._
import lab.lib.movement.{Movement2DIncarnation, _}

object MovementSimulation extends App {
  // -- simulation parameters --
  val distance = 150 // the range to consider two nodes a neighbour
  val howMany = 100 // nodes spawn in the simulation
  val width = 800 // simulated environment width
  val height = 600 // simulated environment height
  // -- aggregate program of simulation --
  val programClass = classOf[LeaderStrategy]
  // -- GUI parameters
  ViewSetting.labelFontSize = 5
  ViewSetting.windowConfiguration = WindowConfiguration(width, height) // WindowConfiguration() for a full screen panel
  // -- build and launch the simulation
  ScafiProgramBuilder (
    Random(howMany, width, height),
    SimulationInfo(program = programClass,
      metaActions = new ActionMovement :: Nil,
      exportEvaluations = List.empty
    ),
    RadiusLikeSimulation(distance),
    neighbourRender = false,
  ).launch()

}
/* utility to sense some "on-off" sensors*/
trait SensorFacade {
  self :  Movement2DProgram =>
  def sense1 : Boolean = sense("sens1")
  def sense2 : Boolean  = sense("sens2")
  def sense3 : Boolean = sense("sens3")
}
/// MOVEMENT EXAMPLES ///
/*
An example of how to enable the movement in ScaFi. The trait should extend Movement2DProgram and must define the velocity to apply at each node.
In this case, it returns a constant velocity (i.e. a field with (1.0, 1.0) in each point).
*/
class ConstantMovement extends Movement2DProgram {
  override def movementBody(): Velocity = (1.0, 1.0)
}
/*
  An example of how to use movement libraries. The program must be mixed in with the library selected and then, in the
  movement body, you should use movement methods. This class use clockwiseRotation (i.e. the aggregate rotate toward a
  position).
 */
class ClockwiseMovement extends Movement2DProgram with Movement2D {
  override def movementBody(): Velocity = clockwiseRotation((0.0, 0.0))
}
/*
  This example shows how to explore an area delimited by two points. This should be useful to find some resources in the space.
 */
class Wander extends Movement2DProgram with Steering with Movement2D {
  private val minBound : Point2D = Point2D(0.0, 0.0)
  private val maxBound : Point2D = Point2D(800.0, 600.0)
  override def movementBody(): Movement2DIncarnation.Velocity = explore(minBound, maxBound, 200)
}
/*
  This example is similar to the above, but this uses withSeparation method. Doings this, the nodes try to avoid each other
 */
class WanderWithSeparation extends Movement2DProgram with Steering with FlockLib with Movement2D {
  private val minBound : Point2D = Point2D(0.0, 0.0)
  private val maxBound : Point2D = Point2D(800.0, 600.0)
  private val distanceBetween : Double = 20.0
  override def movementBody(): Movement2DIncarnation.Velocity = withSeparation {
    explore(minBound, maxBound, 200)
  }(separationDistance = distanceBetween)
}
/*
  This example shows the flocking behaviour of the swarm. Try to change forces to see how the overall behaviour change.
 */
class FlockMovement extends Movement2DProgram with FlockLib {
  override def movementBody(): Movement2DIncarnation.Velocity = FlockBehaviour(
    attractionForce = 0.001,
    alignmentForce = 0.1,
    repulsionForce = 0.5,
    separationDistance = 10.0,
  ).run()
}
/*
  This example shows how to add behaviour to a standard flock behaviour. Check what happens if you enable sense2 in a part of the space.
  To enable sense2 you should select nodes and then click the number 2.
 */
class Concatenation extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock with SensorFacade {
  private val rotationWeight = 0.1
  private val goalWeight = 0.3
  private val obstacleWeight = 0.5
  private val target = Point2D(1000.0, 1000.0)
  def obstacle : Boolean = sense2
  override def movementBody(): Movement2DIncarnation.Velocity = FlockBehaviour()
    .addBehaviour(anticlockwiseRotation(target) * rotationWeight) // via pimping AdvancedFlock, this tend to rotate toward the goal
    .withGoal(target, goalWeight) //this move the aggregate toward the target
    .withObstacleAvoid(sense2, obstacleWeight) //this avoid a part of the computational field.
    .run()
}
/// MOVEMENT + AGGREGATE SYNTAX EXAMPLE ///
/*
  This example shows how to enable different movements in the same aggregate programs. You could use the SensorFacade to divide the field into different zones.
  Pay attention to the use of mux or branch. Branching divides the nodes in two different zones that don't speak together.
  Try to use branch and see what happen.
  P.S.
  The method standStill returns the null velocity vector (Velocity(0,0)).
 */
class MovementSelection extends Movement2DProgram with Movement2D with FlockLib with SensorFacade {
  override def movementBody(): Movement2DIncarnation.Velocity = mux(sense1) {
    flock()
  } {
    mux(sense2) { antiflock() } { standStill }
  }
}
/*
  This example shows how to mix the G block with movement. G can be used to broadcast data to the entire aggregate. The method broadcast internally uses G.
  G, in this case, is used to find a part of the field in which sens1 is enabled. When sens1 is founded by someone, their position is broadcast to
  the aggregate that tends to go in that position.
 */
class Coordination extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock
  with BlockG with SensorFacade  {

  override def movementBody(): Movement2DIncarnation.Velocity = {
    val existSense1 = G[Boolean](sense1, sense1, v => v, nbrRange) // or broadcast(sense1, sense1)
    val sense1Position = G[P](sense1, currentPosition(), v => v, nbrRange) // or broadcast (sense1, currentPosition())
    mux(existSense1) { goToPoint(sense1Position) } { standStill }
  }
}
/*
  This example shows how to elect a leader and then stay near him. It is possible using the S block, allowing to divide the space into zones and
  electing a leader.
  The result shows the creation of agglomerates near a node.
 */
class LeaderElection extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock
  with BlockC with BlockG with BlockT with BlockS {
  private val radius = 200 //the radius of the zone in which a leader is elected.
  private val separationDistance = 30.0
  override def movementBody(): Movement2DIncarnation.Velocity = {
    val leader = S(radius, nbrRange)
    withSeparation(goToPoint(broadcast(leader, currentPosition())))(separationDistance)
  }
}
/*
  This example shows how to use BlockT with movement.
  In this case, when the sense1 field is enabled, for a period, the aggregate exhibit a flock behaviour.
  After this period, the aggregate start to use antiflock behaviour. Try to use mux instead of branch.
  What happens? It was an expected result?
 */
class Temporal extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock with BlockT
  with SensorFacade {
  val period = 100
  override def movementBody(): Movement2DIncarnation.Velocity = {
    val antiflockVelocity : Velocity = antiflock()
    val flockVelocity : Velocity = flock()
    def onSenseOne() : Velocity = {
      mux(T(period) == 0) {
        antiflockVelocity
      } {
        flockVelocity
      }
    }
    branch(sense1) { onSenseOne() } { standStill }
  }
}
/*
  This example combines the leader election strategy with the broadcast strategy. This is an example of the self-organising
   coordination regions (SCR) pattern.
   (for all those are interested in this argument, a detailed explanation could be found here
   https://www.sciencedirect.com/science/article/pii/S0167739X20304775).
   The leader election is done as above. Then the data are collected into the leader with the Block C. In this example,
   the leader receives a sequence of position marked with the identifier of the node.
   After that, the leader computer the centroid of its region and broadcast it to the entire zones.
   Then, the nodes rotate towards this centroid.
 */
class LeaderStrategy extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock
   with BlockG with BlockT with BlockS with BlockC with StateManagement {
  private val radius = 200
  private val separation = 10.0
  override def movementBody(): Movement2DIncarnation.Velocity = {
    val leader = S(radius, nbrRange)
    val slaveData = C[Double, Seq[P]](distanceTo(leader), _ ++ _, Seq(currentPosition()), Seq.empty)
    val centroid = slaveData.fold(Zero)(_ + _) / slaveData.size
    val broadcastToSlave = broadcast(leader, centroid)
    anticlockwiseRotation(broadcastToSlave)
  }
}
/// TASKS ///
/**
  General tips: Try to solve the task with the simplest movement block (e.g. goToPosition, explore,..). When you have a solution,
  integrate it with more advanced movement behaviour (flock, with separation...).
*/
/*
  TASK 1: find food
  The nodes must search for the food node in space.
  Once found, its position must be sent to all the nodes of the aggregate that will have to get closer to it.
  A "food node" is a node with the sense3 enabled (type 3 on selection area).
  Try to treat the "food node" as not a part of the aggregate, namely avoid to do something like this broadcast(isFood, currentPosition).
  The food node must be found by other "drone" via nbr operator.
  Tips: try to use the block G.
 */
class FindFood extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock with Steering
  with SensorFacade with BlockC with BlockS with BlockT {
  private def isFood : Boolean = sense3
  override def movementBody(): Movement2DIncarnation.Velocity = {
    def droneBehaviour() : Velocity = Velocity.Zero //TODO
    val droneResult = droneBehaviour()
    branch(isFood) {
      standStill
    } {
      droneResult
    }
  }
}
/*
  TASK 2: find food and leave
  like the above, but this time, when a node is near to the food, it remains in the position for a fixed period.
  After this, the node should leave the target zone, allowing other nodes to reach the food.
  Tips: Try to use block T and branch operator.
 */
class FindFoodAndLeave extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock with Steering
  with SensorFacade with BlockC with BlockS with BlockT {
  private def isFood : Boolean = sense3
  private val thr = 25.0 //the distance needed to "eat" the food
  private val eatTime = 10 //the time needed to consume the food
  override def movementBody(): Movement2DIncarnation.Velocity = {
    def droneBehaviour(): Velocity = Velocity.Zero //TODO
    val droneResult = droneBehaviour()
    branch(isFood) {
      standStill
    } {
      droneResult
    }
  }
}
/*
  TASK 3 drone rescue
  Some nodes act as a base (sensor 3), to which drones notify if they have
  a break (nodes with sensor 2 enabled). When the base receives the position of a
  "broken" drone, it orders nearby drone to go to that position to "rescue" it (
  i.e. they stay nearby the broken node).
  Tips: try to adapt SCR pattern..
 */
class RescueDrone extends Movement2DProgram with Movement2D with FlockLib with AdvancedFlock with Steering
  with SensorFacade with BlockC with BlockS with BlockT {
  private def isBase : Boolean = sense3
  private def isInjured : Boolean = sense2
  override def movementBody(): Movement2DIncarnation.Velocity = {
    def droneBehaviour(): Velocity = Velocity.Zero //TODO
    val droneResult = droneBehaviour()
    branch(isBase || isInjured) {
      standStill
    } {
      droneResult
    }
  }
}
