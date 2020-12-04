package lab.lib.movement

import it.unibo.scafi.space.Point3D
/**
 * Flocking behaviour from Craig Reynolds (https://www.red3d.com/cwr/index.html)
 * implementation taken from https://gamedevelopment.tutsplus.com/tutorials/3-simple-rules-of-flocking-behaviors-alignment-cohesion-and-separation--gamedev-3444
 * other link : https://www.red3d.com/cwr/boids/
 * This is a library to define flocking like behaviour in the Aggregate Computing context.
 */
trait Flock_Lib {
  self : MovementLibrary.Subcomponent =>

  trait FlockLib {
    self: FieldCalculusSyntax with StandardSensors =>
    /**
      *
      * @param flockingField
      * @param attractionForce
      * @param alignmentForce
      * @param repulsionForce
      * @param separationDistance
      * @param otherVelocityEvaluation
     */
    case class FlockBehaviour(flockingField: Boolean = true,
                              attractionForce: Double = 1.0,
                              alignmentForce: Double = 1.0,
                              repulsionForce: Double = 1.0,
                              separationDistance: Double = Double.PositiveInfinity,
                              otherVelocityEvaluation : List[() => Velocity] = List.empty) {
      def run() : Velocity = {
        rep[Velocity](Velocity.Zero){
          v => {
            val activeNodes = getValuesFromActiveNode(flockingField)(currentPosition())
            val mainVector = List(
              separation(activeNodes, separationDistance) * repulsionForce,
              alignment(flockingField, v, activeNodes.size) * alignmentForce,
              cohesion(activeNodes) * attractionForce
            )
            val other = otherVelocityEvaluation.map(_())
            val resultingVector = concatenateVectors((other ::: mainVector):_*)

            (v + resultingVector).normalized
          }
        }
      }
    }

    def flock() : Velocity = FlockBehaviour().run()

    def antiflock() : Velocity = FlockBehaviour(attractionForce = -1, alignmentForce = - 1, repulsionForce = -1).run()

    def withSeparation(selector : => Boolean)(velocity: => Velocity)(separationDistance: Double) : Velocity = {
      mux[Velocity](selector) {
        val activeNodeInRange = inRange(getValuesFromActiveNode(selector)(currentPosition()), separationDistance)
        ((velocity / (activeNodeInRange.size + 1)) + separation(activeNodeInRange, separationDistance)).normalized
      } {
        Velocity.Zero
      }
    }

    def withSeparation(velocity: Velocity)(separationDistance: Double) : P = withSeparation(true)(velocity)(separationDistance)

    /* steer towards the average heading of local flockmates */
    private[Flock_Lib] def alignment(flockingSensor: => Boolean, velocity: Velocity, neighbourCount : Int): Velocity = {
      val alignmentVector: P = getValuesFromActiveNode(flockingSensor)(velocity).fold(Velocity.Zero)(_ + _)
      (alignmentVector / neighbourCount).normalized
    }
    /*steer to move toward the average position of local flockmates*/
    private[Flock_Lib] def cohesion(neighbors: Seq[Point3D]): Velocity = if(neighbors.isEmpty) {
      Velocity.Zero
    } else {
      val cohesionVector = neighbors.fold(Point3D.Zero)((a,b) => a + b)
      ((cohesionVector / neighbors.size) - currentPosition()).normalized
    }
    /* steer to avoid crowding local flockmates */
    private[Flock_Lib] def separation(activeNode : Seq[Point3D], separationDistance: Double): Velocity = {
      val closestNeighbours = inRange(activeNode, separationDistance).map(_ - currentPosition())
      val separationVector = closestNeighbours.fold[P](Point3D.Zero)((acc, b) => acc + b)
      (-separationVector).normalized
    }

    private[Flock_Lib] def getValuesFromActiveNode[E](flockingField : => Boolean)(value : => E) : Seq[E] = {
      foldhoodPlus[Seq[E]](Seq.empty[E])(_ ++ _){
        mux(nbr(flockingField)){
          Seq[E](nbr(value))
        } /* else */ {
          Seq.empty[E]
        }
      }
    }

    private def inRange(neighbours : Seq[Point3D], range : Double) : Seq[Point3D] = neighbours
      .filter(vector => currentPosition().distance(vector) < range)

    private def concatenateVectors(vectors : (Velocity)*) : Velocity = vectors.fold(Velocity.Zero)(_ + _)

  }

  object FlockLib {
    type Dependencies = AggregateProgram with StandardSensors
  }

  trait AdvancedFlock {
    self : FlockLib with FlockLib.Dependencies =>

    implicit class RichFlock(flock : FlockBehaviour) {
      def addBehaviour(v : => Velocity) = flock.copy(otherVelocityEvaluation = (() => v) :: flock.otherVelocityEvaluation )
      def withWind(v : Velocity) : FlockBehaviour = flock.addBehaviour(v.normalized)
      def withGoal(p : P, importance : Double = 1.0) : FlockBehaviour = flock.addBehaviour { (p - currentPosition()).normalized * importance }
      def withObstacleAvoid(obstacleSensor : => Boolean, force : Double = 1.0) : FlockBehaviour = {
        val unitVector = separation(getValuesFromActiveNode(obstacleSensor)(currentPosition()), Double.PositiveInfinity)
        flock.addBehaviour{ unitVector * force }
      }
    }
  }

}
