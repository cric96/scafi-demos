package lab.simulation

import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ScafiSimulationInitializer.RadiusSimulation
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.{MetaActionProducer, SimulationInfo}
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.ScafiProgramBuilder
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Grid
import it.unibo.scafi.simulation.s2.frontend.view.ViewSetting
import lab.demo._
import lab.gui.lab.gui.patch.PotentialFieldMovements

object GraphicalSimulation extends App {
  val distance = 40
  val rows = 10
  val cols = 10

  val programClass = classOf[Main1]

  val movementSimulation = SimulationInfo(program = programClass,
    metaActions = PotentialFieldMovements() :: Nil,
    exportEvaluations = List.empty
  )

  val textSimulation = SimulationInfo(program = programClass)

  ViewSetting.labelFontSize = 5
  ScafiProgramBuilder (
    Grid(distance, rows, cols),
    textSimulation,
    RadiusSimulation(radius = distance),
    neighbourRender = true,
  ).launch()

}
