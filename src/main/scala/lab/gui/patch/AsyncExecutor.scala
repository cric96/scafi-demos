package lab.gui.patch

import it.unibo.scafi.simulation.s2.frontend.controller.logger.LogManager.{Channel, TreeLog}
import it.unibo.scafi.simulation.s2.frontend.controller.logger.LogManager
import it.unibo.scafi.simulation.s2.frontend.controller.logger.LogManager.TreeLog
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.{ScafiBridge, SimulationExecutor}
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ScafiWorldIncarnation._

object AsyncExecutor extends SimulationExecutor {
  import ScafiBridge._
  override protected def asyncLogicExecution(): Unit = {
    if (contract.simulation.isDefined) {
      val net = contract.simulation.get
      val result = net.exec(runningContext)
      exportProduced += result._1 -> result._2
      //meta actions associated to this simulation
      val metaActions = this.simulationInfo.get.metaActions
      metaActions.filter(x => x.valueParser(result._2.root()).isDefined).foreach(x => net.add(x(result._1, result._2)))
      net.process()
    }
  }
}
