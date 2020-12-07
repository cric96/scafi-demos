package lab.gui.patch

import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.SimulationExecutor

object AsyncExecutor extends SimulationExecutor {
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
