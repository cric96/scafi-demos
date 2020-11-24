package lab.simulation

import it.unibo.scafi.simulation.frontend.{Launcher, Settings}
object GraphicalSimulation extends Launcher {
  Settings.Sim_ProgramClass = "lab.demo.Program1"
  Settings.ShowConfigPanel = false
  Settings.Sim_NbrRadius = 0.2
  Settings.Sim_NumNodes = 40
  launch()
}
