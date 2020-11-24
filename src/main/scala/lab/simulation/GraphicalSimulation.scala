package lab.simulation

import it.unibo.scafi.simulation.frontend.{Launcher, Settings}
import it.unibo.scafi.space.Point3D
object GraphicalSimulation extends Launcher {
  val scale = 100
  Settings.Sim_ProgramClass = "lab.demo.Main1"
  Settings.ShowConfigPanel = false
  Settings.Sim_NbrRadius = 0.2
  Settings.Sim_NumNodes = 40
  Settings.Movement_Activator = {
    case p : Point3D => (p.x / scale, p.y / scale)
    case other => (0.0, 0.0)
  }
  launch()
}
